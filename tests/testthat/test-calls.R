# Offline tests: requests are built and responses are mocked; nothing is sent.
# The mocked response bodies are constructed by hand to follow each provider's
# documented response shape. They are not real model outputs.

root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (f in list.files(file.path(root, "R"), full.names = TRUE)) source(f)

models <- yaml::read_yaml(file.path(root, "config", "models.yaml"))$models
names(models) <- vapply(models, `[[`, "", "label")

make_pdf <- function() {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path); plot.new(); text(0.5, 0.5, "Test report"); grDevices::dev.off()
  path
}

withr::local_envvar(ANTHROPIC_API_KEY = "test-a", OPENAI_API_KEY = "test-o", GEMINI_API_KEY = "test-g")

args <- list(system = "SYSTEM", manuals = c("MANUAL ONE", "MANUAL TWO"),
             instrument_prompt = "PROMPT", target = "TARGET")

body_of <- function(req) req$body$data

test_that("Anthropic request has manuals first, cached, then prompt, PDF, target", {
  pdf <- make_pdf()
  b <- body_of(do.call(build_request, c(list(models$claude, pdf_path = pdf), args)))
  types <- vapply(b$messages[[1]]$content, `[[`, "", "type")
  expect_equal(types, c("document", "document", "text", "document", "text"))
  expect_equal(b$messages[[1]]$content[[2]]$cache_control$type, "ephemeral")
  expect_null(b$messages[[1]]$content[[1]]$cache_control)
  expect_equal(b$messages[[1]]$content[[4]]$source$media_type, "application/pdf")
  expect_equal(b$system, "SYSTEM")
  expect_null(b$temperature)
})

test_that("OpenAI request uses Responses API, store = FALSE, PDF as input_file", {
  pdf <- make_pdf()
  req <- do.call(build_request, c(list(models$gpt, pdf_path = pdf), args))
  b <- body_of(req)
  expect_match(req$url, "/v1/responses$")
  expect_false(b$store)
  types <- vapply(b$input[[1]]$content, `[[`, "", "type")
  expect_equal(types, c("input_text", "input_text", "input_text", "input_file", "input_text"))
  expect_match(b$input[[1]]$content[[4]]$file_data, "^data:application/pdf;base64,")
})

test_that("Google request puts params in generationConfig and PDF inline", {
  pdf <- make_pdf()
  m <- models$gemini; m$params <- list(temperature = 0)
  b <- body_of(do.call(build_request, c(list(m, pdf_path = pdf), args)))
  expect_equal(b$generationConfig$temperature, 0)
  expect_equal(b$contents[[1]]$parts[[4]]$inlineData$mimeType, "application/pdf")
})

test_that("API keys are redacted from printed requests", {
  pdf <- make_pdf()
  req <- do.call(build_request, c(list(models$claude, pdf_path = pdf), args))
  expect_false(grepl("test-a", paste(capture.output(print(req)), collapse = "\n")))
})

mock_json <- function(body, status = 200) {
  httr2::response(status_code = status, headers = list(`Content-Type` = "application/json"),
                  body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE)))
}

fake <- list(
  claude = list(model = "claude-fable-5-2", stop_reason = "end_turn",
                content = list(list(type = "thinking", thinking = "..."),
                               list(type = "text", text = "{\"ok\":true}")),
                usage = list(input_tokens = 1000, output_tokens = 200,
                             cache_read_input_tokens = 900, cache_creation_input_tokens = 0)),
  gpt = list(model = "gpt-6.2-astra", status = "completed",
             output = list(list(type = "reasoning"),
                           list(type = "message",
                                content = list(list(type = "output_text", text = "{\"ok\":true}")))),
             usage = list(input_tokens = 1000, output_tokens = 300,
                          output_tokens_details = list(reasoning_tokens = 100),
                          input_tokens_details = list(cached_tokens = 800))),
  gemini = list(modelVersion = "gemini-4-argon",
                candidates = list(list(finishReason = "STOP",
                                       content = list(parts = list(list(text = "...", thought = TRUE),
                                                                   list(text = "{\"ok\":true}"))))),
                usageMetadata = list(promptTokenCount = 1000, candidatesTokenCount = 250,
                                     thoughtsTokenCount = 50, cachedContentTokenCount = 700))
)

for (lab in names(fake)) {
  test_that(paste("run_call stores raw output and logs metadata:", lab), {
    pdf <- make_pdf(); out <- withr::local_tempdir()
    httr2::local_mocked_responses(function(req) mock_json(fake[[lab]]))
    row <- do.call(run_call, c(list(study_id = "TEST_001", instrument = "rob2", model = models[[lab]],
                                    run_number = 1, prompt_version = "test", pdf_path = pdf,
                                    out_dir = out), args))
    expect_true(file.exists(row$raw_file))
    expect_equal(row$http_status, 200L)
    expect_true(is.na(row$error))
    expect_equal(row$input_tokens, 1000)
    body <- jsonlite::fromJSON(row$raw_file, simplifyVector = FALSE)
    expect_equal(response_text(models[[lab]]$provider, body), "{\"ok\":true}")
    # Raw file is byte-identical to what the API returned
    expect_identical(readBin(row$raw_file, "raw", 1e6),
                     charToRaw(jsonlite::toJSON(fake[[lab]], auto_unbox = TRUE)))
    # A second call with the same run number is refused
    expect_error(do.call(run_call, c(list(study_id = "TEST_001", instrument = "rob2",
                                          model = models[[lab]], run_number = 1,
                                          prompt_version = "test", pdf_path = pdf,
                                          out_dir = out), args)), "not overwriting")
  })
}

test_that("An API error is logged, stored and does not stop the run", {
  pdf <- make_pdf(); out <- withr::local_tempdir()
  httr2::local_mocked_responses(function(req) mock_json(list(error = list(message = "bad request")), 400))
  row <- do.call(run_call, c(list(study_id = "TEST_002", instrument = "nos", model = models$claude,
                                  run_number = 1, prompt_version = "test", pdf_path = pdf,
                                  out_dir = out), args))
  expect_equal(row$http_status, 400L)
  expect_match(row$error, "bad request")
  expect_true(file.exists(row$raw_file))
  log <- append_run_log(row, file.path(out, "run_log.csv"))
  expect_equal(nrow(utils::read.csv(log)), 1)
})
