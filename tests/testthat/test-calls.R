# Offline tests: requests are built and responses are mocked; nothing is sent.
# The mocked response bodies are constructed by hand to follow each provider's
# documented response shape. They are not real model outputs.

withr::local_envvar(ANTHROPIC_API_KEY = "test-a", OPENAI_API_KEY = "test-o", GEMINI_API_KEY = "test-g")

body_of <- function(req) req$body$data
build <- function(m, pdf) do.call(build_request, c(list(m, pdf_path = pdf), call_args))

test_that("Anthropic request: manual PDFs, then cached prompt, study PDF, target", {
  b <- body_of(build(models$claude, make_pdf()))
  content <- b$messages[[1]]$content
  expect_equal(vapply(content, `[[`, "", "type"), c("document", "document", "text", "document", "text"))
  expect_equal(content[[1]]$source$media_type, "application/pdf")
  expect_equal(content[[3]]$cache_control$type, "ephemeral")
  expect_null(content[[2]]$cache_control)
  expect_equal(content[[4]]$source$media_type, "application/pdf")
  expect_null(b$temperature)
})

test_that("OpenAI request: Responses API, store = FALSE, PDF as input_file", {
  req <- build(models$gpt, make_pdf())
  b <- body_of(req)
  expect_match(req$url, "/v1/responses$")
  expect_false(b$store)
  types <- vapply(b$input[[1]]$content, `[[`, "", "type")
  expect_equal(types, c("input_file", "input_file", "input_text", "input_file", "input_text"))
  expect_match(b$input[[1]]$content[[1]]$file_data, "^data:application/pdf;base64,")
})

test_that("Google request: params go in generationConfig, PDF inline", {
  m <- models$gemini; m$params <- list(temperature = 0)
  b <- body_of(build(m, make_pdf()))
  expect_equal(b$generationConfig$temperature, 0)
  expect_equal(b$contents[[1]]$parts[[1]]$inlineData$mimeType, "application/pdf")
  expect_equal(b$contents[[1]]$parts[[4]]$inlineData$mimeType, "application/pdf")
})

test_that("Keys are redacted when printed; dry run needs no key", {
  req <- build(models$claude, make_pdf())
  expect_false(grepl("test-a", paste(capture.output(print(req)), collapse = "\n")))
  withr::with_envvar(c(ANTHROPIC_API_KEY = ""), {
    dry <- do.call(run_call, c(list(study_id = "T", instrument = "rob2", model = models$claude,
                                    run_number = 1, prompt_version = "t", pdf_path = make_pdf(),
                                    out_dir = withr::local_tempdir(), perform = FALSE), call_args))
    expect_true(dry$row$request_mb > 0)
  })
})

fake <- list(
  claude = list(model = "claude-fable-5-2", stop_reason = "end_turn",
                content = list(list(type = "thinking", thinking = "..."),
                               list(type = "text", text = "{\"ok\":true}")),
                usage = list(input_tokens = 100, output_tokens = 200,
                             cache_read_input_tokens = 900, cache_creation_input_tokens = 0,
                             output_tokens_details = list(thinking_tokens = 150))),
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
expected_tokens <- list(claude = c(1000, 200), gpt = c(1000, 300), gemini = c(1000, 300))

one_call <- function(m, out, study = "TEST_001", run = 1) {
  do.call(run_call, c(list(study_id = study, instrument = "rob2", model = m, run_number = run,
                           prompt_version = "test", pdf_path = make_pdf(), out_dir = out), call_args))
}

for (lab in names(fake)) {
  test_that(paste("Successful call stores raw output; tokens on one definition:", lab), {
    out <- withr::local_tempdir()
    httr2::local_mocked_responses(function(req) mock_json(fake[[lab]]))
    row <- one_call(models[[lab]], out)
    expect_equal(row$status, "ok")
    expect_true(file.exists(row$raw_file))
    expect_match(row$raw_file, models[[lab]]$model_id, fixed = TRUE)
    expect_equal(c(row$input_tokens, row$output_tokens), expected_tokens[[lab]])
    expect_equal(row$reasoning_tokens, c(claude = 150, gpt = 100, gemini = 50)[[lab]])
    expect_identical(readBin(row$raw_file, "raw", 1e6),
                     charToRaw(as.character(jsonlite::toJSON(fake[[lab]], auto_unbox = TRUE))))
    expect_equal(response_text(models[[lab]]$provider, jsonlite::fromJSON(row$raw_file, simplifyVector = FALSE)),
                 "{\"ok\":true}")
    expect_match(row$manuals_sha256, "^m1=.*;m2=")
    expect_match(row$settings, "endpoint")
    expect_error(one_call(models[[lab]], out), "not overwriting")
  })
}

test_that("HTTP error is kept in errors/, does not use up the run number", {
  out <- withr::local_tempdir()
  httr2::local_mocked_responses(function(req) mock_json(list(error = list(message = "bad request")), 400))
  row <- one_call(models$claude, out)
  expect_equal(row$status, "http_error")
  expect_true(is.na(row$raw_file))
  expect_true(file.exists(row$error_file))
  expect_match(row$error, "bad request")
  httr2::local_mocked_responses(function(req) mock_json(fake$claude))
  expect_equal(one_call(models$claude, out)$status, "ok")   # run 1 can still be made
})

test_that("Only transient HTTP errors are set to be retried", {
  # httr2's mocking skips the retry loop, so the retry policy is checked directly.
  res <- do.call(run_call, c(list(study_id = "T", instrument = "rob2", model = models$claude,
                                  run_number = 1, prompt_version = "t", pdf_path = make_pdf(),
                                  out_dir = withr::local_tempdir(), perform = FALSE), call_args))
  is_transient <- res$request$policies$retry_is_transient
  expect_equal(res$request$policies$retry_max_tries, 3)
  expect_true(is_transient(httr2::response(529)))
  expect_true(is_transient(httr2::response(429)))
  expect_false(is_transient(httr2::response(400)))
  expect_false(is_transient(httr2::response(200)))
})

test_that("Network error and timeout are logged without a raw file", {
  out <- withr::local_tempdir()
  httr2::local_mocked_responses(function(req) stop("Timeout was reached: operation timed out"))
  row <- one_call(models$gpt, out)
  expect_equal(row$status, "timeout")
  expect_true(is.na(row$raw_file))
  httr2::local_mocked_responses(function(req) stop("Could not resolve host"))
  expect_equal(one_call(models$gpt, out)$status, "network_error")
})

test_that("Google prompt block is recorded as the finish reason", {
  out <- withr::local_tempdir()
  blocked <- list(promptFeedback = list(blockReason = "SAFETY"),
                  usageMetadata = list(promptTokenCount = 1000), modelVersion = "gemini-4-argon")
  httr2::local_mocked_responses(function(req) mock_json(blocked))
  row <- one_call(models$gemini, out)
  expect_equal(row$finish_reason, "PROMPT_BLOCKED:SAFETY")
  expect_true(file.exists(row$raw_file))
})

test_that("Oversized requests are refused before sending", {
  out <- withr::local_tempdir()
  m <- models$claude; m$max_request_mb <- 0
  expect_equal(one_call(m, out)$status, "too_large")
})

test_that("Run log appends rows", {
  out <- withr::local_tempdir()
  httr2::local_mocked_responses(function(req) mock_json(fake$claude))
  log <- file.path(out, "run_log.csv")
  append_run_log(one_call(models$claude, out, "A"), log)
  append_run_log(one_call(models$claude, out, "B"), log)
  expect_equal(nrow(utils::read.csv(log)), 2)
})
