# Build one API request per study, model and run.
#
# Every request carries, in this order:
#   1. system instructions (same for every call)
#   2. the instrument's manuals as PDF files (same for every study of that instrument)
#   3. the instrument prompt: task and output format (same for every study of that instrument)
#   4. the study report as a PDF file
#   5. the assessment target for this study
# Parts 1-3 never change within an instrument, so they come first; providers can
# then reuse them from cache across calls.
#
# `model` is one entry of config/models.yaml. `params` from that entry are added
# to the request unchanged (top level for Anthropic and OpenAI; inside
# generationConfig for Google). Settings fixed in code are listed in
# FIXED_SETTINGS so the run log can record them.

FIXED_SETTINGS <- list(
  anthropic = list(endpoint = "https://api.anthropic.com/v1/messages",
                   anthropic_version = "2023-06-01"),
  openai    = list(endpoint = "https://api.openai.com/v1/responses", store = FALSE),
  google    = list(endpoint = "https://generativelanguage.googleapis.com/v1beta/models/{model}:generateContent")
)

# `key` may be NULL only when the request will not be sent (dry run); a
# placeholder is used so no real key is held in the returned object.
build_request <- function(model, system, manuals, instrument_prompt, pdf_path, target,
                          key = api_key(model$api_key_env)) {
  stopifnot(is.character(manuals), length(manuals) >= 1, all(file.exists(manuals)), file.exists(pdf_path))
  if (is.null(key)) key <- "DRY-RUN-NO-KEY"
  manuals <- unname(manuals)  # paths; names would turn the JSON array into an object
  builder <- switch(model$provider,
    anthropic = build_request_anthropic,
    openai    = build_request_openai,
    google    = build_request_google,
    stop("Unknown provider: ", model$provider, call. = FALSE)
  )
  builder(model, system, manuals, instrument_prompt, pdf_path, target, key)
}

build_request_anthropic <- function(model, system, manuals, instrument_prompt, pdf_path, target, key) {
  manual_blocks <- lapply(manuals, function(path) {
    list(type = "document",
         source = list(type = "base64", media_type = "application/pdf", data = b64_file(path)))
  })
  prompt_block <- list(type = "text", text = instrument_prompt)
  if (isTRUE(model$cache_manuals)) {
    # One cache breakpoint after the instrument prompt: system, manuals and
    # prompt are identical for every study of an instrument.
    prompt_block$cache_control <- list(type = "ephemeral")
  }
  content <- c(
    manual_blocks,
    list(
      prompt_block,
      list(type = "document",
           source = list(type = "base64", media_type = "application/pdf",
                         data = b64_file(pdf_path))),
      list(type = "text", text = target)
    )
  )
  body <- c(
    list(model = model$model_id,
         max_tokens = model$max_output_tokens,
         system = system,
         messages = list(list(role = "user", content = content))),
    model$params
  )
  httr2::request(FIXED_SETTINGS$anthropic$endpoint) |>
    httr2::req_headers(
      `x-api-key` = key,
      `anthropic-version` = FIXED_SETTINGS$anthropic$anthropic_version,
      .redact = "x-api-key"
    ) |>
    httr2::req_body_json(body, auto_unbox = TRUE)
}

build_request_openai <- function(model, system, manuals, instrument_prompt, pdf_path, target, key) {
  manual_parts <- lapply(manuals, function(path) {
    list(type = "input_file", filename = basename(path),
         file_data = paste0("data:application/pdf;base64,", b64_file(path)))
  })
  content <- c(
    manual_parts,
    list(
      list(type = "input_text", text = instrument_prompt),
      list(type = "input_file",
           filename = basename(pdf_path),
           file_data = paste0("data:application/pdf;base64,", b64_file(pdf_path))),
      list(type = "input_text", text = target)
    )
  )
  body <- c(
    list(model = model$model_id,
         instructions = system,
         input = list(list(role = "user", content = content)),
         max_output_tokens = model$max_output_tokens,
         store = FIXED_SETTINGS$openai$store),
    model$params
  )
  httr2::request(FIXED_SETTINGS$openai$endpoint) |>
    httr2::req_auth_bearer_token(key) |>
    httr2::req_body_json(body, auto_unbox = TRUE)
}

build_request_google <- function(model, system, manuals, instrument_prompt, pdf_path, target, key) {
  manual_parts <- lapply(manuals, function(path) {
    list(inlineData = list(mimeType = "application/pdf", data = b64_file(path)))
  })
  parts <- c(
    manual_parts,
    list(
      list(text = instrument_prompt),
      list(inlineData = list(mimeType = "application/pdf", data = b64_file(pdf_path))),
      list(text = target)
    )
  )
  generation_config <- c(list(maxOutputTokens = model$max_output_tokens), model$params)
  body <- list(
    systemInstruction = list(parts = list(list(text = system))),
    contents = list(list(role = "user", parts = parts)),
    generationConfig = generation_config
  )
  url <- sub("{model}", model$model_id, FIXED_SETTINGS$google$endpoint, fixed = TRUE)
  httr2::request(url) |>
    httr2::req_headers(`x-goog-api-key` = key, .redact = "x-goog-api-key") |>
    httr2::req_body_json(body, auto_unbox = TRUE)
}

# Size of the JSON body in MB, to compare with the provider's request limit
# (model$max_request_mb in the config) before sending.
request_size_mb <- function(req) {
  length(charToRaw(jsonlite::toJSON(req$body$data, auto_unbox = TRUE))) / 1024^2
}
