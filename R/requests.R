# Build one API request per study, model and run.
#
# Every request carries, in this order:
#   1. system instructions (same for every call)
#   2. the instrument's manuals as text (same for every study of that instrument)
#   3. the instrument prompt: task and output format (same for every study of that instrument)
#   4. the study report as a PDF file
#   5. the assessment target for this study
# Parts 1-3 never change within an instrument, so they come first; providers can
# then reuse them from cache across calls.
#
# `model` is one entry of config/models.yaml. `params` from that entry are added
# to the request unchanged (top level for Anthropic and OpenAI; inside
# generationConfig for Google), so settings such as temperature or reasoning effort
# are set per model in the config, not in code.

build_request <- function(model, system, manuals, instrument_prompt, pdf_path, target) {
  stopifnot(is.character(manuals), length(manuals) >= 1, file.exists(pdf_path))
  builder <- switch(model$provider,
    anthropic = build_request_anthropic,
    openai    = build_request_openai,
    google    = build_request_google,
    stop("Unknown provider: ", model$provider, call. = FALSE)
  )
  builder(model, system, manuals, instrument_prompt, pdf_path, target)
}

build_request_anthropic <- function(model, system, manuals, instrument_prompt, pdf_path, target) {
  manual_blocks <- lapply(manuals, function(txt) {
    list(type = "document",
         source = list(type = "text", media_type = "text/plain", data = txt))
  })
  if (isTRUE(model$cache_manuals)) {
    # Cache everything up to and including the last manual block.
    manual_blocks[[length(manual_blocks)]]$cache_control <- list(type = "ephemeral")
  }
  content <- c(
    manual_blocks,
    list(
      list(type = "text", text = instrument_prompt),
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
  httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(
      `x-api-key` = api_key(model$api_key_env),
      `anthropic-version` = "2023-06-01",
      .redact = "x-api-key"
    ) |>
    httr2::req_body_json(body, auto_unbox = TRUE)
}

build_request_openai <- function(model, system, manuals, instrument_prompt, pdf_path, target) {
  manual_parts <- lapply(manuals, function(txt) list(type = "input_text", text = txt))
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
         store = FALSE),
    model$params
  )
  httr2::request("https://api.openai.com/v1/responses") |>
    httr2::req_auth_bearer_token(api_key(model$api_key_env)) |>
    httr2::req_body_json(body, auto_unbox = TRUE)
}

build_request_google <- function(model, system, manuals, instrument_prompt, pdf_path, target) {
  manual_parts <- lapply(manuals, function(txt) list(text = txt))
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
  url <- sprintf("https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent",
                 model$model_id)
  httr2::request(url) |>
    httr2::req_headers(`x-goog-api-key` = api_key(model$api_key_env), .redact = "x-goog-api-key") |>
    httr2::req_body_json(body, auto_unbox = TRUE)
}
