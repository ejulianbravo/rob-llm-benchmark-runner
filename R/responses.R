# Read what we need from a raw response body (already parsed from JSON).
# These functions only read; they never change or repair the response.
# Anything missing comes back as NA.

response_text <- function(provider, body) {
  switch(provider,
    anthropic = {
      blocks <- Filter(function(b) identical(b$type, "text"), body$content %||% list())
      if (length(blocks) == 0) NA_character_ else paste(vapply(blocks, `[[`, "", "text"), collapse = "")
    },
    openai = {
      msgs <- Filter(function(o) identical(o$type, "message"), body$output %||% list())
      parts <- unlist(lapply(msgs, function(m) {
        Filter(function(p) identical(p$type, "output_text"), m$content %||% list())
      }), recursive = FALSE)
      if (length(parts) == 0) NA_character_ else paste(vapply(parts, `[[`, "", "text"), collapse = "")
    },
    google = {
      parts <- body$candidates[[1]]$content$parts %||% list()
      parts <- Filter(function(p) !is.null(p$text) && !isTRUE(p$thought), parts)
      if (length(parts) == 0) NA_character_ else paste(vapply(parts, `[[`, "", "text"), collapse = "")
    },
    stop("Unknown provider: ", provider, call. = FALSE)
  )
}

response_meta <- function(provider, body) {
  num <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
  chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)
  switch(provider,
    anthropic = {
      u <- body$usage
      list(model_returned = chr(body$model),
           finish_reason = chr(body$stop_reason),
           input_tokens = num(u$input_tokens),
           output_tokens = num(u$output_tokens),
           reasoning_tokens = NA_real_,  # included in output_tokens; not reported separately
           cache_read_tokens = num(u$cache_read_input_tokens),
           cache_write_tokens = num(u$cache_creation_input_tokens))
    },
    openai = {
      u <- body$usage
      list(model_returned = chr(body$model),
           finish_reason = chr(body$incomplete_details$reason %||% body$status),
           input_tokens = num(u$input_tokens),
           output_tokens = num(u$output_tokens),
           reasoning_tokens = num(u$output_tokens_details$reasoning_tokens),
           cache_read_tokens = num(u$input_tokens_details$cached_tokens),
           cache_write_tokens = NA_real_)
    },
    google = {
      u <- body$usageMetadata
      list(model_returned = chr(body$modelVersion),
           finish_reason = chr(body$candidates[[1]]$finishReason),
           input_tokens = num(u$promptTokenCount),
           output_tokens = num(u$candidatesTokenCount),
           reasoning_tokens = num(u$thoughtsTokenCount),
           cache_read_tokens = num(u$cachedContentTokenCount),
           cache_write_tokens = NA_real_)
    },
    stop("Unknown provider: ", provider, call. = FALSE)
  )
}
