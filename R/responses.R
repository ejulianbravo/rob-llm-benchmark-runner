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
      parts <- if (length(body$candidates)) body$candidates[[1]]$content$parts else NULL
      parts <- Filter(function(p) !is.null(p$text) && !isTRUE(p$thought), parts %||% list())
      if (length(parts) == 0) NA_character_ else paste(vapply(parts, `[[`, "", "text"), collapse = "")
    },
    stop("Unknown provider: ", provider, call. = FALSE)
  )
}

# Token columns have ONE meaning for every provider:
#   input_tokens       all input tokens, including tokens read from or written to cache
#   cache_read_tokens  input tokens served from cache (subset of input_tokens)
#   cache_write_tokens input tokens written to cache (subset of input_tokens; Anthropic only)
#   output_tokens      all output tokens, including reasoning/thinking tokens
#   reasoning_tokens   reasoning/thinking tokens (subset of output_tokens), where reported
# Providers report these differently; the conversions are below. The provider's
# own usage object is also kept unchanged in `usage_raw`.
response_meta <- function(provider, body) {
  num <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)
  chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)
  sum0 <- function(...) { v <- c(...); if (all(is.na(v))) NA_real_ else sum(v, na.rm = TRUE) }
  meta <- switch(provider,
    anthropic = {
      # Anthropic's input_tokens EXCLUDES cache reads and writes; output_tokens
      # INCLUDES thinking, reported separately in output_tokens_details.
      u <- body$usage
      list(model_returned = chr(body$model),
           finish_reason = chr(body$stop_reason),
           input_tokens = sum0(num(u$input_tokens), num(u$cache_read_input_tokens),
                               num(u$cache_creation_input_tokens)),
           cache_read_tokens = num(u$cache_read_input_tokens),
           cache_write_tokens = num(u$cache_creation_input_tokens),
           output_tokens = num(u$output_tokens),
           reasoning_tokens = num(u$output_tokens_details$thinking_tokens))
    },
    openai = {
      # OpenAI's input_tokens INCLUDES cached tokens; output_tokens INCLUDES reasoning.
      u <- body$usage
      list(model_returned = chr(body$model),
           finish_reason = chr(body$incomplete_details$reason %||% body$status),
           input_tokens = num(u$input_tokens),
           cache_read_tokens = num(u$input_tokens_details$cached_tokens),
           cache_write_tokens = NA_real_,
           output_tokens = num(u$output_tokens),
           reasoning_tokens = num(u$output_tokens_details$reasoning_tokens))
    },
    google = {
      # Google's promptTokenCount INCLUDES cached tokens; candidatesTokenCount
      # EXCLUDES thinking (thoughtsTokenCount), so they are added.
      u <- body$usageMetadata
      cand <- if (length(body$candidates)) body$candidates[[1]] else NULL
      finish <- cand$finishReason %||%
        (if (!is.null(body$promptFeedback$blockReason))
           paste0("PROMPT_BLOCKED:", body$promptFeedback$blockReason) else NULL)
      list(model_returned = chr(body$modelVersion),
           finish_reason = chr(finish),
           input_tokens = num(u$promptTokenCount),
           cache_read_tokens = num(u$cachedContentTokenCount),
           cache_write_tokens = NA_real_,
           output_tokens = sum0(num(u$candidatesTokenCount), num(u$thoughtsTokenCount)),
           reasoning_tokens = num(u$thoughtsTokenCount))
    },
    stop("Unknown provider: ", provider, call. = FALSE)
  )
  usage <- switch(provider, anthropic = body$usage, openai = body$usage, google = body$usageMetadata)
  meta$usage_raw <- if (is.null(usage)) NA_character_ else
    as.character(jsonlite::toJSON(usage, auto_unbox = TRUE))
  meta
}

empty_meta <- function() {
  list(model_returned = NA_character_, finish_reason = NA_character_,
       input_tokens = NA_real_, cache_read_tokens = NA_real_, cache_write_tokens = NA_real_,
       output_tokens = NA_real_, reasoning_tokens = NA_real_, usage_raw = NA_character_)
}
