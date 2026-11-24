# Run one call and record it.
#
# - The raw response body is written to disk unchanged, before anything reads it.
# - An existing raw file is never overwritten: a repeated call needs a new run number.
# - Only transient failures (rate limits, server errors) are retried, before any
#   model output exists. A model's answer is never re-asked or repaired.
# - The function always returns one log row, also when the call fails.

run_call <- function(study_id, instrument, model, run_number, prompt_version,
                     system, manuals, instrument_prompt, pdf_path, target,
                     out_dir = "outputs", perform = TRUE) {
  label <- model$label
  raw_dir <- file.path(out_dir, "raw", instrument)
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  raw_file <- file.path(raw_dir, sprintf("%s__%s__run%d.json", study_id, label, run_number))
  if (file.exists(raw_file)) {
    stop("Raw output already exists, not overwriting: ", raw_file, call. = FALSE)
  }

  row <- list(
    call_id = sprintf("%s__%s__run%d", study_id, label, run_number),
    study_id = study_id, instrument = instrument,
    provider = model$provider, model_label = label, model_requested = model$model_id,
    run_number = run_number, prompt_version = prompt_version,
    settings = jsonlite::toJSON(c(list(max_output_tokens = model$max_output_tokens), model$params),
                                auto_unbox = TRUE),
    pdf_sha256 = sha256_file(pdf_path),
    manuals_sha256 = sha256_text(manuals),
    prompt_sha256 = sha256_text(c(system, instrument_prompt)),
    target_sha256 = sha256_text(target),
    started_at = NA_character_, ended_at = NA_character_, duration_s = NA_real_,
    http_status = NA_integer_, raw_file = NA_character_, error = NA_character_
  )

  req <- build_request(model, system, manuals, instrument_prompt, pdf_path, target) |>
    httr2::req_timeout(model$timeout_s %||% 900) |>
    httr2::req_retry(
      max_tries = 3,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504, 529)
    ) |>
    httr2::req_error(is_error = function(resp) FALSE)

  if (!perform) return(list(row = row, request = req))

  row$started_at <- now_utc()
  t0 <- Sys.time()
  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
  row$ended_at <- now_utc()
  row$duration_s <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)

  if (inherits(resp, "error")) {
    row$error <- conditionMessage(resp)
    return(c(row, empty_meta()))
  }

  writeBin(httr2::resp_body_raw(resp), raw_file)
  row$raw_file <- raw_file
  row$http_status <- httr2::resp_status(resp)

  body <- tryCatch(jsonlite::fromJSON(raw_file, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(body)) {
    row$error <- "Response body is not valid JSON"
    return(c(row, empty_meta()))
  }
  if (row$http_status >= 400) {
    row$error <- substr(jsonlite::toJSON(body$error %||% body, auto_unbox = TRUE), 1, 500)
    return(c(row, empty_meta()))
  }
  c(row, response_meta(model$provider, body))
}

empty_meta <- function() {
  list(model_returned = NA_character_, finish_reason = NA_character_,
       input_tokens = NA_real_, output_tokens = NA_real_, reasoning_tokens = NA_real_,
       cache_read_tokens = NA_real_, cache_write_tokens = NA_real_)
}

# Append one row to the run log (CSV), writing the header on first use.
append_run_log <- function(row, log_file = file.path("outputs", "run_log.csv")) {
  dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
  df <- as.data.frame(lapply(row, function(x) if (length(x) == 0) NA else as.character(x)),
                      stringsAsFactors = FALSE)
  write_header <- !file.exists(log_file)
  utils::write.table(df, log_file, sep = ",", row.names = FALSE, col.names = write_header,
                     append = !write_header, qmethod = "double", fileEncoding = "UTF-8")
  invisible(log_file)
}
