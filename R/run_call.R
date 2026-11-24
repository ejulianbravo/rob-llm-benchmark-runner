# Run one call and record it.
#
# What counts as the run's output:
#   - Only a successful HTTP response (status 2xx) is a model output. Its body is
#     written to outputs/raw/ unchanged, before anything reads it, and the run
#     number is then used up. It is never overwritten, repaired or re-asked,
#     even if the model's answer is unusable.
#   - An HTTP error after retries (e.g. rate limit, overloaded server) is not a
#     model output. Its body is kept in outputs/errors/ with a timestamp, the
#     call is logged, and the same run number can be tried again later.
#   - A network error or timeout has no body; it is logged and the run number
#     can be tried again. A timed-out request may still be billed by the
#     provider; check the provider's usage page if this happens.
# Only transient HTTP failures (429, 5xx) are retried automatically, and only
# before any model output exists.
#
# Calls must run one at a time. A lock folder per call guards against two
# processes making the same call at once.

raw_path <- function(out_dir, instrument, study_id, model, run_number) {
  model_safe <- gsub("[^A-Za-z0-9._-]", "-", model$model_id)
  file.path(out_dir, "raw", instrument,
            sprintf("%s__%s__%s__run%d.json", study_id, model$label, model_safe, run_number))
}

run_call <- function(study_id, instrument, model, run_number, prompt_version,
                     system, manuals, instrument_prompt, pdf_path, target,
                     out_dir = "outputs", perform = TRUE) {
  raw_file <- raw_path(out_dir, instrument, study_id, model, run_number)
  call_id <- sub("\\.json$", "", basename(raw_file))
  if (file.exists(raw_file)) {
    stop("Raw output already exists, not overwriting: ", raw_file, call. = FALSE)
  }

  settings <- c(list(max_output_tokens = model$max_output_tokens,
                     cache_manuals = isTRUE(model$cache_manuals),
                     timeout_s = model$timeout_s %||% 1800),
                FIXED_SETTINGS[[model$provider]], model$params)
  manual_names <- names(manuals) %||% paste0("manual", seq_along(manuals))
  row <- list(
    call_id = call_id, study_id = study_id, instrument = instrument,
    provider = model$provider, model_label = model$label, model_requested = model$model_id,
    run_number = run_number, prompt_version = prompt_version,
    settings = as.character(jsonlite::toJSON(settings, auto_unbox = TRUE)),
    pdf_sha256 = sha256_file(pdf_path),
    manuals_sha256 = paste0(manual_names, "=", vapply(manuals, sha256_file, ""), collapse = ";"),
    prompt_sha256 = sha256_text(c(system, instrument_prompt)),
    target_sha256 = sha256_text(target),
    request_mb = NA_real_,
    started_at = NA_character_, ended_at = NA_character_, duration_s = NA_real_,
    status = NA_character_, http_status = NA_integer_,
    raw_file = NA_character_, error_file = NA_character_, error = NA_character_
  )

  key <- if (perform) api_key(model$api_key_env) else NULL
  req <- build_request(model, system, manuals, instrument_prompt, pdf_path, target, key = key) |>
    httr2::req_timeout(model$timeout_s %||% 1800) |>
    httr2::req_retry(
      max_tries = 3,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504, 529)
    ) |>
    httr2::req_error(is_error = function(resp) FALSE)
  row$request_mb <- round(request_size_mb(req), 2)

  if (!perform) return(list(row = c(row, empty_meta()), request = req))

  if (!is.null(model$max_request_mb) && row$request_mb > model$max_request_mb) {
    row$status <- "too_large"
    row$error <- sprintf("Request is %.1f MB; limit set for this model is %s MB",
                         row$request_mb, model$max_request_mb)
    return(c(row, empty_meta()))
  }

  lock <- file.path(out_dir, "locks", call_id)
  dir.create(dirname(lock), recursive = TRUE, showWarnings = FALSE)
  if (!dir.create(lock, showWarnings = FALSE)) {
    stop("Call already in progress (lock exists): ", lock, call. = FALSE)
  }
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)

  row$started_at <- now_utc()
  t0 <- Sys.time()
  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
  row$ended_at <- now_utc()
  row$duration_s <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)

  if (inherits(resp, "error")) {
    msg <- conditionMessage(resp)
    row$status <- if (grepl("timeout|timed out", msg, ignore.case = TRUE)) "timeout" else "network_error"
    row$error <- substr(msg, 1, 500)
    return(c(row, empty_meta()))
  }

  row$http_status <- httr2::resp_status(resp)

  if (row$http_status >= 300) {
    err_dir <- file.path(out_dir, "errors", instrument)
    dir.create(err_dir, recursive = TRUE, showWarnings = FALSE)
    err_file <- file.path(err_dir, sprintf("%s__%s.json", call_id,
                                           format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC")))
    writeBin(httr2::resp_body_raw(resp), err_file)
    row$error_file <- err_file
    row$status <- "http_error"
    body <- tryCatch(jsonlite::fromJSON(err_file, simplifyVector = FALSE), error = function(e) NULL)
    row$error <- substr(as.character(jsonlite::toJSON(body$error %||% body, auto_unbox = TRUE)), 1, 500)
    return(c(row, empty_meta()))
  }

  dir.create(dirname(raw_file), recursive = TRUE, showWarnings = FALSE)
  writeBin(httr2::resp_body_raw(resp), raw_file)
  row$raw_file <- raw_file

  body <- tryCatch(jsonlite::fromJSON(raw_file, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(body)) {
    row$status <- "invalid_json"
    row$error <- "Response body is not valid JSON"
    return(c(row, empty_meta()))
  }
  row$status <- "ok"
  c(row, response_meta(model$provider, body))
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
