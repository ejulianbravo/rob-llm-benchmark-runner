# Small helpers used across the runner.

sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256")
}

sha256_text <- function(x) {
  digest::digest(enc2utf8(paste(x, collapse = "\n")), algo = "sha256", serialize = FALSE)
}

b64_file <- function(path) {
  base64enc::base64encode(path)
}

now_utc <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

# Read an API key from the environment. Keys are never written to disk or logs.
api_key <- function(env_var) {
  key <- Sys.getenv(env_var)
  if (!nzchar(key)) stop("Environment variable ", env_var, " is not set.", call. = FALSE)
  key
}

# Return `x` if not NULL, otherwise `default`.
`%||%` <- function(x, default) if (is.null(x)) default else x

# Text of a PDF, one string per page, in reading order (columns read one after
# the other). Uses poppler's `pdftotext` (on a Mac: `brew install poppler`).
# pdftools::pdf_text() is not used for this because it keeps the page layout and
# mixes the lines of two-column pages.
pdf_pages_text <- function(path) {
  exe <- Sys.which("pdftotext")
  if (!nzchar(exe)) stop("pdftotext not found. Install poppler (Mac: brew install poppler).", call. = FALSE)
  out <- tempfile(fileext = ".txt")
  status <- system2(exe, c("-enc", "UTF-8", shQuote(path), shQuote(out)), stdout = FALSE, stderr = FALSE)
  if (status != 0) stop("pdftotext failed for ", path, call. = FALSE)
  txt <- paste(readLines(out, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  pages <- strsplit(txt, "\f", fixed = TRUE)[[1]]
  n <- pdftools::pdf_info(path)$pages
  length(pages) <- n
  pages[is.na(pages)] <- ""
  pages
}
