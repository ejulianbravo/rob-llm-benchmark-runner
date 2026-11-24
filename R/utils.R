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
