# Load the inputs for a call and check them against their recorded hashes.
#
# Paths outside the repo (frozen dataset, manuals folder) are passed in or set
# in config/paths.yaml, never committed.

INSTRUMENTS <- c("rob2", "robinsi", "robinse", "nos", "probast")

# The frozen studies table for one instrument (from the dataset repo).
load_studies <- function(dataset_dir, instrument) {
  stopifnot(instrument %in% INSTRUMENTS)
  path <- file.path(dataset_dir, "data", instrument, sprintf("studies_%s.csv", instrument))
  utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "", encoding = "UTF-8")
}

# Path to a study's PDF, checked against the frozen SHA-256.
study_pdf <- function(dataset_dir, instrument, study) {
  path <- file.path(dataset_dir, "fulltexts", instrument, paste0(study$study_id, ".pdf"))
  if (!file.exists(path)) stop("PDF not found: ", path, call. = FALSE)
  if (!is.null(study$fulltext_sha256) && !is.na(study$fulltext_sha256)) {
    got <- sha256_file(path)
    if (!identical(got, study$fulltext_sha256)) {
      stop("PDF hash does not match the frozen table for ", study$study_id, call. = FALSE)
    }
  }
  path
}

# The manuals for one instrument, in manifest order: a named vector of PDF
# paths (names = doc_id). Each PDF must match pdf_sha256 in the manifest, so
# every call gets exactly the official file recorded there.
load_manuals <- function(instrument, manuals_dir, manifest_file = "manuals/manifest.csv") {
  m <- utils::read.csv(manifest_file, stringsAsFactors = FALSE, na.strings = "", encoding = "UTF-8")
  m <- m[m$instrument == instrument, , drop = FALSE]
  if (nrow(m) == 0) stop("No manuals listed for ", instrument, call. = FALSE)
  m <- m[order(m$order), , drop = FALSE]
  paths <- file.path(manuals_dir, m$pdf_file)
  for (i in seq_along(paths)) {
    if (!file.exists(paths[i])) stop("Manual PDF not found: ", paths[i], call. = FALSE)
    if (!identical(sha256_file(paths[i]), m$pdf_sha256[i])) {
      stop("Manual PDF hash does not match the manifest: ", m$doc_id[i], call. = FALSE)
    }
  }
  stats::setNames(paths, m$doc_id)
}

# System instructions and the instrument prompt, with the output format filled in
# from the instrument's spec. Prompt files start with header lines:
#   version: 0.1
#   status: draft | frozen
# followed by a line "---" and the prompt text.
read_prompt_file <- function(path) {
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  sep <- which(lines == "---")[1]
  if (is.na(sep)) stop("Prompt file has no '---' header separator: ", path, call. = FALSE)
  header <- lines[seq_len(sep - 1)]
  get <- function(key) {
    hit <- grep(paste0("^", key, ":"), header, value = TRUE)
    if (length(hit) == 0) NA_character_ else trimws(sub(paste0("^", key, ":"), "", hit[1]))
  }
  list(version = get("version"), status = get("status"),
       text = paste(lines[-seq_len(sep)], collapse = "\n"))
}

load_prompts <- function(instrument, prompts_dir = "prompts") {
  sys <- read_prompt_file(file.path(prompts_dir, "system.txt"))
  ins <- read_prompt_file(file.path(prompts_dir, paste0(instrument, ".txt")))
  spec <- load_spec(instrument, prompts_dir)
  ins_text <- sub("{{OUTPUT_FORMAT}}", render_output_format(spec), ins$text, fixed = TRUE)
  list(system = sys$text, prompt = ins_text,
       version = paste0("system-", sys$version, "_", instrument, "-", ins$version),
       frozen = identical(sys$status, "frozen") && identical(ins$status, "frozen"))
}
