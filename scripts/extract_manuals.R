# Make the plain-text versions of the manuals that are sent with each call.
#
# Usage (from the repo folder):
#   Rscript scripts/extract_manuals.R /path/to/manuals_folder
#
# The folder must hold the PDFs named in manuals/manifest.csv (pdf_file). For
# each one this script:
#   1. checks the PDF against pdf_sha256 in the manifest;
#   2. extracts its text with pdftools (page breaks marked "[Page n]");
#   3. writes the text file next to the PDF and records its SHA-256 in the manifest.
# It does not fill checked_by / checked_date: a person must read each text file
# against its PDF (tables and algorithms in particular), correct it by hand if
# needed, then record the final hash and their name with scripts/mark_checked.R.

source("R/utils.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) stop("Give the manuals folder as the only argument.")
manuals_dir <- args[1]
manifest_file <- "manuals/manifest.csv"
m <- utils::read.csv(manifest_file, stringsAsFactors = FALSE, na.strings = "", encoding = "UTF-8")

for (i in seq_len(nrow(m))) {
  pdf <- file.path(manuals_dir, m$pdf_file[i])
  if (!file.exists(pdf)) { message("Missing: ", pdf); next }
  if (!identical(sha256_file(pdf), m$pdf_sha256[i])) {
    message("PDF hash differs from manifest, skipped: ", m$pdf_file[i]); next
  }
  out <- file.path(manuals_dir, m$text_file[i])
  if (file.exists(out)) { message("Text exists, not overwritten: ", out); next }
  pages <- pdftools::pdf_text(pdf)
  txt <- paste(sprintf("[Page %d]\n%s", seq_along(pages), pages), collapse = "\n")
  writeLines(txt, out, useBytes = TRUE)
  message("Wrote ", out)
}
