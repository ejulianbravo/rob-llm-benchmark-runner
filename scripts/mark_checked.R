# Record that a manual's text file has been checked against its PDF.
#
# Usage (from the repo folder):
#   Rscript scripts/mark_checked.R /path/to/manuals_folder doc_id "Your Name"
#
# Writes the text file's SHA-256, the checker's name and today's date into
# manuals/manifest.csv. From then on the runner refuses the text if it changes.

source("R/utils.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) stop("Give: manuals folder, doc_id, checker name.")
manifest_file <- "manuals/manifest.csv"
m <- utils::read.csv(manifest_file, stringsAsFactors = FALSE, na.strings = "", encoding = "UTF-8")
i <- which(m$doc_id == args[2])
if (length(i) != 1) stop("doc_id not found in manifest: ", args[2])
path <- file.path(args[1], m$text_file[i])
txt <- paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
m$text_sha256[i] <- sha256_text(txt)
m$checked_by[i] <- args[3]
m$checked_date[i] <- format(Sys.Date())
utils::write.csv(m, manifest_file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
message("Recorded ", args[2], ": ", m$text_sha256[i])
