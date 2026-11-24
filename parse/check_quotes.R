# Check each quotation against the text of the PDF the model received.
#
# The text is extracted in reading order with poppler's pdftotext
# (pdf_pages_text() in R/utils.R), so the check depends on the PDF's text layer. Results:
#   match_on_page    quote found on the page the model gave
#   match_other_page quote found, but on another page
#   not_found        quote not found anywhere in the extracted text
#   no_text_layer    the PDF has (almost) no extractable text (e.g. a scan); a
#                    full text page has thousands of characters, so fewer than
#                    min_chars_per_page on average means no usable text layer
#                    (RB_029, a scan, has only a publisher footer: ~145 per page)
# Both texts are normalised before matching: lower case, ligatures unified,
# then everything except letters and digits removed (so line breaks, hyphens,
# punctuation and spacing differences do not matter).
# A "not_found" is a flag for a person to check, not proof of an invented
# quotation: tables, two-column layouts and scrambled text layers can hide real
# quotes. In the frozen dataset NOS_018, RE_013, RI_006 and RI_021 have text
# layers that extract in scrambled order, and RB_029 is a scan without text.

normalise_text <- function(x) {
  x <- tolower(x)
  x <- gsub("\uFB01", "fi", x)
  x <- gsub("\uFB02", "fl", x)
  gsub("[^[:alnum:]]", "", x)
}

check_quotes <- function(quotes, pdf_for_study, min_chars_per_page = 300) {
  if (!nrow(quotes)) return(quotes)
  quotes$quote_check <- NA_character_
  for (sid in unique(quotes$study_id)) {
    pages <- normalise_text(pdf_pages_text(pdf_for_study(sid)))
    no_text <- mean(nchar(pages)) < min_chars_per_page
    idx <- which(quotes$study_id == sid)
    for (i in idx) {
      q <- normalise_text(quotes$quote[i])
      if (no_text) { quotes$quote_check[i] <- "no_text_layer"; next }
      if (is.na(q) || !nzchar(q)) { quotes$quote_check[i] <- "empty_quote"; next }
      pg <- suppressWarnings(as.integer(quotes$page[i]))
      hit <- grepl(q, pages, fixed = TRUE)
      quotes$quote_check[i] <- if (!is.na(pg) && pg >= 1 && pg <= length(pages) && hit[pg]) {
        "match_on_page"
      } else if (any(hit) || grepl(q, paste(pages, collapse = " "), fixed = TRUE)) {
        "match_other_page"
      } else "not_found"
    }
  }
  quotes
}
