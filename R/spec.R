# Instrument specs (prompts/specs/<instrument>.json) define the domains or items,
# the signalling-question IDs and the allowed values. The same spec is used to
# write the output format into the prompt and to read the model's answer, so
# the two cannot drift apart.

load_spec <- function(instrument, prompts_dir = "prompts") {
  jsonlite::fromJSON(file.path(prompts_dir, "specs", paste0(instrument, ".json")),
                     simplifyVector = FALSE)
}

# Question IDs for a domain; for domains with variants, those of `variant`
# (or the union of all variants when variant is NULL).
domain_questions <- function(domain, variant = NULL) {
  if (!is.null(domain$questions)) return(unlist(domain$questions))
  if (!is.null(variant) && !is.null(domain$variants[[variant]])) return(unlist(domain$variants[[variant]]))
  unique(unlist(domain$variants))
}

opts <- function(x) paste(unlist(x), collapse = " | ")

render_output_format <- function(spec) {
  ev <- '"evidence": [{"page": <page number in the PDF>, "quote": "<exact words copied from the report>"}]'
  top <- vapply(names(spec$top_fields), function(f) {
    sprintf('  "%s": "%s",', f, opts(spec$top_fields[[f]]))
  }, "")
  if (identical(spec$unit, "item")) {
    items <- vapply(spec$items, function(it) {
      sprintf('    {"item": "%s", %s, "stars": <0 to %d>}', it$id, ev, it$max_stars)
    }, "")
    body <- c("{", sprintf('  "instrument": "%s",', spec$instrument), top,
              '  "items": [', paste(items, collapse = ",\n"), "  ]", "}")
    notes <- c(spec$item_note, "Give every item, in this order.")
  } else {
    doms <- vapply(spec$domains, function(d) {
      variant_line <- if (!is.null(d$variants) && is.null(d$variant_from)) {
        sprintf('"variant": "%s", ', opts(names(d$variants)))
      } else ""
      qs <- domain_questions(d)
      answers <- paste(sprintf('"%s": "<answer>"', qs), collapse = ", ")
      app <- if (!is.null(spec$applicability) && d$id %in% unlist(spec$applicability$domains)) {
        sprintf(', "applicability": "%s"', opts(spec$applicability$options))
      } else ""
      sprintf('    {"domain": "%s", %s%s, "answers": {%s}, "judgement": "<judgement>"%s}',
              d$id, variant_line, ev, answers, app)
    }, "")
    overall <- '  "overall": "<overall judgement>"'
    if (!is.null(spec$applicability)) {
      overall <- paste0(overall, sprintf(',\n  "overall_applicability": "%s"', opts(spec$applicability$options)))
    }
    body <- c("{", sprintf('  "instrument": "%s",', spec$instrument), top,
              '  "domains": [', paste(doms, collapse = ",\n"), "  ],", overall, "}")
    var_notes <- unlist(lapply(spec$domains, function(d) {
      if (is.null(d$variants)) return(NULL)
      sprintf("%s questions by variant: %s.", d$id,
              paste(vapply(names(d$variants), function(v) {
                paste0(v, " = ", paste(unlist(d$variants[[v]]), collapse = ", "))
              }, ""), collapse = "; "))
    }))
    notes <- c(
      sprintf("Answers to signalling questions: %s. Use NA only where the instrument says a question is not asked.",
              opts(spec$answer_options)),
      sprintf("Domain judgements: %s.", opts(spec$judgement_options)),
      sprintf("Overall judgement: %s.", opts(spec$overall_options)),
      var_notes,
      "Give every domain, in this order, with only the questions of the variant used."
    )
  }
  paste(c("Return one JSON object and nothing else (no text before or after it, no code fences), in this form:",
          "", body, "", "Rules for the values:", paste0("- ", notes),
          "- In each evidence list, quote the report word for word. Give the page number of the PDF page where the quote appears. Use an empty list if the report says nothing relevant."),
        collapse = "\n")
}
