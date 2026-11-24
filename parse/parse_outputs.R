# Turn raw model outputs into assessment tables (one per instrument, columns as
# in the project's schema.md) and a long table of quotations.
#
# Reading rules:
#   - The model's answer must be one JSON object. A single surrounding code
#     fence (```json ... ```) is removed and noted; nothing else is changed.
#   - Values are kept exactly as the model wrote them (trimmed of spaces).
#     Values outside the instrument's options are kept and flagged in
#     parse_issues, not recoded. Mapping to analysis categories happens in the
#     analysis scripts.
#   - An answer that cannot be read is kept as a row with parse_status =
#     "unreadable" and empty values.
#
# Usage: source the R/ files and this file, then
#   res <- parse_outputs("outputs/run_log.csv")
#   res$tables$rob2; res$quotes

parse_answer_json <- function(text) {
  if (is.na(text)) return(list(value = NULL, note = "no_text"))
  t <- trimws(text)
  note <- NA_character_
  fence <- "^```(json)?\\s*\\n([\\s\\S]*)\\n```$"
  if (grepl(fence, t, perl = TRUE)) {
    t <- sub(fence, "\\2", t, perl = TRUE)
    note <- "code_fence_removed"
  }
  val <- tryCatch(jsonlite::fromJSON(t, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(val) || !is.list(val) || is.null(names(val))) {
    return(list(value = NULL, note = "not_json_object"))
  }
  list(value = val, note = note)
}

qcol <- function(id) paste0("q", gsub(".", "_", id, fixed = TRUE))
dcol <- function(id) tolower(id)
val_chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else trimws(as.character(x[[1]]))

# One row of instrument columns from a parsed answer.
answer_row <- function(ans, spec) {
  issues <- character()
  out <- list()
  for (f in names(spec$top_fields)) {
    out[[f]] <- val_chr(ans[[f]])
    if (!is.na(out[[f]]) && !out[[f]] %in% unlist(spec$top_fields[[f]])) issues <- c(issues, paste0("unexpected_", f))
  }

  for (ch in spec$choices %||% list()) {
    out[[paste0("choice_", ch$id)]] <- val_chr(ans$review_choices[[ch$id]])
    if (is.na(out[[paste0("choice_", ch$id)]])) issues <- c(issues, paste0("missing_choice_", ch$id))
  }

  if (identical(spec$unit, "item")) {
    got <- ans$items %||% list()
    by_id <- stats::setNames(got, vapply(got, function(x) val_chr(x$item), ""))
    total <- 0
    for (it in spec$items) {
      col <- paste0(tolower(it$id), "_stars")
      v <- val_chr(by_id[[it$id]]$stars)
      out[[col]] <- v
      if (is.null(by_id[[it$id]])) { issues <- c(issues, paste0("missing_", it$id)); total <- NA; next }
      n <- suppressWarnings(as.numeric(v))
      if (is.na(n) || n < 0 || n > it$max_stars || n != round(n)) {
        issues <- c(issues, paste0("unexpected_stars_", it$id)); total <- NA
      } else if (!is.na(total)) total <- total + n
    }
    out$total_stars_calculated <- total
    return(list(row = out, issues = issues))
  }

  got <- ans$domains %||% list()
  by_id <- stats::setNames(got, vapply(got, function(x) val_chr(x$domain), ""))
  for (d in spec$domains) {
    dom <- by_id[[d$id]]
    if (is.null(dom)) issues <- c(issues, paste0("missing_", d$id))
    variant <- if (!is.null(d$variant_from)) out[[d$variant_from]] else val_chr(dom$variant)
    if (!is.null(d$variants) && is.null(d$variant_from)) {
      out[[paste0(dcol(d$id), "_variant")]] <- variant
      if (!is.na(variant) && !variant %in% names(d$variants)) issues <- c(issues, paste0("unexpected_variant_", d$id))
    }
    out[[dcol(d$id)]] <- val_chr(dom$judgement)
    if (!is.null(dom) && !out[[dcol(d$id)]] %in% unlist(spec$judgement_options)) {
      issues <- c(issues, paste0("unexpected_judgement_", d$id))
    }
    if (!is.null(spec$applicability) && d$id %in% unlist(spec$applicability$domains)) {
      a <- val_chr(dom$applicability)
      out[[paste0(dcol(d$id), "_app")]] <- a
      if (!is.null(dom) && !a %in% unlist(spec$applicability$options)) issues <- c(issues, paste0("unexpected_applicability_", d$id))
    }
    expected <- domain_questions(d, if (is.na(variant %||% NA)) NULL else variant)
    for (q in domain_questions(d)) {
      v <- val_chr(dom$answers[[q]])
      out[[qcol(q)]] <- v
      if (q %in% expected && !is.null(dom)) {
        if (is.na(v)) issues <- c(issues, paste0("missing_answer_", q))
        else if (!v %in% unlist(spec$answer_options)) issues <- c(issues, paste0("unexpected_answer_", q))
      }
    }
    extra <- setdiff(names(dom$answers %||% list()), domain_questions(d))
    if (length(extra)) issues <- c(issues, paste0("extra_question_", d$id))
  }
  if (!is.null(spec$applicability)) {
    # PROBAST names: d1_rob ... and overall_rob / overall_app
    for (d in spec$domains) names(out)[names(out) == dcol(d$id)] <- paste0(dcol(d$id), "_rob")
    out$overall_rob <- val_chr(ans$overall)
    out$overall_app <- val_chr(ans$overall_applicability)
    if (!out$overall_app %in% unlist(spec$applicability$options)) issues <- c(issues, "unexpected_overall_app")
    if (!out$overall_rob %in% unlist(spec$overall_options)) issues <- c(issues, "unexpected_overall")
  } else {
    out$overall <- val_chr(ans$overall)
    if (!out$overall %in% unlist(spec$overall_options)) issues <- c(issues, "unexpected_overall")
  }
  list(row = out, issues = issues)
}

answer_quotes <- function(ans, spec) {
  units <- if (identical(spec$unit, "item")) ans$items else ans$domains
  key <- if (identical(spec$unit, "item")) "item" else "domain"
  rows <- lapply(units %||% list(), function(u) {
    ev <- u$evidence %||% list()
    if (!length(ev)) return(NULL)
    data.frame(unit = val_chr(u[[key]]),
               page = vapply(ev, function(e) val_chr(e$page), ""),
               quote = vapply(ev, function(e) val_chr(e$quote), ""),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

parse_outputs <- function(log_file = "outputs/run_log.csv", prompts_dir = "prompts") {
  log <- utils::read.csv(log_file, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  log <- log[!is.na(log$raw_file), , drop = FALSE]
  tables <- list(); quotes <- list()
  for (k in seq_len(nrow(log))) {
    r <- log[k, ]
    spec <- load_spec(r$instrument, prompts_dir)
    body <- tryCatch(jsonlite::fromJSON(r$raw_file, simplifyVector = FALSE), error = function(e) NULL)
    text <- if (is.null(body)) NA_character_ else response_text(r$provider, body)
    parsed <- parse_answer_json(text)
    settings <- tryCatch(jsonlite::fromJSON(r$settings), error = function(e) list())
    common <- list(
      assessment_id = sprintf("%s_LLM_%s_R%d", r$study_id, toupper(r$model_label), r$run_number),
      study_id = r$study_id, assessor_type = "llm_run", reference_role = "model_output",
      source_id = NA_character_,
      granularity = if (r$instrument == "nos") "item_stars" else "question",
      verification_status = NA_character_,
      model_id = r$model_returned %||% r$model_requested, model_label = r$model_label,
      prompt_version = r$prompt_version, run_number = r$run_number,
      temperature = if (is.null(settings$temperature)) NA_real_ else settings$temperature,
      run_datetime = r$started_at, finish_reason = r$finish_reason, call_id = r$call_id
    )
    if (is.null(parsed$value)) {
      inst <- list(); issues <- character(); status <- "unreadable"
    } else {
      a <- answer_row(parsed$value, spec)
      inst <- a$row; issues <- a$issues
      status <- if (length(issues)) "read_with_issues" else "read"
      q <- answer_quotes(parsed$value, spec)
      if (!is.null(q)) {
        quotes[[length(quotes) + 1]] <- cbind(
          data.frame(call_id = r$call_id, study_id = r$study_id, instrument = r$instrument,
                     model_label = r$model_label, run_number = r$run_number), q)
      }
    }
    row <- c(common, inst, list(parse_status = status,
                                parse_note = parsed$note,
                                parse_issues = paste(issues, collapse = ";")))
    tables[[r$instrument]] <- c(tables[[r$instrument]], list(row))
  }
  tables <- lapply(tables, function(rows) {
    cols <- unique(unlist(lapply(rows, names)))
    do.call(rbind, lapply(rows, function(x) {
      x <- x[cols]; names(x) <- cols
      as.data.frame(lapply(x, function(v) if (is.null(v)) NA else v), stringsAsFactors = FALSE)
    }))
  })
  list(tables = tables, quotes = if (length(quotes)) do.call(rbind, quotes) else data.frame())
}
