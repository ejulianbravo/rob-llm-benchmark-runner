# Run (or estimate) a batch of calls for one instrument.
#
#   set = "development": studies outside the benchmark (paths$dev_dir, same
#         folder layout as the frozen dataset). Draft prompts are allowed. Use
#         this for prompt work and the cost test.
#   set = "benchmark":   the frozen benchmark (paths$dataset_dir). Refused
#         unless the prompts are marked frozen.
# In both sets the manual PDFs must match the hashes in manuals/manifest.csv.
#
# Nothing is sent unless dry_run = FALSE AND approved_budget_usd is at least the
# estimated cost of the calls still to make. With dry_run = TRUE (the default)
# the function only returns the plan and the cost estimate.
#
# Order: all of run 1, then run 2; within a run, one model at a time, so that
# calls sharing the same manuals and prompt are close together for caching.
# Calls that already have a raw output are skipped, so an interrupted batch can
# be restarted with the same command.

load_paths <- function(file = "config/paths.yaml") {
  if (!file.exists(file)) stop("Create ", file, " from config/paths.example.yaml", call. = FALSE)
  paths <- yaml::read_yaml(file)
  lapply(paths, function(x) if (is.character(x)) path.expand(x) else x)
}

load_models <- function(labels = NULL, file = "config/models.yaml") {
  models <- yaml::read_yaml(file)$models
  names(models) <- vapply(models, `[[`, "", "label")
  if (is.null(labels)) models else models[labels]
}

run_batch <- function(instrument, model_labels = NULL, runs = 1:2,
                      set = c("development", "benchmark"), study_ids = NULL,
                      dry_run = TRUE, approved_budget_usd = NULL,
                      paths = load_paths(), out_dir = "outputs") {
  set <- match.arg(set)
  stopifnot(instrument %in% INSTRUMENTS)
  data_dir <- if (set == "benchmark") paths$dataset_dir else paths$dev_dir
  if (is.null(data_dir)) stop("No folder set for ", set, " in config/paths.yaml", call. = FALSE)

  prompts <- load_prompts(instrument)
  manuals <- load_manuals(instrument, paths$manuals_dir,
                          manifest_file = paths$manifest_file %||% "manuals/manifest.csv")
  if (set == "benchmark" && !prompts$frozen) {
    stop("Prompts are not frozen; benchmark runs are refused.", call. = FALSE)
  }

  studies <- load_studies(data_dir, instrument)
  if (!is.null(study_ids)) studies <- studies[studies$study_id %in% study_ids, , drop = FALSE]
  if (nrow(studies) == 0) stop("No studies selected.", call. = FALSE)
  models <- load_models(model_labels)

  # Plan: every call still to make, with its estimated cost.
  plan <- list()
  for (run in runs) for (m in models) for (i in seq_len(nrow(studies))) {
    s <- studies[i, ]
    if (file.exists(raw_path(out_dir, instrument, s$study_id, m, run))) next
    pdf <- study_pdf(data_dir, instrument, s)
    est <- estimate_call_cost(m, prompts$system, manuals, prompts$prompt, pdf, s$target_description)
    plan[[length(plan) + 1]] <- cbind(
      data.frame(run_number = run, model_label = m$label, study_id = s$study_id, pdf = pdf),
      est)
  }
  plan <- if (length(plan)) do.call(rbind, plan) else data.frame()
  total <- if (nrow(plan)) sum(plan$est_usd) else 0
  message(sprintf("%s, %s set: %d calls to make, estimated US$%.2f (rough; see R/estimate_cost.R).",
                  instrument, set, nrow(plan), total))

  if (dry_run || nrow(plan) == 0) return(invisible(plan))
  if (is.null(approved_budget_usd) || is.na(total) || total > approved_budget_usd) {
    stop(sprintf("Estimated cost US$%.2f is above the approved budget (%s). Nothing sent.",
                 total, format(approved_budget_usd)), call. = FALSE)
  }

  log_file <- file.path(out_dir, "run_log.csv")
  for (k in seq_len(nrow(plan))) {
    p <- plan[k, ]
    s <- studies[studies$study_id == p$study_id, ]
    row <- run_call(study_id = p$study_id, instrument = instrument, model = models[[p$model_label]],
                    run_number = p$run_number, prompt_version = prompts$version,
                    system = prompts$system, manuals = manuals,
                    instrument_prompt = prompts$prompt, pdf_path = p$pdf,
                    target = s$target_description, out_dir = out_dir)
    row$study_set <- set
    append_run_log(row, log_file)
    message(sprintf("[%d/%d] %s: %s", k, nrow(plan), row$call_id, row$status))
  }
  invisible(utils::read.csv(log_file, stringsAsFactors = FALSE))
}
