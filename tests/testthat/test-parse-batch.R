# Offline tests for prompts, batch planning, parsing and the quotation check.
# Model answers below are constructed by hand; they are not real model outputs.

withr::local_envvar(ANTHROPIC_API_KEY = "test-a", OPENAI_API_KEY = "test-o", GEMINI_API_KEY = "test-g")
old <- setwd(root); withr::defer(setwd(old), teardown_env())

test_that("Every instrument prompt renders, with its output format and draft status", {
  for (i in INSTRUMENTS) {
    p <- load_prompts(i)
    expect_false(grepl("{{OUTPUT_FORMAT}}", p$prompt, fixed = TRUE))
    expect_match(p$prompt, "Return one JSON object")
    if (i == "nos") expect_match(p$prompt, "review_choices")
    expect_false(p$frozen)
  }
})

# A small development set laid out like the frozen dataset
make_dev_set <- function(dir) {
  dir.create(file.path(dir, "data", "rob2"), recursive = TRUE)
  dir.create(file.path(dir, "fulltexts", "rob2"), recursive = TRUE)
  pdf <- make_pdf(c("Randomisation was done by computer.", "All patients were analysed."))
  file.copy(pdf, file.path(dir, "fulltexts", "rob2", "DEV_001.pdf"))
  utils::write.csv(data.frame(study_id = "DEV_001", target_description = "Assess the result.",
                              fulltext_sha256 = sha256_file(pdf)),
                   file.path(dir, "data", "rob2", "studies_rob2.csv"), row.names = FALSE)
  man <- file.path(dir, "manuals"); dir.create(man)
  g <- file.path(man, "g.pdf"); c2 <- file.path(man, "c.pdf")
  file.copy(make_pdf(c("guidance p1", "guidance p2")), g); file.copy(make_pdf("cribsheet"), c2)
  manifest <- file.path(dir, "manifest.csv")
  utils::write.csv(data.frame(instrument = "rob2", order = 1:2, doc_id = c("g", "c"),
                              pdf_file = c("g.pdf", "c.pdf"), pdf_sha256 = c(sha256_file(g), sha256_file(c2))),
                   manifest, row.names = FALSE)
  list(dataset_dir = dir, dev_dir = dir, manuals_dir = man, manifest_file = manifest)
}

test_that("Batch: dry run plans and estimates; benchmark set refused with draft prompts", {
  paths <- make_dev_set(withr::local_tempdir())
  out <- withr::local_tempdir()
  plan <- run_batch("rob2", runs = 1:2, set = "development", paths = paths, out_dir = out)
  expect_equal(nrow(plan), 6)                 # 1 study x 3 models x 2 runs
  expect_true(all(plan$est_usd > 0))
  expect_true(all(plan$manual_pages == 3))
  expect_error(run_batch("rob2", set = "benchmark", paths = paths, out_dir = out), "not frozen")
  expect_error(run_batch("rob2", set = "development", paths = paths, out_dir = out,
                         dry_run = FALSE, approved_budget_usd = 0), "above the approved budget")
})

test_that("Batch: runs within budget, logs, and skips finished calls on restart", {
  paths <- make_dev_set(withr::local_tempdir())
  out <- withr::local_tempdir()
  answer <- list(instrument = "rob2", effect_of_interest = "assignment",
                 domains = lapply(1:5, function(d) list(domain = paste0("D", d), evidence = list(),
                                                        answers = list(), judgement = "Low")),
                 overall = "Low")
  httr2::local_mocked_responses(function(req) mock_json(list(
    model = "claude-fable-5-2", stop_reason = "end_turn",
    content = list(list(type = "text", text = as.character(jsonlite::toJSON(answer, auto_unbox = TRUE)))),
    usage = list(input_tokens = 10, output_tokens = 10))))
  log <- run_batch("rob2", model_labels = "claude", runs = 1, set = "development", paths = paths,
                   out_dir = out, dry_run = FALSE, approved_budget_usd = 5)
  expect_equal(nrow(log), 1)
  expect_equal(log$status, "ok")
  expect_equal(log$study_set, "development")
  again <- run_batch("rob2", model_labels = "claude", runs = 1, set = "development", paths = paths, out_dir = out)
  expect_equal(nrow(again), 0)
})

# Write a fake raw output + run log row for the parser
fake_run <- function(out, instrument, answer_text, study = "S1", label = "claude") {
  dir.create(file.path(out, "raw"), recursive = TRUE, showWarnings = FALSE)
  raw <- file.path(out, "raw", paste0(study, "_", instrument, ".json"))
  writeLines(as.character(jsonlite::toJSON(list(model = "m", content = list(list(type = "text", text = answer_text))),
                                           auto_unbox = TRUE)), raw)
  data.frame(call_id = paste(study, instrument), study_id = study, instrument = instrument,
             provider = "anthropic", model_label = label, model_requested = "m", model_returned = "m",
             run_number = 1, prompt_version = "v", settings = "{\"temperature\":0}",
             started_at = "t", finish_reason = "end_turn", raw_file = raw)
}

test_that("Parser: RoB 2 answer to columns; values kept as written; issues flagged", {
  out <- withr::local_tempdir()
  ans <- list(instrument = "rob2", effect_of_interest = "adherence",
              domains = list(
                list(domain = "D1", evidence = list(list(page = 1, quote = "Randomisation was done by computer.")),
                     answers = list(`1.1` = "Y", `1.2` = "PY", `1.3` = "N"), judgement = "Low"),
                list(domain = "D2", evidence = list(), answers = list(`2.1` = "N", `2.2` = "N", `2.3` = "NA",
                     `2.4` = "NA", `2.5` = "NA", `2.6` = "Y"), judgement = "low risk"),
                list(domain = "D3", evidence = list(), answers = list(`3.1` = "Y"), judgement = "Low"),
                list(domain = "D4", evidence = list(), answers = list(`4.1` = "N", `4.2` = "N", `4.3` = "N"), judgement = "Low")),
              overall = "Some concerns")
  log <- fake_run(out, "rob2", as.character(jsonlite::toJSON(ans, auto_unbox = TRUE)))
  utils::write.csv(log, file.path(out, "log.csv"), row.names = FALSE)
  res <- parse_outputs(file.path(out, "log.csv"))
  t <- res$tables$rob2
  expect_equal(t$d1, "Low")
  expect_equal(t$d2, "low risk")                       # kept, not recoded
  expect_equal(t$q2_6, "Y"); expect_true(is.na(t$q2_7))  # adherence variant has no 2.7
  expect_equal(t$temperature, 0)
  expect_equal(t$parse_status, "read_with_issues")
  expect_match(t$parse_issues, "unexpected_judgement_D2")
  expect_match(t$parse_issues, "missing_D5")
  expect_false(grepl("missing_answer_2.7", t$parse_issues))
  expect_equal(nrow(res$quotes), 1)
})

test_that("Parser: NOS stars, PROBAST applicability, code fence, unreadable", {
  out <- withr::local_tempdir()
  nos <- list(instrument = "nos", form = "cohort",
              review_choices = list(comparability_main_factor = "age", comparability_additional_factor = "severity",
                                    followup_long_enough = "28 days", adequate_followup = ">80% followed"),
              items = lapply(c("S1","S2","S3","S4","C","OE1","OE2","OE3"), function(i)
                list(item = i, evidence = list(), stars = if (i == "C") 2 else 1)))
  prob <- list(instrument = "probast",
               domains = lapply(1:4, function(d) {
                 x <- list(domain = paste0("D", d), evidence = list(), answers = list(), judgement = "High")
                 if (d < 4) x$applicability <- "Low"; x }),
               overall = "High", overall_applicability = "Low")
  log <- rbind(
    fake_run(out, "nos", paste0("```json\n", jsonlite::toJSON(nos, auto_unbox = TRUE), "\n```"), "N1"),
    fake_run(out, "probast", as.character(jsonlite::toJSON(prob, auto_unbox = TRUE)), "P1"),
    fake_run(out, "nos", "I cannot assess this study.", "N2"))
  utils::write.csv(log, file.path(out, "log.csv"), row.names = FALSE)
  res <- parse_outputs(file.path(out, "log.csv"))
  n <- res$tables$nos
  expect_equal(n$total_stars_calculated[n$study_id == "N1"], 9)
  expect_equal(n$parse_note[n$study_id == "N1"], "code_fence_removed")
  expect_equal(n$parse_status[n$study_id == "N2"], "unreadable")
  expect_equal(n$choice_comparability_main_factor[n$study_id == "N1"], "age")
  expect_equal(n$parse_status[n$study_id == "N1"], "read")
  p <- res$tables$probast
  expect_equal(p$d1_rob, "High"); expect_equal(p$d1_app, "Low"); expect_equal(p$overall_app, "Low")
  expect_null(p$d4_app)
})

test_that("Quotation check: page match, other page, not found, no text layer", {
  pdf <- make_pdf(c("Randomisation was done by computer.", "All patients were analysed."))
  q <- data.frame(study_id = "S1", page = c("1", "1", "2", "1"),
                  quote = c("randomisation was done by computer", "All patients were analysed",
                            "Patients were blinded", "Randomisation  was\ndone"),
                  stringsAsFactors = FALSE)
  res <- check_quotes(q, function(id) pdf, min_chars_per_page = 10)
  expect_equal(res$quote_check, c("match_on_page", "match_other_page", "not_found", "match_on_page"))
  blank <- make_pdf(c("", ""))
  expect_equal(check_quotes(q[1, ], function(id) blank, min_chars_per_page = 10)$quote_check, "no_text_layer")
  expect_equal(check_quotes(q[1, ], function(id) pdf)$quote_check, "no_text_layer")  # default threshold: short test pages count as no text
})

test_that("Manual PDFs must match the manifest hash", {
  paths <- make_dev_set(withr::local_tempdir())
  file.copy(make_pdf("changed"), file.path(paths$manuals_dir, "c.pdf"), overwrite = TRUE)
  expect_error(load_manuals("rob2", paths$manuals_dir, paths$manifest_file), "does not match")
})

test_that("The repo manifest lists PDFs for every instrument", {
  m <- utils::read.csv(file.path(root, "manuals", "manifest.csv"))
  expect_setequal(unique(m$instrument), INSTRUMENTS)
  expect_true(all(nchar(m$pdf_sha256) == 64))
})
