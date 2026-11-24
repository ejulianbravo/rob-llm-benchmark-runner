# Cost test: one development study per instrument, all models, run 1 only.
# Approved by Edward on 2026-11-23 for up to US$30 in total (decision log D146).
#
# Usage, from the repo folder:
#   Rscript scripts/cost_test.R          # plan and estimate only, sends nothing
#   Rscript scripts/cost_test.R --run    # send the calls, if the estimate is within the budget
#
# Needs config/paths.yaml (dev_dir set) and the API keys in ~/.Renviron.

for (f in c(list.files("R", full.names = TRUE), list.files("parse", full.names = TRUE))) source(f)
budget_usd <- 30
send <- "--run" %in% commandArgs(trailingOnly = TRUE)

plans <- lapply(INSTRUMENTS, function(i) run_batch(i, runs = 1, set = "development", dry_run = TRUE))
plan <- do.call(rbind, plans)
print(plan[, c("model_label", "study_id", "pdf_pages", "manual_pages", "est_input_tokens", "est_usd")], row.names = FALSE)
total <- sum(plan$est_usd)
message(sprintf("Total estimate: US$%.2f for %d calls (budget US$%.0f).", total, nrow(plan), budget_usd))

if (!send) quit(save = "no")
if (total > budget_usd) stop("Estimate above budget; nothing sent.")
for (i in INSTRUMENTS) {
  run_batch(i, runs = 1, set = "development", dry_run = FALSE, approved_budget_usd = budget_usd)
}
res <- parse_outputs("outputs/run_log.csv")
log <- utils::read.csv("outputs/run_log.csv")
print(log[, c("call_id", "status", "duration_s", "input_tokens", "cache_read_tokens", "output_tokens", "reasoning_tokens", "finish_reason")], row.names = FALSE)
for (i in names(res$tables)) print(res$tables[[i]][, c("assessment_id", "parse_status", "parse_issues")], row.names = FALSE)

# Actual cost from the token counts the providers returned (list prices; cache
# discounts not applied, so this errs high where caching was used).
models <- load_models()
price_in <- vapply(log$model_label, function(l) models[[l]]$price_input_usd_per_m, 0)
price_out <- vapply(log$model_label, function(l) models[[l]]$price_output_usd_per_m, 0)
log$usd <- round(log$input_tokens / 1e6 * price_in + log$output_tokens / 1e6 * price_out, 3)
print(log[, c("call_id", "usd")], row.names = FALSE)
message(sprintf("Actual (from returned tokens, list prices): US$%.2f", sum(log$usd, na.rm = TRUE)))
