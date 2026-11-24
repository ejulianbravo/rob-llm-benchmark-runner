# rob-llm-benchmark-runner

R code that runs large language models on the LLM risk-of-bias benchmark (150 studies; RoB 2, ROBINS-I, ROBINS-E, NOS, PROBAST) and turns their outputs into tables for analysis.

Status: in development. Prompts are drafts. Nothing has been sent to a model API yet; all tests use mocked responses.

## What one call contains

One request per study, model and run, in this order:

1. system instructions (`prompts/system.txt`);
2. the instrument's full official guidance and assessment form, as checked text (`manuals/manifest.csv`);
3. the instrument prompt with its output format (`prompts/<instrument>.txt`, `prompts/specs/<instrument>.json`);
4. the study report as a PDF file (the frozen benchmark copy, hash-checked);
5. the assessment target from the frozen benchmark.

No worked examples, no retrieval. Parts 1–3 are the same for every study of an instrument, so they come first and providers can cache them.

The model returns one JSON object: for each domain (or NOS item), quotations with page numbers, then signalling-question answers, then the judgement; then the overall judgement (not for NOS).

## What is stored

- `outputs/raw/`: every successful response body, unchanged, written before anything reads it. Never overwritten; a run number is used up once a response is stored.
- `outputs/errors/`: bodies of HTTP errors after retries (rate limits, server errors). These are not model outputs; the same run number can be tried again.
- `outputs/run_log.csv`: one row per call: study, model requested and returned, run, prompt version, all settings, hashes of PDF, each manual, prompt and target, request size, timing, status, tokens (one definition for all providers, see `R/responses.R`) and the provider's own usage record.

Unusable answers are kept and logged. Nothing is repaired or re-asked.

## Use

```r
source_dir <- function(d) for (f in list.files(d, "\\.R$", full.names = TRUE)) source(f)
source_dir("R"); source_dir("parse")

# 1. Plan and estimate (sends nothing)
plan <- run_batch("rob2", set = "development")

# 2. Run, only within a budget you have approved (US$)
log <- run_batch("rob2", set = "development", dry_run = FALSE, approved_budget_usd = 20)

# 3. Read the outputs
res <- parse_outputs("outputs/run_log.csv")
res$tables$rob2
quotes <- check_quotes(res$quotes, function(id) file.path(dev_dir, "fulltexts", "rob2", paste0(id, ".pdf")))
```

Benchmark runs (`set = "benchmark"`) are refused until the prompts are marked `status: frozen` and every manual text has been checked (`scripts/mark_checked.R`).

Setup: copy `config/paths.example.yaml` to `config/paths.yaml` and set your folders. Put API keys in `~/.Renviron` (`ANTHROPIC_API_KEY`, `OPENAI_API_KEY`, `GEMINI_API_KEY`), never in the repo. Make the manual texts with `Rscript scripts/extract_manuals.R <manuals folder>`, check each against its PDF, then record it with `scripts/mark_checked.R`.

Tests: `Rscript -e 'testthat::test_dir("tests/testthat")'`

## Layout

```
R/          requests, calls, batch runner, inputs, specs, cost estimate
parse/      raw outputs -> assessment tables; quotation check
prompts/    system prompt, one prompt per instrument, specs (question IDs, allowed values)
config/     models (IDs, settings, prices) and local paths (paths.yaml, not committed)
manuals/    manifest only (citation, edition, URL, hashes); documents not committed
scripts/    manual text extraction and check recording
tests/      offline tests
concept/    the original concept notebook (tag v0.1-concept)
```

## Not committed

API keys, study PDFs, manuals, raw outputs, run logs, local paths.
