# Instructions for Claude in this repository

This repo runs large language models on the LLM risk-of-bias benchmark (150 studies, 30 each for RoB 2, ROBINS-I, ROBINS-E, NOS and PROBAST; published human assessments as the reference) and turns their outputs into tables for analysis in R. Edward Bravo leads the study. Design decisions are recorded in the project's decision log (outside this repo); the ones that govern this code are summarised below.

## Rules

1. **Authorship.** Commits are made under Edward's identity only (Julian Bravo, 52596818+ejulianbravo@users.noreply.github.com). No Claude author, no `Co-Authored-By`, no `Claude-Session` or other attribution lines, in commits, PR descriptions or files.
2. **No spending without approval.** Do not run any call that reaches a model API without Edward's explicit approval of that run and its estimated cost. Offline tests (mocked responses) are fine.
3. **API keys** are read from the environment (`ANTHROPIC_API_KEY`, `OPENAI_API_KEY`, `GEMINI_API_KEY`). Never print, log, write or commit them.
4. **Not committed:** study PDFs, manuals, raw model outputs, run logs (see `.gitignore`). Record their hashes instead.
5. **Raw outputs are sacred.** Save every response body unchanged before anything reads it. Never overwrite, repair, re-ask or regenerate. Failed or unreadable outputs are logged and kept as they are. Only transient HTTP failures (429, 5xx) are retried, before any output exists.
6. **No benchmark studies during development.** Prompts are developed and tuned only on studies outside the benchmark, then frozen with a version number before the first benchmark run.
7. **Changelog.** Every change goes in `CHANGELOG.md`. Releases are git tags.
8. **Write plainly** in code comments, README and messages. Do not overstate what the code or the models can do.

## Design (decision log D110–D139)

- R with `httr2`. Started from the concept notebook in `concept/` (tag `v0.1-concept`).
- One call per study, model and run. Two runs per study per model; run 1 is primary.
- Each call contains, in this order: system instructions; the instrument's full official guidance and official assessment form as the official PDF files (hash-checked); the instrument prompt (task and output format); the study PDF as a file; the assessment target from the frozen benchmark. No worked examples, no retrieval.
  - RoB 2: full guidance (22 Aug 2019) + cribsheet. ROBINS-I: detailed guidance (2016) + tool (2016). ROBINS-E: full version (24 Mar 2024). NOS: coding manual + scale (OHRI). PROBAST: Wolff et al. 2019 + Moons et al. 2019.
  - RoB 2 variant: individually randomised parallel-group trials.
- The instrument and target are fixed by the frozen benchmark. The model does not classify the design or choose the instrument.
- Output: one JSON object. For each domain: supporting quotations with page numbers first, then signalling-question answers, then the judgement. Then the model's own overall judgement (not for NOS). NOS is scored as stars per item.
- Analysis (separate R scripts): the model's overall is kept; a calculated overall is derived from its domains with the same rule as for the human reference. For RoB 2 and ROBINS-E, domain judgements are also derived from the model's own signalling answers with the official algorithm and compared with its direct judgements. Quotations are checked by script against text extracted from the same PDF.
- Models (provisional, fixed after the cost test): see `config/models.yaml`. Settings per model live in the config, not in code. Omit parameters a model does not accept.
- Budget cap €3,000 for all API costs; spend only what the pilot needs.

## Layout

`R/` call functions · `config/` models · `prompts/` instrument prompts · `manuals/` manifest only · `parse/` raw → tables · `tests/` offline tests · `concept/` original notebook.

Run tests: `Rscript -e 'testthat::test_dir("tests/testthat")'`

## Next steps

1. Edward reviews the runner and the draft prompts.
2. Settled since: see decision log D142, D143, D144.
3. Manual PDFs in `manuals_dir`, matching the manifest hashes.
4. Development studies outside the benchmark (`dev_dir`), for prompt work and the cost test.
5. Cost test on development PDFs, after Edward approves the spend; then replace the cost assumptions in `config/models.yaml` and fix model IDs.
6. Freeze prompts (`status: frozen`), register, then benchmark runs.
