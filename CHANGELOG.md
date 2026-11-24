# Changelog

All changes to the runner are recorded here. Versions are git tags.

## Unreleased

- Moved the concept notebook and its files into `concept/`.
- Added the runner layout (`R/`, `config/`, `prompts/`, `manuals/`, `parse/`), README and `.gitignore`.
- Call functions (`R/`): one request builder per provider (Anthropic Messages API, OpenAI Responses API, Google generateContent); PDF sent as a file; manuals and instrument prompt placed first so providers can cache them; raw response saved unchanged and never overwritten; run log with model returned, tokens, cache use, timing and hashes of PDF, manuals, prompt and target.
- `config/models.yaml` with provisional model IDs (to be fixed after the cost test).
- Offline tests (`tests/testthat/`) with mocked responses.
- `CLAUDE.md` with working rules and design summary.

## v0.1-concept (9 Dec 2025)

- Concept notebook: RoB 2 for cluster-randomised trials in R with `httr2`; text extracted with `pdftools`; one extraction call plus one call per domain (OpenAI).
