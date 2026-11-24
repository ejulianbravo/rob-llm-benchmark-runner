# Changelog

All changes to the runner are recorded here. Versions are git tags.

## Unreleased

- Moved the concept notebook and its files into `concept/`.
- Added the runner layout (`R/`, `config/`, `prompts/`, `manuals/`, `parse/`), README and `.gitignore`.
- Call functions (`R/`): one request builder per provider (Anthropic Messages API, OpenAI Responses API, Google generateContent); PDF sent as a file; manuals and instrument prompt placed first so providers can cache them; raw response saved unchanged and never overwritten; run log with model returned, tokens, cache use, timing and hashes of PDF, manuals, prompt and target.
- `config/models.yaml` with provisional model IDs (to be fixed after the cost test).
- Offline tests (`tests/testthat/`) with mocked responses.
- `CLAUDE.md` with working rules and design summary.
- Batch runner (`R/run_batch.R`): plans every call still to make with a rough cost estimate; sends nothing unless `dry_run = FALSE` and an approved budget covers the estimate; refuses benchmark runs until prompts are frozen and manual texts checked; resumes by skipping calls already stored; run 1 completes before run 2.
- Inputs (`R/inputs.R`): frozen studies table, PDF hash check, manuals from the manifest with text-hash check, prompts with version and status.
- Instrument specs (`prompts/specs/`) and draft prompts (`prompts/`, version 0.1, status draft).
- Manuals manifest with citations, editions, URLs and PDF hashes; scripts to extract and record checked texts.
- Parser (`parse/parse_outputs.R`): one table per instrument in the schema's layout plus a quotations table; values kept as written, out-of-range values flagged; unreadable answers kept as rows.
- Quotation check (`parse/check_quotes.R`) against text extracted from the same PDF.
- Changes after review: HTTP error bodies go to `outputs/errors/` and do not use up the run number; token columns have one definition across providers, with the provider's usage record kept; Google prompt blocks recorded as finish reason; settings fixed in code recorded in the log; cache breakpoint moved after the instrument prompt; a hash per manual; raw file names include the model ID; lock per call; dry runs need no key; request size checked before sending; timeout default 1800 s.
- NOS: the model states the choices the scale leaves to reviewers (comparability factors, follow-up length, adequate follow-up) in `review_choices`; the source reviews' own definitions are not sent.
- PDF text now extracted in reading order with poppler's pdftotext (`pdf_pages_text()`), for the manuals and the quotation check; pdftools kept the page layout and mixed the lines of two-column pages.
- Manuals are now sent as the official PDF files, hash-checked against the manifest, instead of extracted text: in the text, tables lost their structure, algorithm figures were lost and the NOS star symbol was garbled. Manifest reduced to PDF columns; the text extraction and check scripts removed. Cost estimate counts manual pages.
- Model settings checked against provider documentation (2026-11-23): output limits raised (64K Claude and GPT, 32,768 Gemini non-streaming); reasoning level set to high for all three (proposal); no temperature sent. All three models have about 1M tokens of context; Anthropic allows up to 600 PDF pages per request at that context size.
- `scripts/cost_test.R`: plans the cost test (one development study per instrument, all models, run 1), sends only with `--run` (or, in RStudio, `options(cost_test.run = TRUE)`) and within US$30, then reports tokens, timing, parse status and cost from returned tokens.
- Anthropic thinking tokens now read from `output_tokens_details.thinking_tokens` (seen in the cost test).
- Fixed: named manual vectors were serialised as a JSON object instead of an array.

## v0.1-concept (9 Dec 2025)

- Concept notebook: RoB 2 for cluster-randomised trials in R with `httr2`; text extracted with `pdftools`; one extraction call plus one call per domain (OpenAI).
