# rob-llm-benchmark-runner

R code that runs large language models on the LLM risk-of-bias benchmark (150 studies; RoB 2, ROBINS-I, ROBINS-E, NOS, PROBAST) and turns their outputs into tables for analysis.

Status: in development. Nothing here has been run on benchmark studies.

## What one call contains

One request per study, model and run:

- the study report as a PDF file (the frozen benchmark copy);
- the instrument's full official guidance and its official assessment form, as checked text;
- the assessment target from the frozen benchmark;
- one prompt per instrument (task, target, output format). No worked examples, no retrieval.

The model returns one JSON object: for each domain, supporting quotations with page numbers, then signalling-question answers, then the domain judgement; then the model's overall judgement (not for NOS).

## What is stored

- Every raw response, unchanged, before any parsing.
- A run log row per call: study ID, model ID as returned by the API, prompt version, run number, settings, start and end time, token counts, status, error.

Failed or unreadable outputs are logged and left as they are. Nothing is repaired or re-asked.

## Layout

```
R/          request builders (Anthropic, OpenAI, Google), call and logging functions
config/     model IDs and settings
prompts/    one versioned prompt per instrument
manuals/    manifest of the official documents (citation, edition, URL, SHA-256); the documents themselves are not committed
parse/      raw responses -> assessment tables
concept/    the original concept notebook (tag v0.1-concept)
```

## Not committed

API keys (read from the environment), study PDFs, manuals, raw outputs and run outputs. See `.gitignore`.

## Origin

Starts from the concept notebook in `concept/` (tag `v0.1-concept`), which assessed cluster-randomised trials with RoB 2 using extracted text and one call per domain.
