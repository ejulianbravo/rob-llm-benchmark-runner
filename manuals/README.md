# Manuals

The official guidance and assessment forms, sent with each call as PDF files, unchanged. The PDFs are not committed (licences differ; RoB 2 guidance is CC BY-NC-ND). `manifest.csv` lists each document's citation, edition, source URL and SHA-256; the runner refuses a PDF whose hash differs.

Why PDF and not extracted text: the extracted text was unusable, and we want to keep fidelity to the original documents rather than change them or write our own version of a manual or its rules into prompts. In the extracted text, tables lost their structure (PROBAST summary tables, RoB 2 mapping tables, parts of the ROBINS-I tool), algorithm figures were lost (RoB 2, ROBINS-E) and the NOS star symbol was garbled. See the project decision log.

| Instrument | Documents |
|---|---|
| RoB 2 | Full guidance (22 Aug 2019) + cribsheet |
| ROBINS-I | Detailed guidance (updated 20 Oct 2016) + tool (version 1 Aug 2016). Not ROBINS-I V2 (2025). |
| ROBINS-E | Full version (24 Mar 2024) |
| NOS | Scale + coding manual (OHRI) |
| PROBAST | Wolff et al. 2019 (tool) + Moons et al. 2019 (explanation and elaboration) |

Put the PDFs, named as in `pdf_file`, in the folder set as `manuals_dir` in `config/paths.yaml`.
