# Manuals

The official guidance and assessment forms, sent with each call as PDF files, unchanged. The PDFs are not committed (licences differ; RoB 2 guidance is CC BY-NC-ND). `manifest.csv` lists each document's citation, edition, source URL and SHA-256; the runner refuses a PDF whose hash differs.

Why PDF and not extracted text: in the extracted text, tables lost their structure (PROBAST summary tables, RoB 2 mapping tables, parts of the ROBINS-I tool), algorithm figures were lost (RoB 2, ROBINS-E) and the NOS star symbol was garbled. See the project decision log.

| Instrument | Documents |
|---|---|
| RoB 2 | Full guidance (22 Aug 2019) + cribsheet |
| ROBINS-I | Detailed guidance (2016) + tool (2016) |
| ROBINS-E | Full version (24 Mar 2024) |
| NOS | Scale + coding manual (OHRI) |
| PROBAST | Wolff et al. 2019 (tool) + Moons et al. 2019 (explanation and elaboration) |

Put the PDFs, named as in `pdf_file`, in the folder set as `manuals_dir` in `config/paths.yaml`.
