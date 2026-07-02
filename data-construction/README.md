# Data construction

These scripts document how the processed files in `../data/` were built from the raw
Common Data Set (CDS) reports. They are provided for **transparency**, not turn-key
re-execution: they require the raw report files, which are not distributed in this
repository (they are public documents on the institutions' own websites), and the PDF
extraction step requires an OpenAI API key.

## Pipeline (raw reports → `data/Final_2010_2022.csv`)

1. **Extract Table B2 (enrollment by race/ethnicity).**
   - `extract_b2_pdf/01_extract_b2_pdf.R` — reads raw CDS PDFs, uses the PDF text layer
     (or Tesseract OCR for image-only PDFs), and calls the OpenAI GPT-4o API to parse
     Table B2 into structured rows. Requires `OPENAI_API_KEY` in the environment. The exact
     prompt is in `../prompts/table_b2_extraction_gpt4o.md`.
   - `extract_b2_pdf/02_clean_b2_pdf.R`, `03_validate_sample_b2_pdf.R` — clean and audit.
   - `extract_b2_xlsx/02_clean_b2_xlsx.R`, `03_validate_sample_b2_xlsx.R` — the Excel path
     (Table B2 copied manually into templates, then cleaned and audited).
   - Table C7 (admissions-priority ratings) was transcribed **manually** (not AI-extracted),
     because its layout varies too widely across institutions and years to standardize.

2. **`02_combine_b2.R`** — combine the PDF- and Excel-derived Table B2 data.

3. **`03_merge_b2_c7.R`** — merge Table B2 (enrollment) with Table C7 (priorities) into a
   single institution–year dataset across all reported years
   (→ `data/cds_merged_all_years.csv`).

4. **`04_restrict_years.R`** — restrict to the 2010–2022 analytic window
   (→ `data/Final_2010_2022.csv`), the input to the analysis in `../R/`.

## Note on paths

These scripts reference the original project layout (e.g., `data_raw/`, `data_intermediate/`)
and are preserved as-run for the record. To re-execute them you would supply the raw CDS
files and adjust the input/output paths to your environment.
