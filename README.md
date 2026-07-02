# Admissions Priorities and Campus Diversity: Insights from the Common Data Set

Replication package (code and processed data) for the paper *Admissions Priorities and
Campus Diversity: Insights from the Common Data Set* (under review).

The study assembles a cross-institutional panel from Common Data Set (CDS) reports and
links institutions' stated admissions priorities to first-year enrollment composition at
highly selective U.S. colleges and universities over 2010–2022. The headline result:
institutions that report considering alumni relation ("legacy status") enroll first-year
shares of underrepresented racial and ethnic minority (URM) students about 6–8 percentage
points lower than institutions that do not, net of sector and other reported priorities —
a descriptive cross-institutional association, not a causal effect.

All analysis is in **R**.

## Repository structure

```
.
├── data/                         # processed, analysis-ready data
│   ├── Final_2010_2022.csv       # institution–year analytic panel (2010–2022)  <- analysis input
│   ├── cds_merged_all_years.csv  # merged B2 + C7, all reported years (reference)
│   └── covariates.csv            # hand-coded institutional covariates (see codebook below)
├── R/                            # the paper's analysis
│   ├── 05_analysis.R             # Tables 1–3, Figure 1, appendix tables/figures
│   └── 06_confounding_and_robustness.R  # covariate sensitivity (Table 4), Figure 2, diagnostics
├── data-construction/            # how data/ was built from raw CDS reports (see its README)
│   ├── extract_b2_pdf/           # GPT-4o + OCR extraction of Table B2 from PDFs
│   ├── extract_b2_xlsx/          # extraction of Table B2 from Excel files
│   ├── 02_combine_b2.R
│   ├── 03_merge_b2_c7.R
│   └── 04_restrict_years.R
├── prompts/                      # generative-AI prompt used in data construction
│   └── table_b2_extraction_gpt4o.md
├── output/                       # created by the analysis scripts (empty in the repo)
├── LICENSE
└── README.md
```

## Reproducing the analysis

**Requirements.** R (≥ 4.1) with:

```r
install.packages(c("tidyverse", "janitor", "scales", "sandwich", "plm", "psych"))
```

**Steps.** From the repository root:

```r
setwd("/path/to/common-data-set")
source("R/05_analysis.R")                    # main results + figures + appendix
source("R/06_confounding_and_robustness.R")  # covariate sensitivity + Figure 2 + diagnostics
```

Both scripts read only from `data/` and write all results to `output/` (created
automatically). No internet access or API key is needed to reproduce the analysis.

**What gets produced (in `output/`):**

- `tables/table1_descriptives.csv`, `tables/table2_main_results.csv`, `tables/table3_robustness.csv`
- `figures/fig1_urm_by_legacy.{pdf,png}`
- `appendix/tableA1…A6*.csv`, `appendix/figA1_scatter.{pdf,png}`, `appendix/figA2_leave_one_out.{pdf,png}`
- `key_numbers_for_text.csv`
- `R_fig_urm_trend_3group.{pdf,png}` (Figure 2) and `R_*.csv` (confounding sensitivity, pre-COVID,
  ordinal, sample flow, PCA loadings/eigenvalues, panel diagnostics)

The primary estimate reproduces to the reported precision: legacy −7.81 pp (95% CI
[−15.57, −0.06], p = 0.048, R² = 0.35) in the primary sample (n = 36); −6.47 pp in the
balanced 13-year panel (n = 27).

## Data

**`data/Final_2010_2022.csv`** — the analytic panel: one row per institution–year, with
institution identifiers, public/private status, CDS Table B2 first-year enrollment counts
by race/ethnicity, and CDS Table C7 admissions-priority ratings (four-point ordinal scale:
Very Important / Important / Considered / Not Considered). This is the only file the
analysis scripts read.

**`data/covariates.csv`** — institutional covariates hand-coded by the authors from public
sources, keyed to the exact `school_name` strings in the panel. Codebook:

| Column | Meaning |
|---|---|
| `school_name` | institution (matches the analytic panel) |
| `sector` | Public or Private |
| `state`, `region` | U.S. state; Census region |
| `flagship` | 1 = the single primary public flagship of its state (leading public research campus); 0 otherwise. Secondary/land-grant campuses (e.g., Purdue, Texas A&M, NC State) = 0. Flagship specifications are exploratory. |
| `aa_restriction` | **Primary** measure: 1 = public institution in a state with a *formal* statutory/constitutional/administrative restriction on race-conscious admissions during the study period — California (Prop 209), Michigan (Proposal 2), Washington (I-200), Florida (One Florida). Privates = 0. |
| `ga_court_based` | 1 = Georgia institutions, whose restriction was court-based (*Johnson v. Board of Regents*, 11th Cir. 2001) rather than statutory; excluded from the primary `aa_restriction` and carried separately. |
| `aa_restriction_incl_ga` | sensitivity variant = `max(aa_restriction, ga_court_based)`. |
| `aa_note` | free-text source/rationale for the coding. |

**`data/cds_merged_all_years.csv`** — the merged Table B2 + Table C7 data across all
reported years (1997–98 through 2023–24), before restriction to the 2010–2022 analytic
window. Included for reference; not required by the analysis.

**Raw data (not included).** The underlying raw CDS reports (institutional PDFs and Excel
files) are **not** distributed here due to size. They are public documents posted on the
institutions' own admissions / institutional-research pages. The `data-construction/`
scripts document exactly how the raw reports were turned into the processed files above.

## Data construction

The `data-construction/` scripts show the full pipeline from raw CDS reports to
`data/Final_2010_2022.csv`. They require the raw report files (not included) and, for the
PDF extraction step, an OpenAI API key in the `OPENAI_API_KEY` environment variable, so
they are provided for transparency rather than turn-key re-execution.

**Generative-AI use.** Table B2 ("Enrollment by Racial/Ethnic Category") was extracted
from PDFs using the OpenAI GPT-4o API (temperature 0), with Tesseract OCR as a fallback for
image-only PDFs. The exact prompt is in `prompts/table_b2_extraction_gpt4o.md`. Table C7
was transcribed manually. All extracted values were validated by manual audit against the
source documents.

## Citation

If you use this code or data, please cite the paper (citation to be added on publication).

## License

Code is released under the MIT License (see `LICENSE`). The processed data files may be
redistributed with attribution; the underlying CDS reports remain the property of the
reporting institutions.

## Contact

Chad M. Topaz — cmt6@williams.edu
