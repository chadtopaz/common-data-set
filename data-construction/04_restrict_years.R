library(tidyverse)

# ============================================================================
# BUILD FINAL ANALYSIS DATASET (2010–2022)
# Reads the already-merged all-years dataset, filters years, writes ONE output.
# ============================================================================

infile  <- "data_final/cds_merged_all_years.csv"
outfile <- "data_final/Final_2010_2022.csv"

stopifnot(file.exists(infile))

data <- read_csv(infile, show_col_types = FALSE) %>%
  filter(year >= 2010, year <= 2022)

write_csv(data, outfile)
cat("Wrote:", outfile, " | rows:", nrow(data), " cols:", ncol(data), "\n")