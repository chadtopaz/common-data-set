library(tidyverse)

set.seed(123)

infile  <- "data_raw/manual/b2_xlsx_manual_extraction.csv"
outfile <- "data_intermediate/qa/b2_xlsx_sample.csv"

stopifnot(file.exists(infile))
dir.create("data_intermediate/qa", showWarnings = FALSE, recursive = TRUE)

data <- read_csv(infile, show_col_types = FALSE)
sample_size <- ceiling(0.10 * nrow(data))

sampled_data <- data %>% sample_n(sample_size)

write_csv(sampled_data, outfile)
cat("10% sample saved to:", outfile, "\n")