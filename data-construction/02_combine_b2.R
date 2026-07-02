library(readr)
library(dplyr)

pdf_file  <- "data_intermediate/b2_pdf_clean.csv"
xlsx_file <- "data_intermediate/b2_xlsx_clean.csv"

pdf_data  <- read_csv(pdf_file)
xlsx_data <- read_csv(xlsx_file)

combined_data <- bind_rows(pdf_data, xlsx_data)

write_csv(combined_data, "data_intermediate/b2_combined.csv")

cat(sprintf(
  "Combined %d PDF rows and %d XLSX rows into %d total rows\n",
  nrow(pdf_data), nrow(xlsx_data), nrow(combined_data)
))
