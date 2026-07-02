# Load required libraries
library(tidyverse)

# Read data from Google Sheets (no authentication needed for public sheets)
B2 <- read_csv("data_intermediate/b2_combined.csv")
C7 <- read_csv("data_intermediate/c7_clean.csv")

# Process C7 data
C7 <- C7 %>%
  separate(year, into = c("year_start", "year_end"), sep = "_") %>%
  mutate(year = as.numeric(year_start)) %>%
  select(-year_start, -year_end) %>%
  pivot_wider(names_from = admission_priority,
              values_from = numerical_rating,
              id_cols = c(file_name, forbes_ranking, school_name, public_private, year)) %>%
  arrange(year, forbes_ranking)

# Process B2 data
B2 <- B2 %>%
  mutate(file_name = str_extract(file_name, "\\d{2}[-_]\\d{2}[-_]\\d{2}")) %>%
  select(-raceEthnicity, -race_clean) %>%
  group_by(file_name, year, std_race) %>%
  summarise(
    degreeSeekingFirstYear = sum(degreeSeekingFirstYear, na.rm = TRUE),
    degreeSeekingAll = sum(degreeSeekingAll, na.rm = TRUE),
    allStudents = sum(allStudents, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = std_race,
    values_from = c(degreeSeekingFirstYear, degreeSeekingAll, allStudents),
    values_fill = 0
  ) %>%
  arrange(year, file_name)

# ---- Defensibility check: merge keys must be unique ----
stopifnot(
  B2 %>% count(file_name, year) %>% filter(n > 1) %>% nrow() == 0,
  C7 %>% count(file_name, year) %>% filter(n > 1) %>% nrow() == 0
)


# Merge datasets
merged_sheet <- inner_join(B2, C7, by = c("file_name", "year"))

# Save merged data to CSV
write_csv(merged_sheet, "data_final/cds_merged_all_years.csv")

# Print confirmation message
cat("Merged dataset successfully saved as 'Final_Demographics_Merged.csv'\n")
cat("Dimensions:", nrow(merged_sheet), "rows ×", ncol(merged_sheet), "columns\n")