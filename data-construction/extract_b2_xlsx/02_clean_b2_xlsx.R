###############################################################################
# Load required libraries
###############################################################################
library(tidyverse)
library(stringr)

###############################################################################
# Step 1: Load raw data and extract 4-digit academic year
###############################################################################
df <- read_csv("data_raw/manual/b2_xlsx_manual_extraction.csv") %>%
  mutate(
    # Extract 2-digit year from file_name using lookbehind/lookahead regex
    year_two_digit = str_extract(file_name, "(?<=[_-])\\d{2}(?=[_-])") %>% as.numeric(),
    # Convert 2-digit year to 4-digit format: assume >50 => 1900s, else 2000s
    year = if_else(year_two_digit > 50, 1900 + year_two_digit, 2000 + year_two_digit)
  ) %>%
  select(-year_two_digit)  # Remove temporary 2-digit year column

###############################################################################
# Step 2: Define function to standardize race labels
###############################################################################
standardize_race <- function(df_year) {
  df_year %>%
    mutate(
      # Clean up the original raceEthnicity string
      race_clean = raceEthnicity %>%
        str_remove_all("\\s*\\(.*?\\)") %>%    # Remove any parentheticals
        str_squish() %>%                       # Trim/collapse whitespace
        str_to_lower() %>%
        # Remove references to "non-hispanic" (with hyphen, space, etc.)
        str_replace_all("non[- ]?hispanic(?:/latino)?", "") %>%
        str_replace_all(",\\s*$", "") %>%      # Remove trailing commas
        str_squish()
    ) %>%
    mutate(
      # Map to standardized categories
      std_race = case_when(
        race_clean == "asian or pacific islander" ~ "AAPI",
        str_detect(race_clean, "native hawaiian|pacific islander") ~ "NHPI",
        str_detect(race_clean, "asian") ~ "Asian",
        str_detect(race_clean, "black|african") ~ "Black",
        str_detect(race_clean, "american indian|alaska native|alaskan native") ~ "AIAN",
        str_detect(race_clean, "hispanic|latino|latinx") ~ "Latinx",
        str_detect(race_clean, "^white") ~ "white",
        str_detect(race_clean, "multi|two or more|bi/multi") ~ "multi",
        str_detect(race_clean, "unknown|other") ~ "Unknown",
        str_detect(race_clean, "international|non[- ]?resident|nonresident|aliens") ~ "intl",
        TRUE ~ race_clean
      )
    )
}

###############################################################################
# Step 3: Apply standardization per academic year
###############################################################################
df_final <- df %>%
  group_by(year) %>%
  group_modify(~ standardize_race(.x)) %>%
  ungroup()

###############################################################################
# Step 3a: Remove empty files, fill missing numeric cells w/ zero,
#          then remove files that are entirely zeros.
###############################################################################

race_cols <- c("degreeSeekingFirstYear", "degreeSeekingAll", "allStudents")

# 3a(i). Drop entire files that contain only NA in all numeric columns 
#        for all the *physically present* rows (i.e., not "Added for consistency").
df_final <- df_final %>%
  group_by(file_name) %>%
  filter(
    # If among the "physically present" rows (raceEthnicity != "Added for consistency"),
    # all of race_cols are NA => drop this file
    !all(
      if_else(
        raceEthnicity != "Added for consistency",
        rowSums(is.na(across(all_of(race_cols)))) == length(race_cols),
        FALSE
      )
    )
  ) %>%
  ungroup()

# 3a(ii). For any row that physically appears in the table, fill NA with 0.
df_final <- df_final %>%
  mutate(
    across(
      all_of(race_cols),
      ~ if_else(raceEthnicity != "Added for consistency" & is.na(.x), 0, .x)
    )
  )

# 3a(iii). Drop entire file if all physically present rows are zeros.
#          That is, if for all rows where raceEthnicity != "Added for consistency",
#          the sum across race_cols is 0 for every row.
df_final <- df_final %>%
  group_by(file_name) %>%
  filter(
    !all(
      if_else(
        raceEthnicity != "Added for consistency",
        rowSums(across(all_of(race_cols))) == 0, 
        FALSE
      )
    )
  ) %>%
  ungroup()

###############################################################################
# Step 4–5: Identify modal (most common) race sets by year
###############################################################################
race_sets <- df_final %>%
  group_by(year, file_name) %>%
  summarise(
    race_list = list(sort(unique(std_race))),
    .groups = "drop"
  ) %>%
  mutate(
    race_key = map_chr(race_list, ~ paste(.x, collapse = ";"))
  )

modal_keys <- race_sets %>%
  count(year, race_key) %>%
  group_by(year) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%  # Top frequency race set per year
  select(year, race_key)

modal_sets <- race_sets %>%
  semi_join(modal_keys, by = c("year", "race_key")) %>%
  select(year, modal_race_list = race_list) %>%
  distinct()

###############################################################################
# Step 6: Prepare modal set lookup and merge with full data
###############################################################################
modal_lookup <- modal_sets %>%
  mutate(races_expected = map(modal_race_list, as.character)) %>%
  select(year, races_expected)

df_final <- df_final %>%
  left_join(modal_lookup, by = "year")

###############################################################################
# Step 6a: If modal set has only AAPI, unify AAPI/NHPI/Asian into AAPI
###############################################################################
df_final <- df_final %>%
  group_by(file_name, year) %>%
  group_modify(~ {
    chunk <- .x
    races_expected <- chunk$races_expected[[1]]
    
    hasAAPI_mod  <- "AAPI"  %in% races_expected
    hasNHPI_mod  <- "NHPI"  %in% races_expected
    hasAsian_mod <- "Asian" %in% races_expected
    
    # If the modal set is strictly AAPI (no separate NHPI or Asian), unify them:
    if (hasAAPI_mod && !hasNHPI_mod && !hasAsian_mod) {
      races_to_combine <- c("AAPI", "NHPI", "Asian")
      present <- chunk %>% filter(std_race %in% races_to_combine)
      
      if (nrow(present) > 1) {
        # Combine numeric columns
        combined <- present %>%
          summarise(
            across(c(degreeSeekingFirstYear, degreeSeekingAll, allStudents),
                   ~ sum(.x, na.rm = TRUE))
          )
        
        if ("AAPI" %in% present$std_race) {
          # Overwrite the existing AAPI row with the sum
          chunk <- chunk %>%
            mutate(across(
              c(degreeSeekingFirstYear, degreeSeekingAll, allStudents),
              ~ if_else(std_race == "AAPI", combined[[cur_column()]], .x)
            )) %>%
            filter(!std_race %in% c("NHPI", "Asian"))
        } else {
          # Convert either NHPI or Asian into AAPI
          source <- if ("NHPI" %in% present$std_race) "NHPI" else "Asian"
          chunk <- chunk %>%
            mutate(
              std_race = if_else(std_race == source, "AAPI", std_race)
            ) %>%
            mutate(across(
              c(degreeSeekingFirstYear, degreeSeekingAll, allStudents),
              ~ if_else(std_race == "AAPI", combined[[cur_column()]], .x)
            )) %>%
            filter(!(std_race %in% races_to_combine & std_race != "AAPI"))
        }
      }
    }
    chunk
  }) %>%
  ungroup()

###############################################################################
# Step 6b: If modal set lacks AAPI, rename any AAPI to Asian
###############################################################################
df_final <- df_final %>%
  group_by(file_name, year) %>%
  group_modify(~ {
    chunk <- .x
    races_expected <- chunk$races_expected[[1]]
    
    if (!("AAPI" %in% races_expected) && any(chunk$std_race == "AAPI")) {
      chunk <- chunk %>%
        mutate(std_race = if_else(std_race == "AAPI", "Asian", std_race))
    }
    chunk
  }) %>%
  ungroup() %>%
  select(-races_expected)

###############################################################################
# Step 7: Add zero-filled rows for races missing from the modal set
###############################################################################
modal_expanded <- modal_sets %>%
  mutate(modal_race = map(modal_race_list, ~ tibble(std_race = .x))) %>%
  select(year, modal_race) %>%
  unnest(modal_race)

file_years <- df %>%
  distinct(file_name, year)

modal_expected_rows <- file_years %>%
  left_join(modal_expanded, by = "year", relationship = "many-to-many")

# Build unique key to match existing rows
existing_keys <- df_final %>%
  transmute(key = paste(year, file_name, std_race, sep = "::"))

# Any modal row not in df_final yet => add as zero-filled placeholders
modal_expected_rows <- modal_expected_rows %>%
  mutate(key = paste(year, file_name, std_race, sep = "::")) %>%
  filter(!key %in% existing_keys$key) %>%
  mutate(
    raceEthnicity          = "Added for consistency",
    race_clean             = NA_character_,
    degreeSeekingFirstYear = 0,
    degreeSeekingAll       = 0,
    allStudents            = 0,
    races_in_file          = list(NULL)
  ) %>%
  select(names(df_final))  # align column order

df_final <- bind_rows(df_final, modal_expected_rows)

# Drop files that are entirely zero-filled placeholders
df_final <- df_final %>%
  group_by(file_name) %>%
  filter(
    !all(
      raceEthnicity == "Added for consistency" &
        degreeSeekingFirstYear == 0 &
        degreeSeekingAll == 0 &
        allStudents == 0
    )
  ) %>%
  ungroup()

###############################################################################
# Step 8: Remove all-zero rows not in the modal race set
###############################################################################
modal_race_lookup <- modal_sets %>%
  mutate(modal_races = map(modal_race_list, as.character)) %>%
  select(year, modal_races)

df_final <- df_final %>%
  left_join(modal_race_lookup, by = "year") %>%
  rowwise() %>%
  mutate(
    is_extraneous = !std_race %in% modal_races,
    is_all_zero   = (degreeSeekingFirstYear == 0 & degreeSeekingAll == 0 & allStudents == 0),
    remove_me     = (is_extraneous & is_all_zero)
  ) %>%
  ungroup() %>%
  filter(!remove_me) %>%
  select(-modal_races, -is_extraneous, -is_all_zero, -remove_me)

###############################################################################
# Step 9: Rebuild race sets after cleaning
###############################################################################
race_sets <- df_final %>%
  group_by(year, file_name) %>%
  summarise(
    race_list = list(sort(unique(std_race))),
    .groups = "drop"
  ) %>%
  mutate(race_key = map_chr(race_list, ~ paste(.x, collapse = ";")))

###############################################################################
# Step 10: Create and attach comparison log + special notes
###############################################################################
comparison_log <- race_sets %>%
  left_join(modal_sets, by = "year") %>%
  mutate(
    missing = map2(modal_race_list, race_list, function(x, y) setdiff(x, y)),
    extraneous = map2(modal_race_list, race_list, function(x, y) setdiff(y, x))
  ) %>%
  mutate(
    missing_str = map_chr(missing, function(x) if (length(x) == 0) "—" else paste(x, collapse = "; ")),
    extraneous_str = map_chr(extraneous, function(x) if (length(x) == 0) "—" else paste(x, collapse = "; "))
  ) %>%
  mutate(
    note = case_when(
      file_name %in% c("27_00_01.pdf", "27_01_02.pdf") ~
        "White includes Unknown; do not compare categories",
      year %in% c(2002, 2006, 2008, 2009) & str_detect(extraneous_str, "multi") ~
        "Multiracial category not standard for this year; possibly institution-specific",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(missing_str != "—" | extraneous_str != "—" | !is.na(note)) %>%
  select(year, file_name, missing = missing_str, extraneous = extraneous_str, note)

# Attach special notes to df_final
df_final <- df_final %>%
  left_join(
    comparison_log %>%
      select(year, file_name, note) %>%
      distinct(),
    by = c("year", "file_name")
  ) %>%
  rename(issue_note = note)

###############################################################################
# Final outputs
#   - df_final       => cleaned dataset with consistent race labels & issue notes
#   - comparison_log => details of categories missing/extraneous for each file
###############################################################################
write_csv(df_final, "data_intermediate/b2_xlsx_clean.csv")