# ==============================#
#         LOAD LIBRARIES       #
# ==============================#
library(tidyverse)
library(pbmcapply)
library(furrr)
library(pdftools)
library(tesseract)
library(magick)
library(httr)
library(jsonlite)

# Set up parallel processing
plan(multisession, workers = max(1, parallel::detectCores() - 1))

# ==============================#
#        CONFIGURATION          #
# ==============================#
local_data_dir <- "data_raw/cds_pdfs"

# Temp working folder (kept, but treated as scratch)
download_dir   <- "downloaded_files"

# Outputs (canonical)
out_data_file  <- "data_intermediate/b2_pdf_raw.csv"
out_log_file   <- "data_intermediate/b2_extraction_log.csv"

# Debug (optional, but useful)
gpt_debug_dir  <- "data_intermediate/gpt_debug"

# 🔑 OpenAI API key (DO NOT hard-code; set in environment instead)
api_key <- Sys.getenv("OPENAI_API_KEY")
stopifnot(nzchar(api_key))

# Create needed directories
dir.create(download_dir, showWarnings = FALSE, recursive = TRUE)
dir.create("data_intermediate", showWarnings = FALSE, recursive = TRUE)
dir.create(gpt_debug_dir, showWarnings = FALSE, recursive = TRUE)

# ==============================#
#     LIST ALL LOCAL PDFs       #
#  Rule (per your convention):
#    - unsuffixed = full CDS (all tables) -> keep
#    - _B.pdf      = B-table-only          -> keep (prefer over full)
#    - _C.pdf      = C-table-only          -> exclude for B2 extraction
# ==============================#
get_all_local_pdfs <- function(root_dir) {
  all_files <- list.files(root_dir, pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE)
  
  pdf_files <- tibble(
    path = all_files,
    name = basename(all_files)
  ) %>%
    mutate(
      is_b_only = str_detect(name, "_B\\.pdf$"),
      is_c_only = str_detect(name, "_C\\.pdf$"),
      # key representing the "full" filename for that school-year
      base_key  = if_else(is_b_only | is_c_only,
                          str_replace(name, "_[BC]\\.pdf$", ".pdf"),
                          name)
    ) %>%
    # Exclude C-only files for B2 extraction
    filter(!is_c_only) %>%
    # Prefer _B over full when both exist
    group_by(base_key) %>%
    arrange(desc(is_b_only)) %>%
    slice(1) %>%
    ungroup() %>%
    select(path, name)
  
  pdf_files
}

cat("📂 Scanning local directory for PDFs...\n")
all_pdfs <- get_all_local_pdfs(local_data_dir)
cat("📁 Found", nrow(all_pdfs), "PDFs for B2 extraction (full + _B; excluding _C)\n")

# ==============================#
#      COPY TO DOWNLOAD DIR     #
# ==============================#
download_pdf <- function(file_row) {
  file_path <- file_row$path
  file_name <- file_row$name
  dest_path <- file.path(download_dir, file_name)
  
  tryCatch({
    file.copy(file_path, dest_path, overwrite = TRUE)
    tibble(file = file_name, status = "copied")
  }, error = function(e) {
    tibble(file = file_name, status = paste("copy_error:", e$message))
  })
}

cat("📥 Copying PDFs to processing folder...\n")
files_to_download <- split(all_pdfs, seq_len(nrow(all_pdfs)))
download_results <- pbmclapply(
  files_to_download,
  download_pdf,
  mc.cores = max(1, parallel::detectCores() - 1)
) %>% bind_rows()

# ==============================#
#        GLOBAL PROMPT          #
# ==============================#
global_prompt <- 'You are an expert table extraction assistant. Your task is to clean and reformat a table
from the Common Data Set (CDS), specifically Table B2: "Enrollment by Racial/Ethnic Category".

IMPORTANT:
- The label "B2" may or may not appear in the PDF. If it does appear, the table might start on that page, continue to the next, or begin on the following page.
- Some PDFs might have no text layer (so they require OCR), or only partial text layers. Regardless, you must identify the correct table by looking at the content (rows with race/ethnicity categories and numeric columns for enrollment).
- There is a small chance the file may not contain Table B2 at all. If you cannot find a table matching the B2 structure, you must return empty or partial results (do not guess or invent data).
- After locating the correct table, first extract the raw text exactly as it appears (without altering or making up numbers). Then, strictly from that raw text, convert the table into structured data as explained below.

The table includes three numeric columns:
- Degree-seeking, First-time, First-year,
- Degree-seeking Undergraduates (include first-time, first-year),
- Total Undergraduates (both degree-seeking and non-degree-seeking).

Your output must be exactly one valid JSON object, of the form:
{
  "columns": ["raceEthnicity", "degreeSeekingFirstYear", "degreeSeekingAll", "allStudents"],
  "data": [
    {"raceEthnicity": "...", "degreeSeekingFirstYear": ..., "degreeSeekingAll": ..., "allStudents": ...},
    ...
  ]
}

Strict requirements:
- Include only race/ethnicity rows that are visibly shown in the table (do NOT guess or invent rows).
- Do NOT include total or subtotal rows (e.g., "Total," "Underrepresented Minorities").
- If a race/ethnicity label is split across multiple lines, combine it.
- Use null for missing or blank numeric values, EXCEPT in the edge cases below.
- Preserve numbers exactly as shown (no rounding or reformatting), EXCEPT in the edge cases below.
- Output ONLY the valid JSON object (no markdown, no extra text).

Edge Cases to Handle Carefully:
1) If a cell is empty or contains only a dash/hyphen, interpret that as 0.
2) If a cell only has a percentage (like "12%"), but you see a "Total" row at the bottom or elsewhere, multiply that percentage by the corresponding "Total" figure to get a count. Round to the nearest integer. Do NOT include that "Total" row in the final output.
3) If a cell shows both a count and a percentage, keep only the count.
4) If no total row is found to support the percentage, leave the cell as null (do not invent a total).'

# ==============================#
#    TEXT EXTRACTION FUNCTION   #
# ==============================#
extract_text_with_ocr_fallback <- function(file_path) {
  text <- tryCatch(pdf_text(file_path), error = function(e) "") %>% paste(collapse = "\n\n")
  if (nchar(text) > 100) return(list(text = text, ocr_used = FALSE))
  
  message("⚠️ No usable text layer found — using OCR with custom PSM...")
  npages <- tryCatch(pdf_info(file_path)$pages, error = function(e) 0)
  if (npages == 0) return(list(text = "", ocr_used = TRUE))
  
  eng <- tesseract("eng", options = list(tessedit_pageseg_mode = 4))
  ocr_text <- map_chr(seq_len(npages), function(i) {
    tryCatch({
      img <- image_read_pdf(file_path, density = 400, pages = i) %>%
        image_convert(colorspace = "gray") %>%
        image_threshold(type = "white", threshold = "80%") %>%
        image_deskew(threshold = 40)
      ocr(img, engine = eng)
    }, error = function(e) "")
  })
  list(text = paste(ocr_text, collapse = "\n\n"), ocr_used = TRUE)
}

# ==============================#
#   PARSE JSON RETURNED BY GPT  #
# ==============================#
parse_gpt_json <- function(json_str, file_name, used_ocr) {
  dat <- fromJSON(json_str)
  stopifnot(is.list(dat), "columns" %in% names(dat), "data" %in% names(dat))
  out <- as_tibble(dat$data) %>% mutate(file_name = file_name)
  list(data = out, log = tibble(file = file_name, status = "success", used_ocr = used_ocr))
}

# ==============================#
#  CALL GPT & PARSE TABLE B2    #
# ==============================#
extract_table_b2_gpt_from_pdf <- function(file_path, api_key, global_prompt) {
  file_name <- basename(file_path)
  cat("📄 Processing:", file_name, "\n")
  Sys.sleep(runif(1, 0.2, 0.5))  # Random delay
  
  text_info <- extract_text_with_ocr_fallback(file_path)
  full_text <- text_info$text
  used_ocr  <- text_info$ocr_used
  
  if (nchar(full_text) < 100) {
    return(list(data = NULL, log = tibble(file = file_name, status = "no_text_found", used_ocr = used_ocr)))
  }
  
  messages <- list(
    list(role = "system", content = global_prompt),
    list(role = "user", content = paste0(
      "Here is the full text of the PDF. Please locate Table B2 (as described above), ",
      "extract its raw text exactly as it appears, and then convert that text into the required JSON format.\n\n",
      full_text
    ))
  )
  
  res <- NULL
  max_retries <- 5
  for (attempt in 1:max_retries) {
    res <- tryCatch({
      POST(
        url = "https://api.openai.com/v1/chat/completions",
        add_headers(Authorization = paste("Bearer", api_key), `Content-Type` = "application/json"),
        body = toJSON(list(model = "gpt-4o", temperature = 0, messages = messages), auto_unbox = TRUE),
        timeout(30)
      )
    }, error = function(e) NULL)
    
    status <- if (!is.null(res)) status_code(res) else 0
    if (!is.null(res) && status >= 200 && status < 300) break
    if (attempt < max_retries) {
      wait_time <- 2 ^ (attempt - 1)
      message("🔁 GPT retry in ", wait_time, "s (attempt ", attempt, ")")
      Sys.sleep(wait_time)
    }
  }
  
  if (is.null(res)) return(list(data = NULL, log = tibble(file = file_name, status = "gpt_failed", used_ocr = used_ocr)))
  
  parsed <- tryCatch(content(res, as = "parsed"), error = function(e) NULL)
  if (is.null(parsed) || !is.list(parsed$choices) || length(parsed$choices) == 0) {
    return(list(data = NULL, log = tibble(file = file_name, status = paste0("gpt_http_", status_code(res)), used_ocr = used_ocr)))
  }
  
  gpt_output_raw <- parsed$choices[[1]]$message$content
  writeLines(gpt_output_raw, file.path(gpt_debug_dir, paste0(tools::file_path_sans_ext(file_name), "_raw.txt")))
  
  cleaned_json <- gpt_output_raw %>%
    str_replace_all("(?s)^.*?(?=\\{)", "") %>%
    str_replace_all("```json|```", "") %>%
    str_trim() %>%
    gsub('("(degreeSeekingFirstYear|degreeSeekingAll|allStudents)"\\s*:\\s*)([0-9]{1,3}),([0-9]{3})',
         "\\1\\3\\4", .)
  
  tryCatch({
    parse_gpt_json(cleaned_json, file_name, used_ocr)
  }, error = function(e) {
    list(data = NULL, log = tibble(file = file_name, status = "json_parse_failed", used_ocr = used_ocr))
  })
}

# ==============================#
#          MAIN LOOP            #
# ==============================#
pdf_files <- list.files(download_dir, pattern = "\\.pdf$", full.names = TRUE)

process_pdf_file <- function(file_path) {
  tryCatch({
    extract_table_b2_gpt_from_pdf(file_path, api_key, global_prompt)
  }, error = function(e) {
    file_name <- basename(file_path)
    list(data = NULL, log = tibble(file = file_name, status = "unknown_error", used_ocr = NA))
  })
}

cat("🧠 Starting GPT-based Table B2 extraction...\n")
results_raw <- future_map(
  pdf_files,
  ~ process_pdf_file(.x),
  .progress = TRUE,
  .options = furrr_options(seed = TRUE, scheduling = 1)
)

# ==============================#
#         RETRY FAILURES        #
# ==============================#
results_list <- map(results_raw, "data")
log_list     <- map_dfr(results_raw, "log")

failed <- which(map_lgl(results_list, is.null))
if (length(failed) > 0) {
  retry_results <- future_map(
    failed,
    ~ process_pdf_file(pdf_files[.x]),
    .progress = TRUE
  )
  for (i in seq_along(failed)) {
    results_list[[failed[i]]] <- retry_results[[i]]$data
    log_list <- bind_rows(log_list, retry_results[[i]]$log)
  }
}

# ==============================#
#         WRITE OUTPUT          #
# ==============================#
final_data <- bind_rows(compact(results_list))
final_logs <- log_list

write_csv(final_data, out_data_file)
write_csv(final_logs, out_log_file)

cat("✅ Extraction complete. Data written to ", out_data_file, ". Logs in ", out_log_file, "\n", sep = "")