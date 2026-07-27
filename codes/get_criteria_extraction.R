# SocEnRep - Build the Part B (criteria extraction) datasets
#
# Reads each coder's coding workbook (sheets Part_B1 and Part_B2), cleans them,
# and writes six files:
#
#   dataset/b1_criteria_extraction.csv   Amelia, all 145 papers
#   dataset/b2_criteria_extraction.csv   Amelia, all 145 papers
#   dataset/b1_shared_subsample_IRR.csv  Amelia + Gunther, 15 IRR papers
#   dataset/b2_shared_subsample_IRR.csv  Amelia + Gunther, 15 IRR papers
#   dataset/criteria_codebook.csv        field reference (B1 + B2)
#   dataset/criteria_codebook_values.csv observed values for the controlled-
#                                         vocabulary fields, with counts
#
# Unlike Part A, Part B has no Google Form to read question text and field
# types off of, so the codebook is built by hand here from the PART B section
# of protocol_logbook.docx, and cross-checked against what coders actually
# entered (criteria_codebook_values.csv) rather than assumed static.
#
# On IRR for Part B: unlike Part A, this is extraction rather than rating, so the
# coders do not produce a fixed set of units. Rows only line up once you decide
# what a unit is - paper x operationalization category for B1, paper x criterion
# label for B2 - and coders may disagree by extracting a criterion at all, which
# is itself part of the reliability question. These files are the input to that
# alignment step, not a coder x unit matrix ready for kripp.alpha().
#
# Prerequisites:
#   install.packages(c("readr", "readxl", "dplyr", "tidyr", "stringr", "here"))
#
# Input:  coding/criteria extraction/<coder>/<coder>_coding-sheet_criteria_extraction.xlsx
#         coding/criteria extraction/Gunther/sample.xlsx  (the 15 IRR papers)
#         output/search_output/10_final_set.xlsx          (paper titles)
#
# Output: dataset/*.csv
#         output/cleaning_log/*.csv   (data-quality flags, not results)
#
# last modified: 27.07.2026

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(here)

# ===========================================================================
# 0. CONFIGURATION
# ===========================================================================

CODING_DIR <- here("coding", "criteria extraction")

# The workbook a coder's rows come from is what identifies them - the free-text
# "coder name" column inside the sheet is only cross-checked against this.
CODER_FILES <- c(
  Amelia  = file.path(CODING_DIR, "Amelia",  "Amelia_coding-sheet_criteria_extraction.xlsx"),
  Gunther = file.path(CODING_DIR, "Gunther", "Gunther_coding-sheet_criteria_extraction.xlsx")
)

FULL_CORPUS_CODER <- "Amelia"   # the coder whose files cover all 145 papers
N_CORPUS <- 145

# The sample of record for the criteria IRR: the sheet the second coder works
# from, not the raw draw in output/search_output/11_irr_sample_criteria.csv.
IRR_SAMPLE_FILE <- file.path(CODING_DIR, "Gunther", "sample.xlsx")

B1_COLUMNS <- c("paper ID", "coder name", "operationalization category",
                "operationalization value", "text evidence", "notes")

B2_COLUMNS <- c("paper ID", "coder name", "criterion category", "criterion label",
                "new criterion label", "normative framing", "text evidence",
                "is prerequisite for", "depends on", "can be automated",
                "LLM involved", "notes")

DATA_DIR <- here("dataset")
LOG_DIR  <- here("output", "cleaning_log")
dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LOG_DIR,  recursive = TRUE, showWarnings = FALSE)

save_log <- function(x, name) {
  write_excel_csv(x, file.path(LOG_DIR, paste0(name, ".csv")), na = "")
  invisible(x)
}

report <- function(x, title) {
  cat("\n", strrep("-", 70), "\n", title, "\n", strrep("-", 70), "\n", sep = "")
  print(as.data.frame(x), row.names = FALSE, right = FALSE)
  invisible(x)
}

norm_title <- function(x) str_replace_all(str_to_lower(x), "[^a-z0-9]", "")

# Text evidence and notes are verbatim quotes - keep their internal line breaks.
VERBATIM <- c("text evidence", "notes")

# ===========================================================================
# 1. READ
# ===========================================================================

# Everything as text: these are categorical codes and verbatim quotes, and a
# header-only sheet would otherwise come back with logical columns that refuse
# to bind with a coded coder's character columns.
read_part <- function(path, sheet, expected) {
  if (!file.exists(path)) {
    stop("Coding workbook not found: ", path, call. = FALSE)
  }
  if (!sheet %in% excel_sheets(path)) {
    stop("Sheet '", sheet, "' not found in ", basename(path),
         ". Sheets present: ", paste(excel_sheets(path), collapse = ", "),
         call. = FALSE)
  }

  d <- read_excel(path, sheet = sheet, col_types = "text", .name_repair = "minimal")

  missing <- setdiff(expected, names(d))
  if (length(missing) > 0) {
    stop("Sheet '", sheet, "' in ", basename(path), " is missing column(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  d |>
    select(all_of(expected)) |>
    # Kept so the flag tables can point at a row in the workbook (+1 for header).
    mutate(sheet_row = row_number() + 1L) |>
    # Excel keeps formatted-but-empty rows below the data; drop anything blank.
    filter(if_any(-sheet_row, \(x) !is.na(x) & nzchar(str_trim(x))))
}

read_coder <- function(coder, path) {
  list(
    b1 = read_part(path, "Part_B1", B1_COLUMNS) |> mutate(coder = coder, .before = 1),
    b2 = read_part(path, "Part_B2", B2_COLUMNS) |> mutate(coder = coder, .before = 1)
  )
}

cat("\n", strrep("=", 70), "\n",
    "SocEnRep Part B - build the criteria extraction datasets\n",
    "Run: ", format(Sys.time(), "%d.%m.%Y %H:%M"), "\n",
    strrep("=", 70), "\n", sep = "")

parts <- Map(read_coder, names(CODER_FILES), CODER_FILES)

b1_raw <- bind_rows(lapply(parts, `[[`, "b1"))
b2_raw <- bind_rows(lapply(parts, `[[`, "b2"))

read_summary <- tibble(
  coder = names(parts),
  b1_rows = vapply(parts, \(p) nrow(p$b1), integer(1)),
  b2_rows = vapply(parts, \(p) nrow(p$b2), integer(1))
)
report(read_summary, "ROWS READ PER CODER")

empty_coders <- read_summary$coder[read_summary$b1_rows == 0 & read_summary$b2_rows == 0]
if (length(empty_coders) > 0) {
  cat("\nNOTE: no coding yet from ", paste(empty_coders, collapse = ", "),
      " - the IRR files hold the other coder(s) only. Re-run once they have coded.\n",
      sep = "")
}

# ===========================================================================
# 2. CLEAN
# ===========================================================================

corpus <- read_excel(here("output", "search_output", "10_final_set.xlsx"),
                     sheet = "Literature") |>
  filter(!is.na(`No.`)) |>
  transmute(paper_id  = as.integer(`No.`),
            paper_title = Title,
            title_key = norm_title(Title))

clean_part <- function(d, part) {
  if (nrow(d) == 0) {
    return(d |> mutate(paper_id = integer(0), paper_title = character(0)) |>
             select(-`paper ID`, -`coder name`))
  }

  out <- d |>
    mutate(across(where(is.character), \(x) {
      x[!is.na(x) & !nzchar(str_trim(x))] <- NA_character_
      x
    })) |>
    mutate(across(-any_of(VERBATIM) & where(is.character), \(x) str_squish(x))) |>
    mutate(across(any_of(VERBATIM), \(x) str_trim(x))) |>
    mutate(paper_id = suppressWarnings(as.integer(`paper ID`)), .before = 1)

  bad_id <- out |>
    filter(is.na(paper_id) | !paper_id %in% corpus$paper_id) |>
    count(coder, `paper ID`, name = "rows")
  if (nrow(bad_id) > 0) {
    cat("\nWARNING (", part, "): ", sum(bad_id$rows),
        " row(s) carry a paper ID that is not in 10_final_set.xlsx\n", sep = "")
    save_log(bad_id, paste0("flag_", part, "_unknown_paper_id"))
  }

  # The free-text "coder name" column is only a cross-check; the workbook the
  # rows came from is authoritative.
  mismatch <- out |>
    filter(!is.na(`coder name`),
           !str_detect(`coder name`, regex(coder, ignore_case = TRUE))) |>
    count(coder, `coder name`, name = "rows")
  if (nrow(mismatch) > 0) {
    cat("\nWARNING (", part, "): the 'coder name' column disagrees with the ",
        "workbook it came from - see the cleaning log\n", sep = "")
    save_log(mismatch, paste0("flag_", part, "_coder_name_mismatch"))
  }

  out |>
    select(-`paper ID`, -`coder name`) |>
    left_join(corpus |> select(paper_id, paper_title), by = "paper_id") |>
    relocate(paper_id, coder, paper_title) |>
    arrange(paper_id, coder)
}

b1 <- clean_part(b1_raw, "b1")
b2 <- clean_part(b2_raw, "b2")

# Individual B2 cells left blank in the criterion category column - the rest of
# the row is coded. Where the label maps to exactly one category everywhere else,
# the intended value is recoverable, so the flag names it and points at the
# workbook row to fix. Excludes "new criterion", which spans all three by design.
category_lookup <- b2 |>
  filter(!is.na(`criterion category`), `criterion label` != "new criterion") |>
  distinct(`criterion label`, `criterion category`) |>
  add_count(`criterion label`, name = "n_categories") |>
  filter(n_categories == 1) |>
  select(`criterion label`, recoverable_category = `criterion category`)

blank_cat <- b2 |>
  filter(is.na(`criterion category`)) |>
  left_join(category_lookup, by = "criterion label") |>
  transmute(coder, sheet = "Part_B2", sheet_row, paper_id,
            `criterion label`, `new criterion label`, recoverable_category)
if (nrow(blank_cat) > 0) {
  cat("\nB2 cells with a blank criterion category: ", nrow(blank_cat),
      " (the rest of each row is coded)\n", sep = "")
  print(as.data.frame(blank_cat |> select(-`new criterion label`)),
        row.names = FALSE, right = FALSE)
}
save_log(blank_cat, "flag_b2_blank_category")

# ===========================================================================
# 3. CODEBOOK
# ===========================================================================

# Field reference. Descriptions paraphrase the PART B coding scheme in
# protocol_logbook.docx. field_type: "categorical" = controlled vocabulary,
# "free_text" = verbatim/coder-written, "identifier" = join key, "derived" =
# not read from the workbook.
codebook <- tribble(
  ~part, ~variable,                     ~field_type,   ~description,
  "B1",  "paper_id",                    "identifier",  "No. in 10_final_set.xlsx, matched from the sheet's paper ID column",
  "B1",  "coder",                       "identifier",  "Coder the row came from (the workbook it was read from, not the free-text 'coder name' column)",
  "B1",  "paper_title",                 "derived",     "Joined from 10_final_set.xlsx",
  "B1",  "operationalization category", "categorical", "Which of the four B1 indicators this row describes: Classification, Unit, Coverage, or Statistical criterion",
  "B1",  "operationalization value",    "categorical", "Coded value for the category above. Classification: Binary/Ordinal/Continuum/Not specified. Unit: Paper-level/Claim-level/Not specified. Coverage: All claims/Only focal claim/Fixed n of claims/Sampled/Not specified. Statistical criterion is free text instead (see criteria_codebook_values.csv, which excludes it).",
  "B1",  "text evidence",               "free_text",   "Verbatim quote supporting the code",
  "B1",  "notes",                       "free_text",   "Coder's own notes",
  "B2",  "paper_id",                    "identifier",  "No. in 10_final_set.xlsx, matched from the sheet's paper ID column",
  "B2",  "coder",                       "identifier",  "Coder the row came from (the workbook it was read from, not the free-text 'coder name' column)",
  "B2",  "paper_title",                 "derived",     "Joined from 10_final_set.xlsx",
  "B2",  "criterion category",          "categorical", "Reproducibility dimension the criterion belongs to: Availability, Executability, or Consistency",
  "B2",  "criterion label",             "categorical", "The specific criterion invoked (e.g. Data deposit, Code deposit, Path resolution). 'new criterion' flags a label not in the controlled list; see new criterion label",
  "B2",  "new criterion label",         "free_text",   "Coder-written label when criterion label = 'new criterion'",
  "B2",  "normative framing",           "categorical", "How the criterion is stated: Injunctive (rule/requirement), Descriptive (observed common practice), Tacit (inferred, never stated explicitly), or Unclear",
  "B2",  "text evidence",               "free_text",   "Verbatim quote supporting the code",
  "B2",  "is prerequisite for",         "free_text",   "Criterion label(s) this one is a prerequisite for (semicolon-separated); feeds the dependency DAG",
  "B2",  "depends on",                  "free_text",   "Criterion label(s) this one depends on (semicolon-separated); feeds the dependency DAG",
  "B2",  "can be automated",            "categorical", "Per the paper: yes (automated), no (should be manual), or unclear",
  "B2",  "LLM involved",                "categorical", "Whether an LLM is involved in the automation; coded only when can be automated = yes",
  "B2",  "notes",                       "free_text",   "Coder's own notes"
)

# Observed values for the controlled-vocabulary fields, computed from the
# cleaned data (all coders) rather than hardcoded, so drift from the protocol's
# list is visible without re-reading protocol_logbook.docx.
observed_values <- function(d, part, variable) {
  d |>
    filter(!is.na(.data[[variable]])) |>
    group_by(value = .data[[variable]]) |>
    summarise(n_rows = n(), n_papers = n_distinct(paper_id), .groups = "drop") |>
    mutate(part = part, variable = variable, .before = 1) |>
    arrange(desc(n_rows))
}

# "operationalization value" mixes controlled values (Classification/Unit/
# Coverage) with free text (Statistical criterion) - exclude the latter so the
# free-text tail doesn't swamp the controlled-vocabulary counts.
b1_controlled_value <- b1 |> filter(`operationalization category` != "Statistical criterion")

codebook_values <- bind_rows(
  observed_values(b1, "B1", "operationalization category"),
  observed_values(b1_controlled_value, "B1", "operationalization value"),
  observed_values(b2, "B2", "criterion category"),
  observed_values(b2, "B2", "criterion label"),
  observed_values(b2, "B2", "normative framing"),
  observed_values(b2, "B2", "can be automated"),
  observed_values(b2, "B2", "LLM involved")
)

write_excel_csv(codebook, file.path(DATA_DIR, "criteria_codebook.csv"), na = "")
write_excel_csv(codebook_values, file.path(DATA_DIR, "criteria_codebook_values.csv"), na = "")

# ===========================================================================
# 4. SPLIT
# ===========================================================================

# --- the full-corpus coder's own files -------------------------------------
b1_full <- b1 |> filter(coder == FULL_CORPUS_CODER)
b2_full <- b2 |> filter(coder == FULL_CORPUS_CODER)

# --- the 15-paper IRR sample -----------------------------------------------
# The sample of record is the sheet the second coder actually works from, not
# the raw draw in 11_irr_sample_criteria.csv - the two can diverge once the
# assignments are made (they did for the descriptive sample). Matched on title
# rather than on `No.`, because 10_final_set.xlsx was renumbered after the
# samples were drawn.
irr_sample <- read_excel(IRR_SAMPLE_FILE, sheet = 1, col_types = "text",
                         .name_repair = "minimal") |>
  filter(!is.na(`Paper ID`)) |>
  transmute(sample_no = as.integer(`Paper ID`),
            sample_title = Title,
            title_key = norm_title(Title)) |>
  left_join(corpus, by = "title_key")

if (any(is.na(irr_sample$paper_id))) {
  stop(sum(is.na(irr_sample$paper_id)), " paper(s) in ", basename(IRR_SAMPLE_FILE),
       " do not match a title in 10_final_set.xlsx: ",
       paste(irr_sample$sample_title[is.na(irr_sample$paper_id)], collapse = " | "),
       call. = FALSE)
}

renumbered <- irr_sample |>
  filter(sample_no != paper_id) |>
  select(sample_no, current_paper_id = paper_id, paper_title)
if (nrow(renumbered) > 0) {
  report(renumbered, "IRR SAMPLE ROWS WHOSE `No.` NO LONGER MATCHES THE CORPUS (matched by title)")
}
save_log(renumbered, "flag_criteria_irr_sample_renumbered")

b1_irr <- b1 |> filter(paper_id %in% irr_sample$paper_id)
b2_irr <- b2 |> filter(paper_id %in% irr_sample$paper_id)

# Which of the 15 papers each coder has actually produced rows for. A paper with
# no rows from a coder is ambiguous - either not yet coded, or coded and found to
# yield nothing - so it is listed rather than silently treated as agreement.
coverage <- expand_grid(paper_id = irr_sample$paper_id, coder = names(CODER_FILES)) |>
  left_join(count(b1_irr, paper_id, coder, name = "b1_rows"), by = c("paper_id", "coder")) |>
  left_join(count(b2_irr, paper_id, coder, name = "b2_rows"), by = c("paper_id", "coder")) |>
  mutate(across(c(b1_rows, b2_rows), \(x) replace_na(x, 0L))) |>
  arrange(paper_id, match(coder, names(CODER_FILES)))

report(coverage |>
         pivot_wider(names_from = coder, values_from = c(b1_rows, b2_rows),
                     names_glue = "{coder}_{.value}"),
       "IRR SAMPLE COVERAGE (rows per paper per coder)")
save_log(coverage, "flag_criteria_irr_coverage")

# ===========================================================================
# 5. WRITE
# ===========================================================================

# sheet_row only exists so the flag tables can point back at the workbook.
write_dataset <- function(d, name) {
  write_excel_csv(select(d, -any_of("sheet_row")), file.path(DATA_DIR, name), na = "")
}

write_dataset(b1_full, "b1_criteria_extraction.csv")
write_dataset(b2_full, "b2_criteria_extraction.csv")
write_dataset(b1_irr,  "b1_shared_subsample_IRR.csv")
write_dataset(b2_irr,  "b2_shared_subsample_IRR.csv")

cat("\n", strrep("=", 70), "\n",
    "b1_criteria_extraction.csv: ", nrow(b1_full), " rows across ",
    n_distinct(b1_full$paper_id), " papers\n",
    "b2_criteria_extraction.csv: ", nrow(b2_full), " rows across ",
    n_distinct(b2_full$paper_id), " papers\n",
    "b1_shared_subsample_IRR.csv      : ", nrow(b1_irr), " rows | coders: ",
    paste(sort(unique(b1_irr$coder)), collapse = ", "), "\n",
    "b2_shared_subsample_IRR.csv      : ", nrow(b2_irr), " rows | coders: ",
    paste(sort(unique(b2_irr$coder)), collapse = ", "), "\n",
    "criteria_codebook.csv            : ", nrow(codebook), " fields\n",
    "criteria_codebook_values.csv     : ", nrow(codebook_values), " value rows across ",
    n_distinct(codebook_values$variable), " fields\n",
    "Cleaning log                     : ", LOG_DIR, "\n",
    strrep("=", 70), "\n", sep = "")

if (n_distinct(b1_full$paper_id) > N_CORPUS || n_distinct(b2_full$paper_id) > N_CORPUS) {
  warning("More distinct papers than the ", N_CORPUS, "-paper corpus.", call. = FALSE)
}
