# SocEnRep - Download and clean the Part A (descriptive) coding responses
#
# Pulls the live Google Form responses, cleans them, and splits them into the
# two analysis files:
#
#   dataset/descriptive.csv                     all coders, all submissions
#   dataset/descriptive_coding.csv       Amelia only, full 145-paper corpus
#   dataset/descriptive_shared_subsample_IRR.csv  45 IRR sample papers x 4 coders
#   dataset/descriptive_codebook.csv            variable code -> question text
#
# The IRR file is a complete 45 x 4 grid: papers a coder never submitted appear
# as rows with NA in every coding field and `submitted = FALSE`. Krippendorff's
# alpha handles missing values, so nothing is dropped for uneven coverage. To
# feed it to irr::kripp.alpha(), reshape one variable at a time:
#
#   irr |> select(paper_id, coder, B1) |>
#     pivot_wider(names_from = paper_id, values_from = B1) |>
#     column_to_rownames("coder") |> as.matrix() |> kripp.alpha(method = "nominal")
#
# The responses sheet is link-shared ("anyone with the link can view"), so the
# CSV export endpoint works without authentication. If link sharing is ever
# turned off, set USE_GOOGLESHEETS4 <- TRUE below and authenticate once.
#
# Free-text "Other" answers are NOT recoded here - harmonising them is a coding
# decision, not a cleaning step. descriptive_analysis_coding.R lists them in its
# `*_flag_*` tables.
#
# Prerequisites:
#   install.packages(c("readr", "readxl", "dplyr", "tidyr", "stringr", "here"))
#   optional: install.packages("googlesheets4")
#
# Input:  Google Sheet "Coding Litreview SocEnrep (Responses)"
#         output/search_output/10_final_set.xlsx          (paper IDs, DOIs)
#         coding/descriptive/sample_descriptive.xlsx      (the 45 IRR papers,
#                                                          one sheet per RA)
#
# Output: dataset/*.csv
#         output/cleaning_log/*.csv   (data-quality flags, not results)
#
# last modified: 26.07.2026

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(here)

# ===========================================================================
# 0. CONFIGURATION
# ===========================================================================

SHEET_ID <- "1rq4P2yBNVkCeBErIB9zXXE4ejfzI7uh2JXo5lhnl4Jc"

# TRUE routes the download through googlesheets4 (needs a one-off OAuth login).
# Only needed if the sheet stops being link-shared.
USE_GOOGLESHEETS4 <- FALSE

# The responses carry the coders' work e-mail addresses. This repo is public,
# so they are left out of the exported files; the `coder` column identifies who
# coded what. The address is still used internally to resolve coder-name
# variants before it is dropped.
DROP_EMAIL <- TRUE

# Duplicate submissions for the same coder x paper: "last" keeps the most
# recent timestamp (a correction), "first" keeps the original.
KEEP_SUBMISSION <- "last"

N_CORPUS <- 145
CODERS   <- c("Amelia", "Ineke", "Mehtab", "Elisabetta")

# The sample of record for the shared subsample: the assignment workbook the RAs
# actually worked from, one sheet per RA (sheet name = coder). NOT the raw draw
# in 11_irr_sample_descriptive.csv - the two differ, see section 5.
IRR_SAMPLE_FILE <- here("coding", "descriptive", "sample_descriptive.xlsx")
IRR_SHEET_SKIP  <- 1   # row 1 is a banner ("<coder> - Articles to Review")

# Fallback for harmonising the free-text "Coder name" field.
CODER_EMAILS <- c(
  "amelia.zein@cais-research.de"          = "Amelia",
  "ineke-levin.mlynarek@cais-research.de" = "Ineke",
  "mehtab.shah@gesis.org"                 = "Mehtab",
  "elisabetta.divito@rwi-essen.de"        = "Elisabetta"
)

DATA_DIR <- here("dataset")
LOG_DIR  <- here("output", "cleaning_log")
dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LOG_DIR,  recursive = TRUE, showWarnings = FALSE)

# Free-text fields keep their internal line breaks; everything else is squished.
TEXT_FIELDS <- c("D1A", "notes")

# Non-question columns, mapped by their exact Google Forms header.
META_COLUMNS <- c(
  "Timestamp"     = "timestamp",
  "Email Address" = "email",
  "DOI/Link"      = "doi_entered",
  "Paper title"   = "paper_title",
  "Coder name"    = "coder"
)

# Field types for the codebook. Kept in sync with descriptive_analysis_coding.R,
# which treats the multi-select fields as comma-joined checkbox lists.
FIELD_TYPES <- c(
  B1 = "single_select", B1A = "multi_select",
  C1 = "single_select", C2  = "single_select",
  D1 = "single_select", D1A = "free_text",
  D2 = "single_select", D2A = "multi_select",
  E1 = "single_select", E1A = "single_select", E1B = "multi_select",
  E1C = "multi_select", E1D = "multi_select", E1E = "single_select",
  E1F = "single_select",
  F1 = "single_select", F2  = "multi_select", F3 = "multi_select"
)

save_log <- function(x, name) {
  write_excel_csv(x, file.path(LOG_DIR, paste0(name, ".csv")), na = "")
  invisible(x)
}

# ===========================================================================
# 1. HELPERS
# ===========================================================================

# Matching key for papers. DOIs are typed by hand and come in several shapes
# (bare DOI, https://doi.org/..., a publisher URL, in one case an OpenAlex ID),
# so titles are the reliable join key and the DOI is only cross-checked.
norm_title <- function(x) str_replace_all(str_to_lower(x), "[^a-z0-9]", "")

norm_doi <- function(x) {
  x <- str_squish(str_to_lower(x))
  x <- str_remove(x, "^https?://(dx\\.)?doi\\.org/")
  x <- str_remove(x, "^doi:\\s*")
  str_remove(x, "/+$")
}

blank_to_na <- function(x) {
  x <- as.character(x)
  x[!is.na(x) & !nzchar(str_trim(x))] <- NA_character_
  x
}

# Google Forms writes M/D/YYYY H:M:S in this sheet's locale. Falls back to the
# variant without seconds and warns rather than silently producing NA.
parse_timestamp <- function(x) {
  ts <- as.POSIXct(x, format = "%m/%d/%Y %H:%M:%S", tz = "Europe/Berlin")
  gap <- is.na(ts) & !is.na(x)
  if (any(gap)) {
    ts[gap] <- as.POSIXct(x[gap], format = "%m/%d/%Y %H:%M", tz = "Europe/Berlin")
  }
  gap <- is.na(ts) & !is.na(x)
  if (any(gap)) {
    warning(sum(gap), " timestamp(s) could not be parsed; ",
            "check the sheet's locale (expected M/D/YYYY H:M:S). ",
            "Duplicate resolution falls back to sheet row order for those rows.",
            call. = FALSE)
  }
  ts
}

# "B1a. Which discipline(s) ..." -> "B1A"; the codebook code is the column name.
code_from_question <- function(nm) {
  code <- str_match(nm, "^([A-Fa-f][0-9]+[a-z]?)\\.")[, 2]
  ifelse(is.na(code), NA_character_, str_to_upper(code))
}

# ===========================================================================
# 2. DOWNLOAD
# ===========================================================================

fetch_responses <- function(sheet_id) {
  if (USE_GOOGLESHEETS4) {
    if (!requireNamespace("googlesheets4", quietly = TRUE)) {
      stop("USE_GOOGLESHEETS4 is TRUE but the googlesheets4 package is not installed.",
           call. = FALSE)
    }
    return(googlesheets4::read_sheet(sheet_id, col_types = "c"))
  }

  url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id, "/export?format=csv")
  tmp <- tempfile(fileext = ".csv")
  ok <- tryCatch({
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(e) {
    message("Download failed: ", conditionMessage(e))
    FALSE
  })
  if (!ok || !file.exists(tmp) || file.size(tmp) == 0) {
    stop("Could not download the responses sheet. Check the network connection, ",
         "or set USE_GOOGLESHEETS4 <- TRUE.", call. = FALSE)
  }

  # Google answers with an HTML sign-in page (still HTTP 200) when the sheet is
  # not readable by the link, which would otherwise parse as a nonsense CSV.
  if (str_detect(readLines(tmp, n = 1, warn = FALSE), "^\\s*<")) {
    stop("The sheet is not publicly readable - Google returned a sign-in page. ",
         "Either re-enable link sharing, or set USE_GOOGLESHEETS4 <- TRUE.",
         call. = FALSE)
  }

  # Everything as character: these are categorical codes and verbatim quotes,
  # and type guessing would mangle DOIs and single-value columns.
  read_csv(tmp, col_types = cols(.default = col_character()),
           na = character(), progress = FALSE)
}

cat("\n", strrep("=", 70), "\n",
    "SocEnRep Part A - download and clean descriptive coding responses\n",
    "Run: ", format(Sys.time(), "%d.%m.%Y %H:%M"), "\n",
    strrep("=", 70), "\n", sep = "")

raw <- fetch_responses(SHEET_ID)
cat("\nDownloaded ", nrow(raw), " submissions x ", ncol(raw), " columns\n", sep = "")

# ===========================================================================
# 3. RENAME COLUMNS AND BUILD THE CODEBOOK
# ===========================================================================

questions <- names(raw)
new_names <- unname(META_COLUMNS[questions])              # meta columns first
codes     <- code_from_question(questions)                # then B1, B1A, ...
new_names <- coalesce(new_names, codes)

# The free-text catch-all field has no code and no fixed header wording.
new_names[is.na(new_names) &
            str_detect(questions, regex("^please put any additional",
                                        ignore_case = TRUE))] <- "notes"

if (any(is.na(new_names))) {
  warning("Unrecognised column(s) in the responses sheet, kept under a ",
          "snake_case name: ", paste(questions[is.na(new_names)], collapse = " | "),
          call. = FALSE)
  fallback <- str_replace_all(str_to_lower(str_squish(questions)), "[^a-z0-9]+", "_")
  new_names[is.na(new_names)] <- str_remove_all(fallback[is.na(new_names)], "^_|_$")
}

names(raw) <- new_names

codebook <- tibble(
  variable  = new_names,
  question  = str_squish(questions),
  field_type = coalesce(unname(FIELD_TYPES[new_names]),
                        if_else(new_names %in% c("notes", "D1A"), "free_text", "identifier"))
) |>
  # The coder-entered DOI is replaced by the corpus DOI during cleaning, so it
  # is not a column of the exported files; document what it became instead.
  filter(variable != "doi_entered", !(DROP_EMAIL & variable == "email")) |>
  bind_rows(tribble(
    ~variable,   ~question,                                                    ~field_type,
    "paper_id",  "Derived: `No.` in 10_final_set.xlsx, matched on paper title", "derived",
    "doi",       "Derived: DOI from 10_final_set.xlsx (the coder-entered DOI/Link is only cross-checked)", "derived",
    "submitted", "Derived (IRR file only): FALSE where the coder never submitted this paper", "derived"
  ))

# ===========================================================================
# 4. CLEAN
# ===========================================================================

coding_fields <- intersect(names(FIELD_TYPES), names(raw))

clean <- raw |>
  mutate(across(everything(), blank_to_na)) |>
  # Squish the categorical fields; free text keeps its paragraph breaks.
  mutate(across(all_of(setdiff(names(raw), TEXT_FIELDS)), \(x) str_squish(x))) |>
  mutate(across(any_of(TEXT_FIELDS), \(x) str_trim(x))) |>
  mutate(
    row_in_sheet = row_number(),
    email        = str_to_lower(email),
    timestamp    = parse_timestamp(timestamp),
    title_key    = norm_title(paper_title),
    doi_entered  = norm_doi(doi_entered)
  )

# --- coder names -----------------------------------------------------------
# The form asked for the coder name as free text, so it arrives with variants
# ("Elisabetta Di Vito" vs "Elisabetta"). Resolve against the canonical list,
# falling back to the submitting e-mail address.
coder_from_name <- function(x) {
  hit <- vapply(x, function(one) {
    if (is.na(one)) return(NA_character_)
    m <- CODERS[str_detect(one, regex(paste0("^", CODERS), ignore_case = TRUE))]
    if (length(m) == 1) m else NA_character_
  }, character(1), USE.NAMES = FALSE)
  hit
}

clean <- clean |>
  mutate(
    coder_raw = coder,
    coder     = coalesce(coder_from_name(coder_raw), unname(CODER_EMAILS[email]))
  )

coder_variants <- clean |>
  filter(coder_raw != coder | is.na(coder)) |>
  count(coder_raw, email, resolved_to = coder, name = "submissions") |>
  # The cleaning log is committed too, so it follows the same rule.
  select(!any_of(if (DROP_EMAIL) "email" else character()))
if (nrow(coder_variants) > 0) {
  cat("\nCoder-name variants harmonised:\n")
  print(as.data.frame(coder_variants), row.names = FALSE, right = FALSE)
}
# Flags are written unconditionally: one that is only saved when it has rows
# goes stale on disk once the underlying issue is fixed.
save_log(coder_variants, "flag_coder_name_variants")
if (any(is.na(clean$coder))) {
  stop(sum(is.na(clean$coder)), " submission(s) could not be assigned to a coder. ",
       "Add the name or e-mail to CODERS / CODER_EMAILS.", call. = FALSE)
}

# --- join the corpus to get paper IDs and canonical DOIs -------------------
corpus <- read_excel(here("output", "search_output", "10_final_set.xlsx"),
                     sheet = "Literature") |>
  filter(!is.na(`No.`)) |>
  transmute(paper_id  = as.integer(`No.`),
            title_key = norm_title(Title),
            paper_title_corpus = Title,
            doi       = norm_doi(DOI))

clean <- clean |> left_join(corpus, by = "title_key")

unmatched <- clean |>
  filter(is.na(paper_id)) |>
  select(row_in_sheet, coder, paper_title, doi_entered)
if (nrow(unmatched) > 0) {
  cat("\nWARNING: ", nrow(unmatched), " submission(s) do not match any title in ",
      "10_final_set.xlsx - they carry no paper_id.\n", sep = "")
}
save_log(unmatched, "flag_unmatched_papers")

# The DOI the coder typed is only a cross-check; the corpus DOI is authoritative.
doi_mismatch <- clean |>
  filter(!is.na(paper_id), !is.na(doi), !is.na(doi_entered), doi != doi_entered) |>
  count(paper_id, paper_title_corpus, doi_corpus = doi, doi_entered, name = "submissions")
if (nrow(doi_mismatch) > 0) {
  cat("\nDOI/Link entries that differ from the corpus DOI: ", nrow(doi_mismatch),
      " (corpus DOI kept; see the cleaning log)\n", sep = "")
}
save_log(doi_mismatch, "flag_doi_mismatch")

# Coder-entered titles differ in spelling here and there; use the corpus title
# so the same paper reads identically across coders.
clean <- clean |>
  mutate(paper_title = coalesce(paper_title_corpus, paper_title),
         doi         = coalesce(doi, doi_entered))

# --- duplicate submissions -------------------------------------------------
dup_keys <- clean |>
  count(coder, paper_id, name = "submissions") |>
  filter(submissions > 1)

dropped <- tibble()   # so the flag is written even when there is nothing to drop

if (nrow(dup_keys) > 0) {
  # Order by timestamp, falling back to sheet order where a timestamp is missing.
  ordered <- clean |>
    group_by(coder, paper_id) |>
    arrange(timestamp, row_in_sheet, .by_group = TRUE) |>
    mutate(submission_no = row_number(),
           n_submissions = n(),
           keep = if (KEEP_SUBMISSION == "last") {
             submission_no == n_submissions
           } else {
             submission_no == 1L
           }) |>
    ungroup()

  # Which fields actually changed between the submissions - usually the point
  # of the re-submission, and the thing worth eyeballing before trusting it.
  changed_fields <- ordered |>
    filter(n_submissions > 1) |>
    group_by(coder, paper_id) |>
    summarise(across(all_of(coding_fields), \(x) n_distinct(x) > 1),
              .groups = "drop") |>
    pivot_longer(all_of(coding_fields), names_to = "field", values_to = "differs") |>
    filter(differs) |>
    group_by(coder, paper_id) |>
    summarise(fields_differing = paste(field, collapse = "; "), .groups = "drop")

  dropped <- ordered |>
    filter(n_submissions > 1, !keep) |>
    left_join(changed_fields, by = c("coder", "paper_id")) |>
    mutate(fields_differing = replace_na(fields_differing, "(identical)")) |>
    select(coder, paper_id, paper_title, timestamp, submission_no,
           n_submissions, fields_differing, all_of(coding_fields))

  cat("\nDuplicate submissions: ", nrow(dropped), " row(s) dropped (kept the ",
      KEEP_SUBMISSION, " per coder x paper)\n", sep = "")
  print(as.data.frame(dropped |>
                        mutate(paper_title = str_trunc(paper_title, 45)) |>
                        select(coder, paper_id, paper_title, timestamp, fields_differing)),
        row.names = FALSE, right = FALSE)

  clean <- ordered |> filter(keep) |> select(-keep, -submission_no, -n_submissions)
}
save_log(dropped, "flag_duplicate_submissions")

# --- final column order ----------------------------------------------------
clean <- clean |>
  arrange(paper_id, match(coder, CODERS)) |>
  select(paper_id, coder, paper_title, doi, timestamp,
         any_of("email"), all_of(coding_fields), any_of("notes"))

if (DROP_EMAIL) clean <- clean |> select(-any_of("email"))

# ===========================================================================
# 5. SPLIT
# ===========================================================================

# --- Amelia's full-corpus coding (Part A input for the descriptive analysis)
amelia <- clean |> filter(coder == "Amelia")

# --- the shared IRR subsample ----------------------------------------------

# Papers are matched on title rather than on `No.`, because 10_final_set.xlsx
# was renumbered after the sample was drawn. Titles are stable.
read_assignments <- function(sheet) {
  d <- read_excel(IRR_SAMPLE_FILE, sheet = sheet, skip = IRR_SHEET_SKIP,
                  col_types = "text", .name_repair = "minimal")
  missing <- setdiff(c("No.", "Title"), names(d))
  if (length(missing) > 0) {
    stop("Sheet '", sheet, "' in ", basename(IRR_SAMPLE_FILE),
         " is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  d |>
    filter(!is.na(`No.`)) |>
    transmute(coder     = sheet,
              sheet_no  = as.integer(`No.`),
              title_key = norm_title(Title),
              # A ticked "Done" box is the RA's own claim that they coded it.
              marked_done = if ("Done" %in% names(d)) {
                !is.na(Done) & nzchar(str_trim(Done))
              } else NA)
}

assignments <- bind_rows(lapply(excel_sheets(IRR_SAMPLE_FILE), read_assignments)) |>
  left_join(corpus |> select(paper_id, title_key,
                             paper_title = paper_title_corpus, doi),
            by = "title_key")

if (any(is.na(assignments$paper_id))) {
  stop(sum(is.na(assignments$paper_id)), " assigned paper(s) in ",
       basename(IRR_SAMPLE_FILE), " do not match a title in 10_final_set.xlsx.",
       call. = FALSE)
}

irr_sample <- assignments |>
  distinct(paper_id, paper_title, doi) |>
  arrange(paper_id)

renumbered <- assignments |>
  filter(sheet_no != paper_id) |>
  distinct(sheet_no, current_paper_id = paper_id, paper_title)
if (nrow(renumbered) > 0) {
  cat("\nAssignment rows whose `No.` no longer matches 10_final_set.xlsx ",
      "(matched by title instead):\n", sep = "")
  print(as.data.frame(renumbered |> mutate(paper_title = str_trunc(paper_title, 50))),
        row.names = FALSE, right = FALSE)
}
save_log(renumbered, "flag_irr_sample_renumbered")

# Papers an RA ticked off as done but never submitted through the form - a real
# missing submission rather than an artefact of how the sample is defined.
claimed_not_submitted <- assignments |>
  filter(marked_done) |>
  select(coder, paper_id, paper_title) |>
  anti_join(clean |> select(coder, paper_id), by = c("coder", "paper_id")) |>
  arrange(coder, paper_id)
if (nrow(claimed_not_submitted) > 0) {
  cat("\nMarked done in ", basename(IRR_SAMPLE_FILE),
      " but no form submission: ", nrow(claimed_not_submitted), "\n", sep = "")
  print(as.data.frame(claimed_not_submitted |>
                        mutate(paper_title = str_trunc(paper_title, 50))),
        row.names = FALSE, right = FALSE)
}
save_log(claimed_not_submitted, "flag_marked_done_not_submitted")

# Complete 45 x 4 grid: a coder who never submitted a paper appears as an empty
# row rather than being silently absent.
irr <- expand_grid(paper_id = irr_sample$paper_id, coder = CODERS) |>
  left_join(irr_sample |> select(paper_id, paper_title, doi), by = "paper_id") |>
  left_join(clean |> select(-any_of(c("paper_title", "doi"))),
            by = c("paper_id", "coder")) |>
  mutate(submitted = !is.na(timestamp)) |>
  relocate(submitted, .after = coder) |>
  arrange(paper_id, match(coder, CODERS))

irr_gaps <- irr |>
  filter(!submitted) |>
  select(paper_id, coder, paper_title)
if (nrow(irr_gaps) > 0) {
  cat("\nIRR sample gaps: ", nrow(irr_gaps), " of ", nrow(irr),
      " coder x paper cells were never submitted\n", sep = "")
  print(as.data.frame(irr_gaps |> mutate(paper_title = str_trunc(paper_title, 50))),
        row.names = FALSE, right = FALSE)

  fully_missed <- irr_gaps |> count(paper_id, name = "coders_missing") |>
    filter(coders_missing >= length(CODERS) - 1)
  if (nrow(fully_missed) > 0) {
    cat("NOTE: paper ID ", paste(fully_missed$paper_id, collapse = ", "),
        " has at most one coder and contributes nothing to alpha.\n", sep = "")
  }
}
save_log(irr_gaps, "flag_irr_missing_submissions")

# Submissions the RAs made outside the sample (the trial-round paper, and any
# paper coded by arrangement). They stay in descriptive.csv but are not IRR.
outside <- clean |>
  filter(coder != "Amelia", !paper_id %in% irr_sample$paper_id) |>
  count(paper_id, paper_title, name = "ra_submissions")
if (nrow(outside) > 0) {
  cat("\nRA submissions outside the ", nrow(irr_sample),
      "-paper IRR sample (kept in descriptive.csv only):\n", sep = "")
  print(as.data.frame(outside |> mutate(paper_title = str_trunc(paper_title, 50))),
        row.names = FALSE, right = FALSE)
}
save_log(outside, "flag_ra_papers_outside_sample")

# ===========================================================================
# 6. WRITE
# ===========================================================================

# write_excel_csv writes a UTF-8 BOM, so the verbatim quotes with accented
# characters open correctly in Excel.
write_excel_csv(clean,    file.path(DATA_DIR, "descriptive.csv"), na = "")
write_excel_csv(amelia,   file.path(DATA_DIR, "descriptive_coding.csv"), na = "")
write_excel_csv(irr,      file.path(DATA_DIR, "descriptive_shared_subsample_IRR.csv"), na = "")
write_excel_csv(codebook, file.path(DATA_DIR, "descriptive_codebook.csv"), na = "")

coverage <- clean |>
  group_by(coder) |>
  summarise(papers = n_distinct(paper_id),
            in_irr_sample = n_distinct(paper_id[paper_id %in% irr_sample$paper_id]),
            .groups = "drop") |>
  arrange(match(coder, CODERS))

cat("\n", strrep("-", 70), "\n", "COVERAGE\n", strrep("-", 70), "\n", sep = "")
print(as.data.frame(coverage), row.names = FALSE, right = FALSE)

cat("\n", strrep("=", 70), "\n",
    "descriptive.csv                     : ", nrow(clean), " rows\n",
    "descriptive_coding.csv              : ", nrow(amelia), " rows (corpus: ", N_CORPUS, ")\n",
    "descriptive_shared_subsample_IRR.csv: ", nrow(irr), " rows (",
    n_distinct(irr$paper_id), " papers x ", length(CODERS), " coders, ",
    sum(irr$submitted), " submitted)\n",
    "descriptive_codebook.csv            : ", nrow(codebook), " variables\n",
    "Cleaning log                        : ", LOG_DIR, "\n",
    strrep("=", 70), "\n", sep = "")

if (nrow(amelia) != N_CORPUS) {
  warning("Amelia's coding covers ", nrow(amelia), " papers, expected ", N_CORPUS,
          ".", call. = FALSE)
}
