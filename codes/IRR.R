# SocEnRep - Interrater reliability (Krippendorff's alpha) for Part A (descriptive) coding
#
# Computes Krippendorff's alpha for every codeable field in the 45-paper
# descriptive-coding IRR sample (Amelia + 3 RAs). Threshold per the protocol
# (protocol_logbook.docx, "Coding process") is alpha >= 0.70; fields below
# that are flagged as candidates for a reconciliation meeting.
#
# Only Part A (descriptive coding) is covered here. Part B (criteria
# extraction, Amelia vs. Gunther) is left out on purpose - Gunther's 15-paper
# sheet is still empty, and Part B is extraction rather than rating (see the
# note at the top of get_criteria_extraction.R), so it needs its own
# alignment step before an alpha can even be computed.
#
# The codebook (dataset/descriptive_codebook.csv) splits the codeable fields
# into three types, handled differently here:
#   - single_select fields: one alpha per field, nominal, straight from the
#     raw category label per paper x coder.
#   - multi_select (checkbox) fields: one POOLED alpha per field. Every
#     option that was ever picked is dummy-coded into a present/absent (0/1)
#     indicator, and all (paper x option) cells are stacked into one coder x
#     item matrix. This gives one number per field instead of fragmenting
#     into dozens of per-option alphas, most of which would be undefined
#     (options picked by only 1-2 papers give no basis for an agreement
#     estimate).
#   - D1A (pasted definition text) and notes are free text, so a real content
#     alpha isn't possible without a separate qualitative pass. As a coarse
#     stand-in, this checks only whether coders agree on whether they wrote
#     anything at all (blank vs. non-blank) - NOT whether the text agrees.
#
# Krippendorff's alpha handles missing data natively (skip logic, e.g. B1A is
# only answered when B1 = "Yes", shows up as NA and is simply left out of the
# comparison), so nothing is dropped or imputed.
#
# Note: irr::kripp.alpha() throws a "NAs introduced by coercion" warning on
# nominal character data (e.g. "Yes"/"No"). Reading the package source shows
# this comes from an internal as.numeric() step the function always runs but
# never actually uses for method = "nominal" (only the interval/ratio
# branches touch it) - confirmed against irr 0.85. Safe to suppress.
#
# Prerequisites:
#   install.packages(c("readr", "dplyr", "tidyr", "stringr", "tibble", "irr", "here"))
#
# Input:  dataset/descriptive_shared_subsample_IRR.csv  (45 papers x 4 coders)
#         dataset/descriptive_codebook.csv               (variable -> question text)
#
# Output: output/analysis_output/tables/irr_descriptive.csv
#
# last modified: 10.08.2026

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(irr)
library(here)

# here() resolves to the litsearch/ folder (the git root), so this script runs
# regardless of which folder Positron was opened in.

options(width = 130)

ALPHA_THRESHOLD <- 0.70
TABLE_DIR <- here("output", "analysis_output", "tables")
dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# Helpers for multi-select (checkbox) fields
# ---------------------------------------------------------------------------
# Same logic as in descriptive_analysis_coding.R. Google Forms joins checked
# options with ", ". Two option labels in the deployed form contain their own
# commas, so a naive split on "," shatters them:
#   - "Containerization (e.g., Docker, Singularity, CodeOcean, Binder, ...)"  [E1b]
#   - "After acceptance, before publication" and
#     "Before submission, proactively"                                        [F3]
# The first case is handled generically by masking commas inside parentheses;
# the second by protecting the literal labels before splitting.

# Written as a visible escape rather than a literal character, unlike the
# original in descriptive_analysis_coding.R, where it renders as invisible
# in most editors and reads as an empty string until you check the raw bytes.
SENTINEL <- "\x01"

PROTECTED_OPTIONS <- c(
  "After acceptance, before publication",
  "Before submission, proactively"
)

mask_parens <- function(s) {
  vapply(s, function(one) {
    if (is.na(one) || !str_detect(one, "\\(")) return(one)
    chars <- str_split(one, "")[[1]]
    depth <- 0L
    for (i in seq_along(chars)) {
      if (chars[i] == "(") {
        depth <- depth + 1L
      } else if (chars[i] == ")") {
        depth <- max(0L, depth - 1L)
      } else if (chars[i] == "," && depth > 0L) {
        chars[i] <- SENTINEL
      }
    }
    paste(chars, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

split_multi <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""          # unanswered fields contribute no options
  x <- mask_parens(x)
  for (opt in PROTECTED_OPTIONS) {
    x <- str_replace_all(x, fixed(opt), str_replace_all(opt, ",", SENTINEL))
  }
  out <- str_split(x, ",")
  out <- lapply(out, function(tok) {
    tok <- str_replace_all(tok, SENTINEL, ",")
    tok <- str_squish(tok)
    tok[nzchar(tok)]
  })
  out
}

# ---------------------------------------------------------------------------
# Core: reshape a long (item, coder, value) table into a coder x item matrix
# and run Krippendorff's alpha. Shared by all three field types below - they
# differ only in how `item` and `value` are built beforehand.
# ---------------------------------------------------------------------------

run_alpha <- function(long, variable, label, field_type) {
  wide <- long |>
    pivot_wider(names_from = item, values_from = value) |>
    column_to_rownames("coder") |>
    as.matrix()

  res <- suppressWarnings(kripp.alpha(wide, method = "nominal"))
  alpha <- ifelse(is.nan(res$value), NA_real_, round(res$value, 3))

  # Items where fewer than 2 coders provided a value can't inform the alpha
  # (nothing to compare); report how many actually did.
  n_valid <- sum(colSums(!is.na(wide)) >= 2)

  tibble(
    variable        = variable,
    label           = label,
    field_type      = field_type,
    n_items         = ncol(wide),
    n_valid         = n_valid,
    alpha           = alpha,
    meets_threshold = !is.na(alpha) & alpha >= ALPHA_THRESHOLD
  )
}

# ===========================================================================
# 1. LOAD
# ===========================================================================

irr_data <- read_csv(here("dataset", "descriptive_shared_subsample_IRR.csv"),
                      show_col_types = FALSE)

codebook <- read_csv(here("dataset", "descriptive_codebook.csv"),
                      show_col_types = FALSE)
field_label <- setNames(codebook$question, codebook$variable)

cat("\nIRR sample: ", n_distinct(irr_data$paper_id), " papers x ",
    n_distinct(irr_data$coder), " coders (",
    sum(irr_data$submitted), " of ", nrow(irr_data), " cells submitted)\n", sep = "")

# ===========================================================================
# 2. SINGLE-SELECT FIELDS
# ===========================================================================

SINGLE_FIELDS <- c("B1", "C1", "C2", "D1", "D2", "E1", "E1A", "E1E", "E1F", "F1")

single_results <- lapply(SINGLE_FIELDS, function(v) {
  long <- irr_data |>
    transmute(item = as.character(paper_id), coder, value = .data[[v]])
  run_alpha(long, v, field_label[[v]], "single_select")
}) |> bind_rows()

# ===========================================================================
# 3. MULTI-SELECT (CHECKBOX) FIELDS - pooled across options
# ===========================================================================

MULTI_FIELDS <- c("B1A", "D2A", "E1B", "E1C", "E1D", "F2", "F3")

multi_results <- lapply(MULTI_FIELDS, function(v) {
  tokens       <- split_multi(irr_data[[v]])
  options_used <- sort(unique(unlist(tokens)))

  # One 0/1 column per option: did this row's coder check that option?
  presence <- vapply(options_used, function(opt) {
    vapply(tokens, function(t) as.integer(opt %in% t), integer(1))
  }, integer(nrow(irr_data)))
  colnames(presence) <- options_used

  long <- bind_cols(irr_data |> select(paper_id, coder), as_tibble(presence)) |>
    pivot_longer(-c(paper_id, coder), names_to = "option", values_to = "value") |>
    transmute(item = paste(paper_id, option, sep = "__"), coder, value)

  run_alpha(long, v, field_label[[v]], "multi_select") |>
    mutate(n_options = length(options_used))
}) |> bind_rows()

# ===========================================================================
# 4. FREE-TEXT FIELDS - coarse "wrote something vs. left blank" check
# ===========================================================================

BLANK_FIELDS <- c("D1A", "notes")

blank_results <- lapply(BLANK_FIELDS, function(v) {
  long <- irr_data |>
    transmute(item = as.character(paper_id), coder,
              value = as.integer(!is.na(.data[[v]]) & nzchar(.data[[v]])))
  run_alpha(long, v, field_label[[v]], "freetext_blank_check")
}) |> bind_rows()

# ===========================================================================
# 5. COMBINE, REPORT, SAVE
# ===========================================================================

results <- bind_rows(single_results, multi_results, blank_results) |>
  mutate(field_type = factor(field_type,
                             levels = c("single_select", "multi_select",
                                        "freetext_blank_check"))) |>
  arrange(field_type) |>
  select(variable, label, field_type, n_items, n_valid, n_options,
         alpha, meets_threshold)

cat("\n", strrep("=", 78), "\n",
    "KRIPPENDORFF'S ALPHA - PART A (DESCRIPTIVE CODING), 45-PAPER IRR SAMPLE\n",
    strrep("=", 78), "\n", sep = "")
print(as.data.frame(results |> select(-label)), row.names = FALSE)

below <- results |> filter(!meets_threshold)
if (nrow(below) > 0) {
  cat("\n", nrow(below), " field(s) below the alpha >= ", ALPHA_THRESHOLD,
      " threshold - candidates for the reconciliation meeting:\n", sep = "")
  for (i in seq_len(nrow(below))) {
    cat("  ", below$variable[i], " (", below$label[i], "): alpha = ",
        below$alpha[i], "\n", sep = "")
  }
} else {
  cat("\nAll fields meet the alpha >= ", ALPHA_THRESHOLD, " threshold.\n", sep = "")
}

write_csv(results, file.path(TABLE_DIR, "irr_descriptive.csv"))
cat("\nWritten to: ", file.path(TABLE_DIR, "irr_descriptive.csv"), "\n", sep = "")
