# SocEnRep – WP1 Literature Search via OpenAlex
# Tier 1 (core concept queries) and Tier 2 (concept × discipline queries)
#
# Prerequisites:
#   install.packages("openalexR")
#   install.packages("dplyr")
#   install.packages("readr")
#   install.packages("tibble")
#   install.packages("tidyr")
#   install.packages("stringr")
#
# last modified: 12.03.2026

library(openalexR)
library(dplyr)
library(readr)
library(tibble)
library(tidyr)
library(stringr)

DATE_FROM         <- "2015-01-01"   # after:2015 filter from the protocol
DATE_TO           <- "2026-03-12"   # fixed to search date for reproducibility
OUTPUT_DIR        <- "lit_search_output"
VERBOSE           <- TRUE           # set FALSE to suppress progress messages
INTER_QUERY_SLEEP <- 5              # seconds between queries (rate limit protection)

dir.create(OUTPUT_DIR, showWarnings = FALSE)

# HELPER FUNCTION

#' Run a single OpenAlex full-text search and return a tibble.
#'
#' Uses the `search` parameter, which queries title + abstract + fulltext
#' (where available) -- equivalent to an unfielded Google Scholar query with
#' quoted phrases. This is intentional: we want matches in abstracts as well
#' as titles, matching the protocol rationale.
#'
#' Includes retry logic: if the API returns an empty result or errors,
#' the query is retried up to `max_retries` times with a wait between attempts.
#' This guards against transient timeouts (e.g., Q1 silently returning 0).
#'
#' @param query_id    Character. Query identifier, e.g. "Q1".
#' @param query_str   Character. The search string, e.g. '"computational reproducibility"'.
#' @param date_from   Character. ISO date "YYYY-MM-DD".
#' @param date_to     Character. ISO date "YYYY-MM-DD".
#' @param verbose     Logical. Print progress.
#' @param max_retries Integer. Number of attempts before giving up.
#' @param wait_sec    Numeric. Seconds to wait between retries.
#'
#' @return A tibble with standardised columns, or an empty tibble on error.

run_query <- function(query_id, query_str, date_from, date_to,
                      verbose = TRUE, max_retries = 3, wait_sec = 10) {

  if (verbose) message("\n[", query_id, "] Searching: ", query_str)

  result <- NULL

  for (attempt in seq_len(max_retries)) {

    result <- tryCatch(
      oa_fetch(
        entity                = "works",
        search                = query_str,
        from_publication_date = date_from,
        to_publication_date   = date_to,
        options               = list(sort = "relevance_score:desc"),
        verbose               = verbose
      ),
      error = function(e) {
        message("  ERROR on [", query_id, "] attempt ", attempt, ": ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(result) && nrow(result) > 0) break

    if (attempt < max_retries) {
      message("  Empty or failed result on attempt ", attempt,
              ". Retrying in ", wait_sec, "s...")
      Sys.sleep(wait_sec)
    } else {
      message("  No results after ", max_retries, " attempts for [", query_id, "].")
      return(tibble())
    }
  }

  # Standardize output: keep only the columns we need for screening.
  # Column guards handle openalexR version differences in naming.
  if (!"source_display_name" %in% names(result)) result$source_display_name <- NA_character_
  if (!"abstract"            %in% names(result)) result$abstract            <- NA_character_

  result |>
    mutate(query_id = query_id, query_str = query_str) |>
    select(
      query_id,
      query_str,
      openalex_id      = id,
      title,
      publication_year,
      doi,
      type,
      is_oa,
      cited_by_count,
      source_name      = source_display_name,
      abstract
    ) |>
    mutate(
      AN                 = openalex_id,     # RIS AN tag — preserved through ASReview → Zotero export
      screening_decision = NA_character_,  # to be filled: Include / Exclude / Uncertain
      screening_notes    = NA_character_
    )
}

# TIER 1: CORE CONCEPT QUERIES (no discipline filter)
# Each query uses a quoted phrase matching the protocol exactly.
# The `search` parameter in OpenAlex searches title + abstract,
# so quoted phrases must appear as-is in one of those fields.

tier1_queries <- tribble(
  ~query_id, ~query_str,
  "Q1",  '"computational reproducibility"',
  "Q2",  '"reproducibility criteria"',
  "Q3",  '"reproducibility checklist"',
  "Q4",  '"reproducibility requirement"',
  "Q5",  '"reproducibility badge"',
  "Q6",  '"reproducibility verification"'
)

message("\n", strrep("=", 60))
message("TIER 1 SEARCHES (", nrow(tier1_queries), " queries)")
message(strrep("=", 60))

tier1_results <- do.call(bind_rows, lapply(seq_len(nrow(tier1_queries)), function(i) {
  Sys.sleep(INTER_QUERY_SLEEP)
  run_query(
    query_id  = tier1_queries$query_id[i],
    query_str = tier1_queries$query_str[i],
    date_from = DATE_FROM,
    date_to   = DATE_TO,
    verbose   = VERBOSE,
    wait_sec  = if (tier1_queries$query_id[i] == "Q1") 60 else 15
  )
}))

# TIER 2: CONCEPT × DISCIPLINE QUERIES
# The discipline term is appended inside the search string, matching the
# exact query strings specified in the protocol. OpenAlex will require all
# terms to appear in the title/abstract -- the quoted phrase AND the
# discipline word.
#
# Multi-word discipline terms ("social science", "political science",
# "communication science", "management science") are quoted to enforce
# phrase matching in OpenAlex.

tier2_queries <- tribble(
  ~query_id, ~query_str,
  "Q7",  '"computational reproducibility" economics',
  "Q8",  '"computational reproducibility" "social science"',
  "Q9",  '"computational reproducibility" "political science"',
  "Q10", '"computational reproducibility" sociology',
  "Q11", '"computational reproducibility" "communication science"',
  "Q12", '"reproducibility criteria" economics',
  "Q13", '"reproducibility criteria" "social science"',
  "Q14", '"computational reproducibility" "management science"',
  "Q15", '"computational reproducibility" finance'
)

message("\n", strrep("=", 60))
message("TIER 2 SEARCHES (", nrow(tier2_queries), " queries)")
message(strrep("=", 60))

tier2_results <- do.call(bind_rows, lapply(seq_len(nrow(tier2_queries)), function(i) {
  Sys.sleep(INTER_QUERY_SLEEP)
  run_query(
    query_id  = tier2_queries$query_id[i],
    query_str = tier2_queries$query_str[i],
    date_from = DATE_FROM,
    date_to   = DATE_TO,
    verbose   = VERBOSE
  )
}))

# COMBINE AND DEDUPLICATE
all_results <- bind_rows(tier1_results, tier2_results)

message("\n", strrep("=", 60))
message("DEDUPLICATION")
message(strrep("=", 60))
message("Total records before deduplication: ", nrow(all_results))

# --- Overlap log ------------------------------------------------------------
# Computed BEFORE deduplication so we capture all query_id occurrences.
# Uses n_distinct() to guard against within-query pagination duplicates.

overlap_log <- all_results |>
  group_by(openalex_id, title) |>
  summarise(
    retrieved_by = paste(unique(query_id), collapse = ", "),
    n_queries    = n_distinct(query_id),
    .groups      = "drop"
  )

# --- Helper: normalise titles for fuzzy matching ----------------------------
# Lowercases, collapses whitespace, strips all non-alphanumeric characters.
# This ensures "Reproducibility: A Review" and "Reproducibility A Review"
# are treated as the same title.

normalise_title <- function(x) {
  x |>
    tolower() |>
    str_squish() |>
    str_remove_all("[^a-z0-9 ]")
}

# --- Pass 1: deduplicate by OpenAlex ID (exact) -----------------------------
# Catches straightforward duplicates from query overlap.

step1 <- all_results |>
  distinct(openalex_id, .keep_all = TRUE)

message("After Pass 1 (OpenAlex ID):      ", nrow(step1), " records")

# --- Pass 2: deduplicate by DOI ---------------------------------------------
# Catches preprint + published version pairs where each has a different
# OpenAlex ID but the same DOI. Records without a DOI are passed through
# unchanged (they cannot be matched on this criterion).

step2 <- bind_rows(
  step1 |>
    filter(!is.na(doi)) |>
    mutate(doi_lower = tolower(trimws(doi))) |>
    distinct(doi_lower, .keep_all = TRUE) |>
    select(-doi_lower),
  step1 |>
    filter(is.na(doi))
)

message("After Pass 2 (DOI):              ", nrow(step2), " records  ",
        "(removed ", nrow(step1) - nrow(step2), ")")

# --- Pass 3: deduplicate by normalised title --------------------------------
# Catches remaining duplicates where DOI is absent or differs between
# versions (e.g., two preprint versions, or a record missing a DOI in
# OpenAlex). Over-deduplication risk is low but non-zero for very short
# titles; inspect the removal log if Pass 3 removes more than ~50 records.

step3 <- step2 |>
  mutate(title_norm = normalise_title(title)) |>
  distinct(title_norm, .keep_all = TRUE) |>
  select(-title_norm)

message("After Pass 3 (normalised title): ", nrow(step3), " records  ",
        "(removed ", nrow(step2) - nrow(step3), ")")

# Spot-check: export records removed by Pass 3 for manual inspection
removed_by_pass3 <- anti_join(step2, step3, by = "openalex_id") |>
  select(openalex_id, title, doi, source_name, publication_year)

if (nrow(removed_by_pass3) > 0) {
  write_csv(
    removed_by_pass3,
    file.path(OUTPUT_DIR, "04_pass3_removed_titles.csv")
  )
  message("  Pass 3 removal log written to 04_pass3_removed_titles.csv")
  if (nrow(removed_by_pass3) > 50) {
    message("  WARNING: Pass 3 removed >50 records. ",
            "Inspect 04_pass3_removed_titles.csv before proceeding.")
  }
}

deduplicated <- step3

message("Total records after deduplication: ", nrow(deduplicated))

# --- Flag records missing a DOI ---------------------------------------------
n_no_doi <- sum(is.na(deduplicated$doi))
message("Records without a DOI (likely preprints/grey lit): ", n_no_doi)

# OUTPUT FILES
# Full deduplicated results for title/abstract screening
# Note: col types are set explicitly so screening_decision and screening_notes
# are always read back as character (not logical) when re-imported into R.
write_csv(
  deduplicated,
  file.path(OUTPUT_DIR, "01_deduplicated_results.csv")
)

# Overlap log (which queries retrieved the same paper)
write_csv(
  overlap_log,
  file.path(OUTPUT_DIR, "02_overlap_log.csv")
)

# Search log summary (record counts per query, for PRISMA flow)
search_log <- bind_rows(tier1_queries, tier2_queries) |>
  mutate(tier = if_else(query_id %in% tier1_queries$query_id, 1L, 2L)) |>
  left_join(
    all_results |> count(query_id, name = "n_retrieved"),
    by = "query_id"
  ) |>
  mutate(
    search_date          = Sys.Date() |> as.character(),
    date_filter          = paste0(DATE_FROM, " to ", DATE_TO),
    n_retrieved          = replace_na(n_retrieved, 0L),
    n_after_global_dedup = nrow(deduplicated)  # same value for all rows (shared pool)
  )

write_csv(
  search_log,
  file.path(OUTPUT_DIR, "03_search_log.csv")
)

# REPORT
message("\n", strrep("=", 60))
message("SEARCH SUMMARY")
message(strrep("=", 60))

search_log |>
  select(query_id, tier, query_str, n_retrieved) |>
  print(n = 20)

message("\nTotal unique records (Tier 1 + Tier 2): ", nrow(deduplicated))
message("Output files written to: ", OUTPUT_DIR, "/")
message("  01_deduplicated_results.csv  -- main file for title/abstract screening")
message("  02_overlap_log.csv           -- query overlap analysis")
message("  03_search_log.csv            -- PRISMA flow input")
message("  04_pass3_removed_titles.csv  -- Pass 3 removal log (inspect if >50 removed)")
message("\nNext step: open 01_deduplicated_results.csv and fill in")
message("'screening_decision' (Include / Exclude / Uncertain) per row.")