# SocEnRep – WP1 Forward Snowballing via OpenAlex
# Step 4 of the systematic literature review protocol
#
# Run this script from: WP1 & 2/litsearch/
# (i.e., the same working directory as litsearch.R)
#
# Input:
#   output/search_output/07_fulltext_screened.ris   — seed papers (full-text screened set)
#   output/search_output/01_deduplicated_results.csv — original search pool (for dedup)
#
# Output:
#   output/search_output/08_snowballing_log.csv           — per-seed citation counts
#   output/search_output/08_forward_snowballing_raw.csv   — all unique citing works
#   output/search_output/08_forward_snowballing_new.csv   — new records only (ready for screening)
#
# Prerequisites:
#   install.packages(c("openalexR", "dplyr", "readr", "tibble", "stringr"))
#
# last modified: 30.03.2026


library(openalexR)
library(dplyr)
library(readr)
library(tibble)
library(stringr)

OUTPUT_DIR        <- "output/search_output"
SEED_RIS          <- file.path(OUTPUT_DIR, "07_fulltext_screened.ris")
ORIGINAL_POOL_CSV <- file.path(OUTPUT_DIR, "01_deduplicated_results.csv")
DATE_TO           <- "2026-03-30"
INTER_QUERY_SLEEP <- 5    # seconds between API calls (rate-limit protection)
RESOLVE_DOI       <- TRUE # attempt DOI-based OpenAlex ID lookup for seeds without AN field
VERBOSE           <- TRUE

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# HELPER: Normalise titles for fuzzy deduplication
# (same logic as litsearch.R — must stay in sync)

normalise_title <- function(x) {
  x |>
    tolower() |>
    str_squish() |>
    str_remove_all("[^a-z0-9 ]")
}

# HELPER: Parse RIS file → tibble of seed records
# Extracts TI (title), AN (OpenAlex ID), DO (DOI) per record.

parse_ris_seeds <- function(ris_path) {
  lines <- readLines(ris_path, encoding = "UTF-8", warn = FALSE)

  records <- list()
  current <- list(title = NA_character_, openalex_id = NA_character_, doi = NA_character_)

  for (line in lines) {
    # RIS format: "XX  - value" — tag is chars 1-2, value starts at char 7
    tag   <- str_trim(str_sub(line, 1, 2))
    value <- str_trim(str_sub(line, 7))

    if (tag == "TY") {
      # New record — reset
      current <- list(title = NA_character_, openalex_id = NA_character_, doi = NA_character_)
    } else if (tag == "TI") {
      current$title <- value
    } else if (tag == "AN") {
      # Strip URL prefix: "https://openalex.org/W123" → "W123"
      current$openalex_id <- str_remove(value, "^https://openalex\\.org/")
    } else if (tag == "DO") {
      current$doi <- value
    } else if (tag == "ER") {
      records <- c(records, list(as_tibble(current)))
      current <- list(title = NA_character_, openalex_id = NA_character_, doi = NA_character_)
    }
  }

  bind_rows(records)
}

# HELPER: Resolve OpenAlex ID from DOI
# Used for seeds that have a DOI in the RIS but no AN field.
# Returns the OpenAlex work ID ("W...") or NA on failure.

resolve_id_from_doi <- function(doi_val, verbose = TRUE) {
  if (is.na(doi_val) || doi_val == "") return(NA_character_)

  if (verbose) message("    Resolving OpenAlex ID for DOI: ", doi_val)

  result <- tryCatch(
    oa_fetch(entity = "works", doi = doi_val, verbose = FALSE),
    error = function(e) {
      message("    Could not resolve DOI: ", doi_val, " — ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(result) || nrow(result) == 0) {
    message("    No OpenAlex record found for DOI: ", doi_val)
    return(NA_character_)
  }

  id <- str_remove(result$id[[1]], "^https://openalex\\.org/")
  if (verbose) message("    → Resolved to: ", id)
  id
}

# HELPER: Fetch all works citing a given OpenAlex ID

fetch_citing_works <- function(seed_id, seed_title = NA_character_,
                                date_to = NULL,
                                verbose = TRUE, max_retries = 3, wait_sec = 15) {

  if (verbose) message("\n[", seed_id, "] Citing works for: ", str_trunc(seed_title, 70))

  result <- NULL

  for (attempt in seq_len(max_retries)) {
    result <- tryCatch(
      oa_fetch(
        entity  = "works",
        cites   = seed_id,   # direct arg avoids filter=list() URL bloat in openalexR 3.x
        verbose = FALSE
      ),
      error = function(e) {
        message("  ERROR on [", seed_id, "] attempt ", attempt, ": ", conditionMessage(e))
        "ERROR"  # sentinel: distinguishes a real API error from a valid NULL (0 results)
      }
    )

    # oa_fetch returns NULL for 0 results (not an empty tibble), so we use a
    # sentinel string "ERROR" to tell the two cases apart.
    if (!identical(result, "ERROR")) break  # NULL (0 citers) or tibble: valid response

    if (attempt < max_retries) {
      message("  Retrying in ", wait_sec, "s...")
      Sys.sleep(wait_sec)
    } else {
      message("  Failed after ", max_retries, " attempts for [", seed_id, "].")
      return(tibble())
    }
  }

  if (is.null(result) || nrow(result) == 0) {
    if (verbose) message("  → 0 citing works found")
    return(tibble())
  }

  if (verbose) message("  → ", nrow(result), " citing works retrieved (before date filter)")

  # Post-filter by publication year — avoids passing date params in the URL
  # (which causes HTTP 400 "request line is too large" on paginated responses).
  if (!is.null(date_to)) {
    cutoff_year <- as.integer(str_sub(date_to, 1, 4))
    result <- result |> filter(is.na(publication_year) | publication_year <= cutoff_year)
    if (verbose) message("  → ", nrow(result), " after date filter (<= ", cutoff_year, ")")
  }

  if (nrow(result) == 0) {
    if (verbose) message("  → 0 citing works after date filter")
    return(tibble())
  }

  # Standardise columns (same set as litsearch.R output)
  if (!"source_display_name" %in% names(result)) result$source_display_name <- NA_character_
  if (!"abstract"            %in% names(result)) result$abstract            <- NA_character_

  result |>
    mutate(
      seed_openalex_id = seed_id,
      seed_title       = seed_title
    ) |>
    select(
      seed_openalex_id,
      seed_title,
      openalex_id      = id,
      title,
      publication_year,
      doi,
      type,
      is_oa,
      cited_by_count,
      source_name      = source_display_name,
      abstract
    )
}

# STEP 1: Parse seed papers from RIS
message(strrep("=", 60))
message("FORWARD SNOWBALLING – Step 4")
message(strrep("=", 60))
message("\nParsing seed records from: ", SEED_RIS)

seeds <- parse_ris_seeds(SEED_RIS)

if (nrow(seeds) == 0) stop("No records found in RIS file: ", SEED_RIS, call. = FALSE)

n_total   <- nrow(seeds)
n_with_id <- sum(!is.na(seeds$openalex_id))
n_no_id   <- n_total - n_with_id

message("  Total records in RIS:        ", n_total)
message("  Records with OpenAlex ID:    ", n_with_id)
message("  Records without OpenAlex ID: ", n_no_id)

# STEP 1b: Resolve OpenAlex IDs for seeds without AN field (DOI fallback)

if (RESOLVE_DOI && n_no_id > 0) {
  message("\nAttempting DOI-based OpenAlex ID resolution for ", n_no_id, " seeds...")

  seeds_missing <- seeds |> filter(is.na(openalex_id))

  # Only attempt resolution for seeds that actually have a DOI.
  # Use a named vector to avoid NA-matching pitfalls with match().
  doi_vals <- seeds_missing$doi[!is.na(seeds_missing$doi)]

  resolved_named <- setNames(
    vapply(doi_vals, function(d) {
      Sys.sleep(2)  # gentle rate limiting for resolution queries
      resolve_id_from_doi(d, verbose = VERBOSE)
    }, character(1)),
    doi_vals
  )

  seeds <- seeds |>
    mutate(openalex_id = if_else(
      is.na(openalex_id) & !is.na(doi),
      resolved_named[doi],
      openalex_id
    ))

  n_resolved      <- sum(!is.na(resolved_named))
  n_still_missing <- sum(is.na(seeds$openalex_id))

  message("  Successfully resolved: ", n_resolved)
  message("  Still unresolved (will be skipped): ", n_still_missing)

  if (n_still_missing > 0) {
    message("\n  Seeds that could not be resolved:")
    seeds |>
      filter(is.na(openalex_id)) |>
      select(title, doi) |>
      print(n = Inf)
  }
}

seeds_to_query <- seeds |> filter(!is.na(openalex_id))
message("\nSeeds to query: ", nrow(seeds_to_query),
        " (", n_total - nrow(seeds_to_query), " skipped — no resolvable OpenAlex ID)")

# STEP 2: Fetch citing works for each seed

message("\n", strrep("=", 60))
message("FETCHING CITING WORKS (", nrow(seeds_to_query), " seeds)")
message(strrep("=", 60))

citing_raw <- vector("list", nrow(seeds_to_query))

for (i in seq_len(nrow(seeds_to_query))) {
  Sys.sleep(INTER_QUERY_SLEEP)
  citing_raw[[i]] <- fetch_citing_works(
    seed_id    = seeds_to_query$openalex_id[i],
    seed_title = seeds_to_query$title[i],
    date_to    = DATE_TO,
    verbose    = VERBOSE
  )
}

all_citing <- bind_rows(citing_raw)

# bind_rows of all-empty tibbles produces a 0-column tibble, which breaks
# downstream column references. Re-initialise with the expected schema.
if (nrow(all_citing) == 0) {
  all_citing <- tibble(
    seed_openalex_id = character(), seed_title       = character(),
    openalex_id      = character(), title            = character(),
    publication_year = integer(),   doi              = character(),
    type             = character(), is_oa            = logical(),
    cited_by_count   = integer(),   source_name      = character(),
    abstract         = character()
  )
}

message("\nTotal citing records retrieved (with duplicates): ", nrow(all_citing))

# Per-seed log — guard count() against empty all_citing (column would be absent)
n_citing_by_seed <- if (nrow(all_citing) > 0) {
  all_citing |> count(seed_openalex_id, name = "n_citing_retrieved")
} else {
  tibble(seed_openalex_id = character(), n_citing_retrieved = integer())
}

snowballing_log <- seeds_to_query |>
  select(seed_openalex_id = openalex_id, seed_title = title, seed_doi = doi) |>
  left_join(n_citing_by_seed, by = "seed_openalex_id") |>
  mutate(
    n_citing_retrieved = replace_na(n_citing_retrieved, 0L),
    query_date         = Sys.Date() |> as.character()
  )

write_csv(snowballing_log, file.path(OUTPUT_DIR, "08_snowballing_log.csv"))
message("Per-seed log written to 08_snowballing_log.csv")

# STEP 3: Deduplicate citing works among themselves (3-pass)
message("\n", strrep("=", 60))
message("DEDUPLICATION – internal (among retrieved citing works)")
message(strrep("=", 60))
message("Before deduplication: ", nrow(all_citing))

# Pass 1: OpenAlex ID (exact)
step1 <- all_citing |>
  distinct(openalex_id, .keep_all = TRUE)
message("After Pass 1 (OpenAlex ID):      ", nrow(step1),
        "  (removed ", nrow(all_citing) - nrow(step1), ")")

# Pass 2: DOI (case-insensitive, ignoring NA)
step2 <- bind_rows(
  step1 |>
    filter(!is.na(doi)) |>
    mutate(doi_lower = tolower(trimws(doi))) |>
    distinct(doi_lower, .keep_all = TRUE) |>
    select(-doi_lower),
  step1 |>
    filter(is.na(doi))
)
message("After Pass 2 (DOI):              ", nrow(step2),
        "  (removed ", nrow(step1) - nrow(step2), ")")

# Pass 3: Normalised title
step3 <- step2 |>
  mutate(title_norm = normalise_title(title)) |>
  distinct(title_norm, .keep_all = TRUE) |>
  select(-title_norm)
message("After Pass 3 (normalised title): ", nrow(step3),
        "  (removed ", nrow(step2) - nrow(step3), ")")

citing_deduped <- step3
message("Total unique citing works: ", nrow(citing_deduped))

write_csv(citing_deduped, file.path(OUTPUT_DIR, "08_forward_snowballing_raw.csv"))
message("Raw results written to 08_forward_snowballing_raw.csv")

# STEP 4: Remove records already in the original search pool (3-pass)
message("\n", strrep("=", 60))
message("DEDUPLICATION – against original pool (01_deduplicated_results.csv)")
message(strrep("=", 60))

original_pool <- read_csv(
  ORIGINAL_POOL_CSV,
  col_types = cols(
    screening_decision = col_character(),
    screening_notes    = col_character(),
    is_oa              = col_logical()
  )
)

message("Original pool size: ", nrow(original_pool))

# Pass 1: OpenAlex ID
new_after_id <- citing_deduped |>
  filter(!openalex_id %in% original_pool$openalex_id)
message("After removing by OpenAlex ID:      ", nrow(new_after_id),
        "  (removed ", nrow(citing_deduped) - nrow(new_after_id), ")")

# Pass 2: DOI
pool_dois <- original_pool |>
  filter(!is.na(doi)) |>
  mutate(doi_lower = tolower(trimws(doi))) |>
  pull(doi_lower)

new_after_doi <- new_after_id |>
  mutate(doi_lower = tolower(trimws(doi))) |>
  filter(is.na(doi) | !doi_lower %in% pool_dois) |>
  select(-doi_lower)
message("After removing by DOI:              ", nrow(new_after_doi),
        "  (removed ", nrow(new_after_id) - nrow(new_after_doi), ")")

# Pass 3: Normalised title
# Exclude NA from pool_titles — in R, NA %in% c(NA, ...) is TRUE, which would
# incorrectly drop new records that happen to have a missing title.
pool_titles <- original_pool |>
  filter(!is.na(title)) |>
  mutate(title_norm = normalise_title(title)) |>
  pull(title_norm)

new_records <- new_after_doi |>
  mutate(title_norm = normalise_title(title)) |>
  filter(is.na(title_norm) | !title_norm %in% pool_titles) |>
  select(-title_norm)
message("After removing by normalised title: ", nrow(new_records),
        "  (removed ", nrow(new_after_doi) - nrow(new_records), ")")

# STEP 5: Write final output

new_records <- new_records |>
  mutate(
    search_strand      = "forward_snowballing",
    screening_decision = NA_character_,
    screening_notes    = NA_character_
  )

write_csv(new_records, file.path(OUTPUT_DIR, "08_forward_snowballing_new.csv"))

message("\n", strrep("=", 60))
message("SUMMARY")
message(strrep("=", 60))
message("Seed papers in RIS:               ", n_total)
message("Seeds queried (had OpenAlex ID):  ", nrow(seeds_to_query))
message("Total citing works retrieved:     ", nrow(all_citing))
message("After internal deduplication:     ", nrow(citing_deduped))
message("Already in original pool:         ", nrow(citing_deduped) - nrow(new_records))
message("NEW records for screening:        ", nrow(new_records))
message("")
message("Output files written to: ", OUTPUT_DIR, "/")
message("  08_snowballing_log.csv             — per-seed citation counts")
message("  08_forward_snowballing_raw.csv     — all unique citing works (pre-pool dedup)")
message("  08_forward_snowballing_new.csv     — NEW records ready for title/abstract screening")
message("")
message("Next step: screen 08_forward_snowballing_new.csv by title/abstract")
message("using the same criteria as Step 2. Log screening decisions in")
message("'screening_decision' (Include / Exclude / Uncertain) and add")
message("included records to the Zotero library.")
