# SocEnRep – Stratified IRR sampling for Step 5 coding
#
# Draws two proportionally stratified random samples from the final included set:
#   - 45 papers for descriptive coding IRR (Ineke, Mehtab, and one RA from RWI independently)
#   - 15 papers for criteria extraction IRR (Gunther)
#
# Both samples exclude BLOG and ART (too little extractable information).
# Both samples are drawn independently from the same eligible pool.
# Stratification is by publication type to preserve the pool's composition.
#
# Prerequisites:
#   install.packages("readxl")
#   install.packages("dplyr")
#   install.packages("readr")
#
# Input:  output/search_output/10_final_set.xlsx
# Output: output/search_output/11_irr_sample_descriptive.csv
#         output/search_output/11_irr_sample_criteria.csv
#
# last modified: 26.07.2026 (paths updated for the output/ restructure)

library(readxl)
library(dplyr)
library(readr)

set.seed(2026)  # fixed number so that random sample is always consistent 

N_DESCRIPTIVE <- 45   # ~30% of 145; coded by RAs alongside Amelia
N_CRITERIA    <- 15   # ~10% of 145; coded by Gunther alongside Amelia
EXCLUDE_TYPES <- c("BLOG", "ART") # The reason is that these types of publications contain limited information
INPUT_FILE    <- "output/search_output/10_final_set.xlsx"
OUTPUT_DIR    <- "output/search_output"

# Guard against redrawing the samples the coders already worked from. See the
# note next to the write step at the bottom of this script.
OVERWRITE_SAMPLES <- FALSE

papers <- read_excel(INPUT_FILE, sheet = "Literature") |>
  filter(!is.na(`No.`)) |>                        # drop empty trailing rows
  filter(!is.na(Type), !Type %in% EXCLUDE_TYPES)

message("Eligible papers: ", nrow(papers), " (excluded BLOG and ART)")
message("\nType breakdown:")
papers |> count(Type, sort = TRUE) |> print(n = Inf)

# Proportional allocation with largest-remainder rounding.
# This guarantees the total always equals the target exactly,
# unlike simple round() which can be off by 1 or 2.
alloc_proportional <- function(counts, target) {
  exact     <- counts / sum(counts) * target
  allocated <- floor(exact)
  remainder <- target - sum(allocated)
  idx       <- order(exact - allocated, decreasing = TRUE)[seq_len(remainder)]
  allocated[idx] <- allocated[idx] + 1
  allocated
}

# Sample n_total papers from data, stratified by Type
draw_sample <- function(data, n_total) {
  strata <- data |>
    count(Type) |>
    mutate(n_draw = alloc_proportional(n, n_total))

  if (sum(strata$n_draw) != n_total)
    stop("Allocation mismatch: got ", sum(strata$n_draw), ", expected ", n_total)

  do.call(bind_rows, lapply(seq_len(nrow(strata)), function(i) {
    data |>
      filter(Type == strata$Type[i]) |>
      slice_sample(n = strata$n_draw[i])
  })) |>
    arrange(Type, `No.`)
}

sample_descriptive <- draw_sample(papers, N_DESCRIPTIVE)
sample_criteria    <- draw_sample(papers, N_CRITERIA)

message("\nDescriptive IRR sample (n = ", nrow(sample_descriptive),
        ", for RAs):")
sample_descriptive |> count(Type) |> print()

message("\nCriteria extraction IRR sample (n = ", nrow(sample_criteria),
        ", for Gunther):")
sample_criteria |> count(Type) |> print()

overlap <- sum(sample_descriptive$`No.` %in% sample_criteria$`No.`)
message("\nPapers appearing in both samples: ", overlap)

out_descriptive <- file.path(OUTPUT_DIR, "11_irr_sample_descriptive.csv")
out_criteria    <- file.path(OUTPUT_DIR, "11_irr_sample_criteria.csv")

# set.seed() alone does NOT make this script reproducible: the draw depends on
# the contents of 10_final_set.xlsx, which has been edited since the samples
# were drawn on 12.05.2026 (rows were renumbered). Re-running therefore yields a
# DIFFERENT set of papers, which would break the link between the coding data
# and the sample the RAs actually coded. The saved CSVs are the artifact.
if (!OVERWRITE_SAMPLES && (file.exists(out_descriptive) || file.exists(out_criteria))) {
  stop("IRR samples already exist in ", OUTPUT_DIR, "/.\n",
       "  These are the samples the coders actually worked from (drawn 12.05.2026).\n",
       "  Re-running would draw a different set, because 10_final_set.xlsx has\n",
       "  changed since. Set OVERWRITE_SAMPLES <- TRUE at the top only if you\n",
       "  really intend to replace them.",
       call. = FALSE)
}

write_csv(sample_descriptive, out_descriptive)
write_csv(sample_criteria, out_criteria)

message("\nDone. Outputs written to ", OUTPUT_DIR, "/")
message("  11_irr_sample_descriptive.csv  -- ", N_DESCRIPTIVE,
        " papers for descriptive coding IRR")
message("  11_irr_sample_criteria.csv     -- ", N_CRITERIA,
        " papers for criteria extraction IRR")
