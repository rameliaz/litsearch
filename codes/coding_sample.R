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
# Input:  lit_search_output/10_final_set_before_expert.xlsx
# Output: lit_search_output/11_irr_sample_descriptive.csv
#         lit_search_output/11_irr_sample_criteria.csv
#
# last modified: 15.05.2026

library(readxl)
library(dplyr)
library(readr)

set.seed(2026)  # fixed number so that random sample is always consistent 

N_DESCRIPTIVE <- 45   # ~30% of 145; coded by RAs alongside Amelia
N_CRITERIA    <- 15   # ~10% of 145; coded by Gunther alongside Amelia
EXCLUDE_TYPES <- c("BLOG", "ART") # The reason is that these types of publications contain limited information
INPUT_FILE    <- "lit_search_output/10_final_set.xlsx"
OUTPUT_DIR    <- "lit_search_output"

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

write_csv(sample_descriptive,
          file.path(OUTPUT_DIR, "11_irr_sample_descriptive.csv"))
write_csv(sample_criteria,
          file.path(OUTPUT_DIR, "11_irr_sample_criteria.csv"))

message("\nDone. Outputs written to ", OUTPUT_DIR, "/")
message("  11_irr_sample_descriptive.csv  -- ", N_DESCRIPTIVE,
        " papers for descriptive coding IRR")
message("  11_irr_sample_criteria.csv     -- ", N_CRITERIA,
        " papers for criteria extraction IRR")
