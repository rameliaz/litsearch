# Build the Part B2 criteria dependency tables
#
# Section 3.7 of descriptive_analysis_coding.R counts how often the "is
# prerequisite for" / "depends on" fields are filled and builds a corpus-wide
# from/to count (07_dependency_edges.csv), but stops there: "counts only; the
# DAG comes later". This script is that next step - it resolves each stated
# link back to the paper(s) that assert it, so a link can be checked against
# its source rather than taken as a bare count.
#
# Both fields are free text, filled in from each criterion's own point of
# view (e.g. paper 45's "dependency resolution" row names "computing
# environment" under "is prerequisite for"; paper 45's "computing environment"
# row names "dependency resolution" under "depends on" - same link, stated
# twice). Building the edge list from only one field, as 07_dependency_edges.csv
# does, misses a link on the (rare) rows where a coder filled in only the
# other side. This script reads both fields and records, per link, whether it
# was corroborated from both sides or only one - the "stated_via" column below.
#
# Prerequisites:
#   install.packages(c("readr", "dplyr", "tidyr", "stringr", "here"))
#
# Input:  dataset/b2_criteria_extraction.csv
#
# Output: output/analysis_output/tables/07_dependency_edges_by_paper.csv
#           One row per paper x prerequisite x dependent link actually
#           asserted in that paper - the answer to "which corpus paper
#           mentions this link".
#         output/analysis_output/tables/07_dependency_pairs_summary.csv
#           One row per distinct prerequisite -> dependent pair, with the
#           full list of papers behind it (corpus-wide summary of the above).
#         output/analysis_output/tables/07_flag_dependency_delimiter.csv
#           Rows where a coder separated multiple targets with a comma
#           instead of the ";" the codebook specifies. Read as one token by
#           07_dependency_edges.csv (see its 07_flag_unmatched_dependency_targets.csv),
#           this script splits on both, but the source cell still needs fixing
#           by hand - fix in the coding workbook and re-run
#           get_criteria_extraction.R, then this file should come back empty.
#
# last modified: 28.09.2026

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(here)

TABLES_DIR <- here("output", "analysis_output", "tables")
dir.create(TABLES_DIR, recursive = TRUE, showWarnings = FALSE)

save_table <- function(x, name) {
  write_excel_csv(x, file.path(TABLES_DIR, paste0(name, ".csv")), na = "")
  invisible(x)
}

report <- function(x, title) {
  cat("\n", strrep("-", 70), "\n", title, "\n", strrep("-", 70), "\n", sep = "")
  print(as.data.frame(x), row.names = FALSE, right = FALSE)
  invisible(x)
}

# ===========================================================================
# 1. READ + RESOLVE CRITERION IDENTITY
# ===========================================================================

b2 <- read_csv(here("dataset", "b2_criteria_extraction.csv"),
               col_types = cols(paper_id = col_integer(), .default = col_character()))

# Same resolution as descriptive_analysis_coding.R's `criterion`: a "new
# criterion" row is identified by its coder-written label, not the literal
# string "new criterion" - and that written label is also what appears
# verbatim in another row's "is prerequisite for" / "depends on" cell, so no
# separate lookup is needed for the target side.
b2 <- b2 |>
  mutate(criterion = if_else(`criterion label` == "new criterion",
                              str_squish(`new criterion label`),
                              `criterion label`),
         .after = `new criterion label`)

dup_id <- b2 |> count(paper_id, criterion) |> filter(n > 1)
if (nrow(dup_id) > 0) {
  stop("paper_id x criterion is not unique - the evidence-text lookup below ",
       "assumes one row per criterion per paper. Offending rows: ",
       paste(dup_id$paper_id, dup_id$criterion, sep = "/", collapse = ", "),
       call. = FALSE)
}

# ===========================================================================
# 2. FLAG: COMMA USED AS THE TARGET SEPARATOR
# ===========================================================================

# No controlled criterion label contains a comma, so any comma in these two
# fields is a delimiter slip, not a value. Flagged here, then split on
# below anyway so the links inside it aren't silently dropped from the tables.
delimiter_flag <- b2 |>
  filter(str_detect(`is prerequisite for`, ",") | str_detect(`depends on`, ",")) |>
  select(paper_id, paper_title, criterion, `is prerequisite for`, `depends on`)
if (nrow(delimiter_flag) > 0) {
  report(delimiter_flag, "COMMA USED INSTEAD OF ';' TO SEPARATE TARGETS (fix in the coding workbook)")
}
save_table(delimiter_flag, "07_flag_dependency_delimiter")

# ===========================================================================
# 3. BUILD PAPER-LEVEL LINKS
# ===========================================================================

# category_lookup fills in the category of the *target* named in a free-text
# cell (the target's own row has it; the cell itself is just a label).
category_lookup <- b2 |>
  filter(!is.na(`criterion category`)) |>
  distinct(paper_id, criterion, category = `criterion category`)

split_targets <- function(d, field) {
  d |>
    select(paper_id, paper_title, coder, criterion, category = `criterion category`,
           target = all_of(field)) |>
    filter(!is.na(target), nzchar(target)) |>
    mutate(target = str_split(target, "[;,]")) |>
    unnest(target) |>
    mutate(target = str_squish(target)) |>
    filter(nzchar(target))
}

# "is prerequisite for": this row's own criterion is the prerequisite, the
# named target is the dependent (its category needs the lookup).
from_prereq_field <- split_targets(b2, "is prerequisite for") |>
  transmute(paper_id, paper_title, coder,
            prerequisite_criterion = criterion, prerequisite_category = category,
            dependent_criterion = target, role = "is prerequisite for") |>
  left_join(category_lookup, by = c("paper_id", "dependent_criterion" = "criterion")) |>
  rename(dependent_category = category)

# "depends on": this row's own criterion is the dependent, the named target
# is the prerequisite (its category needs the lookup).
from_depends_field <- split_targets(b2, "depends on") |>
  transmute(paper_id, paper_title, coder,
            dependent_criterion = criterion, dependent_category = category,
            prerequisite_criterion = target, role = "depends on") |>
  left_join(category_lookup, by = c("paper_id", "prerequisite_criterion" = "criterion")) |>
  rename(prerequisite_category = category)

link_cols <- c("paper_id", "paper_title", "coder", "prerequisite_criterion",
               "prerequisite_category", "dependent_criterion", "dependent_category", "role")
all_statements <- bind_rows(from_prereq_field[link_cols], from_depends_field[link_cols])

evidence_lookup <- b2 |> select(paper_id, criterion, evidence = `text evidence`)

links_by_paper <- all_statements |>
  distinct(paper_id, paper_title, coder, prerequisite_criterion, prerequisite_category,
           dependent_criterion, dependent_category, role) |>
  group_by(paper_id, paper_title, coder, prerequisite_criterion, prerequisite_category,
           dependent_criterion, dependent_category) |>
  summarise(stated_via = if (n() == 2) "both fields" else role, .groups = "drop") |>
  left_join(evidence_lookup, by = c("paper_id", "prerequisite_criterion" = "criterion")) |>
  rename(prerequisite_row_evidence = evidence) |>
  left_join(evidence_lookup, by = c("paper_id", "dependent_criterion" = "criterion")) |>
  rename(dependent_row_evidence = evidence) |>
  arrange(paper_id, prerequisite_criterion, dependent_criterion)

self_loops <- links_by_paper |> filter(prerequisite_criterion == dependent_criterion)
if (nrow(self_loops) > 0) {
  report(self_loops |> select(paper_id, prerequisite_criterion),
         "SELF-REFERENTIAL LINKS (a criterion named as its own prerequisite/dependent)")
}

report(links_by_paper |> count(stated_via), "LINKS BY HOW THEY WERE STATED")
save_table(links_by_paper, "07_dependency_edges_by_paper")

# ===========================================================================
# 4. CORPUS-WIDE SUMMARY: ONE ROW PER PAIR, PAPERS LISTED
# ===========================================================================

pairs_summary <- links_by_paper |>
  group_by(prerequisite_criterion, prerequisite_category,
           dependent_criterion, dependent_category) |>
  summarise(
    n_papers = n_distinct(paper_id),
    n_both_fields = sum(stated_via == "both fields"),
    paper_ids = paste(sort(unique(paper_id)), collapse = "; "),
    paper_titles = paste(unique(paper_title), collapse = " | "),
    .groups = "drop"
  ) |>
  mutate(cross_dimension = prerequisite_category != dependent_category) |>
  arrange(desc(n_papers), prerequisite_criterion, dependent_criterion)

report(pairs_summary |> select(-paper_titles), "DEPENDENCY PAIRS ACROSS THE CORPUS")
save_table(pairs_summary, "07_dependency_pairs_summary")

cat("\n", strrep("=", 70), "\n",
    "07_dependency_edges_by_paper.csv  : ", nrow(links_by_paper), " links across ",
    n_distinct(links_by_paper$paper_id), " papers\n",
    "07_dependency_pairs_summary.csv   : ", nrow(pairs_summary), " distinct pairs\n",
    "07_flag_dependency_delimiter.csv  : ", nrow(delimiter_flag), " row(s)\n",
    strrep("=", 70), "\n", sep = "")
