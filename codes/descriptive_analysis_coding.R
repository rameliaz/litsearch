# SocEnRep – Descriptive analysis of Step 5 coding results
#
# Produces descriptive statistics only (counts, distributions, cross-tabulations).
# No inferential statistics and no IRR here.
#
# Covers the three files in dataset/ that hold full coding by Amelia:
#   Part A  descriptive coding of all 145 papers
#   Part B1 how each paper operationalizes reproducibility success
#   Part B2 which specific criteria each paper invokes
#
# Prerequisites:
#   install.packages(c("readr", "readxl", "dplyr", "tidyr", "stringr",
#                      "forcats", "ggplot2", "scales", "here"))
#
# Input:  dataset/descriptive_coding.csv
#         dataset/b1_criteria_extraction.csv
#         dataset/b2_criteria_extraction.csv
#         output/search_output/10_final_set.xlsx   (for Year and Type)
#
# Output: output/analysis_output/tables/*.csv
#         output/analysis_output/figures/*.png
#
# last modified: 26.07.2026

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(scales)
library(here)

# here() resolves to the litsearch/ folder (the git root), so this script runs
# regardless of which folder Positron was opened in.

options(width = 130)  # keep the console tables from wrapping

N_CORPUS  <- 145  # papers in the final set
TABLE_DIR <- here("output", "analysis_output", "tables")
FIG_DIR   <- here("output", "analysis_output", "figures")

dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_DIR,   recursive = TRUE, showWarnings = FALSE)

# Colours for the three reproducibility dimensions, used consistently in every figure
DIM_COLOURS <- c(
  "Availability"  = "#4C72B0",
  "Executability" = "#DD8452",
  "Consistency"   = "#55A868"
)

theme_socenrep <- function() {
  theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.title         = element_text(face = "bold", size = 12),
      plot.subtitle      = element_text(colour = "grey30", size = 9),
      plot.caption       = element_text(colour = "grey45", size = 8, hjust = 0),
      strip.text         = element_text(face = "bold")
    )
}

save_table <- function(x, name) {
  write_csv(x, file.path(TABLE_DIR, paste0(name, ".csv")))
  invisible(x)
}

save_fig <- function(plot, name, width = 8, height = 5) {
  ggsave(file.path(FIG_DIR, paste0(name, ".png")), plot,
         width = width, height = height, dpi = 300, bg = "white")
  invisible(plot)
}

# Prints a table to the console under a banner, so the whole run reads like a
# meeting handout.
report <- function(x, title, n = 100) {
  cat("\n", strrep("-", 70), "\n", title, "\n", strrep("-", 70), "\n", sep = "")
  d <- as.data.frame(x)
  print(utils::head(d, n), row.names = FALSE, right = FALSE)
  if (nrow(d) > n) cat("... ", nrow(d) - n, " more row(s); see the CSV\n", sep = "")
  invisible(x)
}

# ---------------------------------------------------------------------------
# Helpers for multi-select (checkbox) fields
# ---------------------------------------------------------------------------
# Google Forms joins checked options with ", ". Two option labels in the
# deployed form contain their own commas, so a naive split on "," shatters them:
#   - "Containerization (e.g., Docker, Singularity, CodeOcean, Binder, ...)"  [E1b]
#   - "After acceptance, before publication" and
#     "Before submission, proactively"                                        [F3]
# The first case is handled generically by masking commas inside parentheses;
# the second by protecting the literal labels before splitting.

SENTINEL <- ""

PROTECTED_OPTIONS <- c(
  "After acceptance, before publication",
  "Before submission, proactively"
)

# Replaces commas that sit inside parentheses so they survive the split.
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

# Counts checked options across papers. Case-variant spellings of the same
# free-text entry (e.g. "AI Agents" vs "AI agents") are merged onto the most
# frequent spelling; genuinely distinct entries are left alone.
count_multi <- function(df, col) {
  tokens    <- split_multi(df[[col]])
  n_answered <- sum(lengths(tokens) > 0)
  tibble(paper_row = rep(seq_len(nrow(df)), lengths(tokens)),
         option    = unlist(tokens)) |>
    mutate(key = str_to_lower(option)) |>
    add_count(key, option, name = "spelling_n") |>
    group_by(key) |>
    mutate(option = option[which.max(spelling_n)]) |>
    ungroup() |>
    count(option, name = "n_papers", sort = TRUE) |>
    mutate(n_answered     = n_answered,
           pct_of_answered = round(100 * n_papers / n_answered, 1))
}

# Long Google Forms labels ("Yes. The paper explicitly notes that ...") are cut
# back to their first sentence for plotting. Splits on a period followed by a
# space, so abbreviations inside a label ("User study (e.g., survey ...)")
# survive intact.
short_label <- function(x, max_chars = 48) {
  first <- str_extract(x, "^.*?\\.(?=\\s)")
  first <- coalesce(first, x)
  str_trunc(str_squish(first), max_chars)
}

# ===========================================================================
# 1. LOAD
# ===========================================================================

descriptive <- read_csv(here("dataset", "descriptive_coding.csv"),
                        show_col_types = FALSE)
b1 <- read_csv(here("dataset", "b1_criteria_extraction.csv"),
               show_col_types = FALSE)
b2 <- read_csv(here("dataset", "b2_criteria_extraction.csv"),
               show_col_types = FALSE)

corpus <- read_excel(here("output", "search_output", "10_final_set.xlsx"),
                     sheet = "Literature") |>
  filter(!is.na(`No.`)) |>
  transmute(paper_id = as.integer(`No.`),
            year     = suppressWarnings(as.integer(Year)),
            type     = Type)

# get_descriptive_coding.R already writes the codebook variable codes (B1, B1A,
# C1, ...) as column names. The rename below is a no-op on those files and only
# does work on older exports that still carry the full Google Forms question
# text; anything without a leading code keeps its name.
code_from_question <- function(nm) {
  code <- str_match(nm, "^([A-Fa-f][0-9][a-z]?)\\.")[, 2]
  ifelse(is.na(code), nm, str_to_upper(code))
}
names(descriptive) <- code_from_question(names(descriptive))
if ("Paper title" %in% names(descriptive)) {
  descriptive <- descriptive |> rename(paper_title = `Paper title`)
}

# get_criteria_extraction.R writes `paper_id`; older hand-exports of the coding
# workbook carried the sheet's own `paper ID` header.
ensure_paper_id <- function(d) {
  if (!"paper_id" %in% names(d)) d <- d |> mutate(paper_id = `paper ID`)
  d |> mutate(paper_id = as.integer(paper_id))
}
b1 <- ensure_paper_id(b1)
b2 <- ensure_paper_id(b2)

# ===========================================================================
# 2. COVERAGE AND DATA QUALITY
# ===========================================================================

cat("\n", strrep("=", 70), "\n",
    "SocEnRep Step 5 - descriptive analysis of Amelia's coding\n",
    "Corpus: ", N_CORPUS, " papers | run: ", format(Sys.Date(), "%d.%m.%Y"), "\n",
    strrep("=", 70), "\n", sep = "")

coverage <- tibble(
  part = c("A: descriptive coding", "B1: operationalizations", "B2: criteria"),
  papers_covered = c(nrow(descriptive),
                     n_distinct(b1$paper_id),
                     n_distinct(b2$paper_id)),
  rows = c(nrow(descriptive), nrow(b1), nrow(b2))
) |>
  mutate(pct_of_corpus = round(100 * papers_covered / N_CORPUS, 1))

report(coverage, "CODING COVERAGE")
save_table(coverage, "00_coverage")

# Papers with no criteria extracted at all. 
no_criteria <- corpus |>
  filter(!paper_id %in% b2$paper_id) |>
  arrange(paper_id)
cat("\nPapers with no B2 criterion extracted: ", nrow(no_criteria),
    " (IDs: ", paste(no_criteria$paper_id, collapse = ", "), ")\n", sep = "")
save_table(no_criteria, "00_papers_without_criteria")

# The category is recoverable from the criterion label, which maps 1:1 onto a
# dimension everywhere else in the data. Fill it in, but say so out loud.
label_lookup <- b2 |>
  filter(!is.na(`criterion category`), `criterion label` != "new criterion") |>
  distinct(`criterion label`, `criterion category`)

missing_cat <- b2 |> filter(is.na(`criterion category`))
if (nrow(missing_cat) > 0) {
  cat("\nNOTE: ", nrow(missing_cat), " B2 row(s) have a blank criterion category ",
      "(paper ID ", paste(missing_cat$paper_id, collapse = ", "), "). ",
      "Filled from the criterion label.\n", sep = "")
}
# Written unconditionally: a flag table that is only saved when it has rows goes
# stale on disk once the underlying issue is fixed.
save_table(missing_cat, "00_flag_blank_category")

b2 <- b2 |>
  left_join(label_lookup, by = "criterion label", suffix = c("", "_lookup")) |>
  mutate(`criterion category` = coalesce(`criterion category`, `criterion category_lookup`)) |>
  select(-`criterion category_lookup`)

b2 <- b2 |>
  mutate(
    dimension = factor(`criterion category`,
                       levels = c("Availability", "Executability", "Consistency")),
    # "new criterion" rows carry their actual label in a separate column
    criterion = if_else(`criterion label` == "new criterion",
                        str_squish(`new criterion label`),
                        `criterion label`),
    is_new = `criterion label` == "new criterion"
  ) |>
  left_join(corpus, by = "paper_id")

# ===========================================================================
# 3. PART B2 - CRITERIA (the core of the meeting)
# ===========================================================================

cat("\n\n", strrep("=", 70), "\n", "PART B2: REPRODUCIBILITY CRITERIA\n",
    strrep("=", 70), "\n", sep = "")

cat("\nTotal criterion mentions extracted: ", nrow(b2), "\n",
    "Papers contributing at least one:    ", n_distinct(b2$paper_id), "\n",
    "Distinct criterion labels:           ", n_distinct(b2$criterion), "\n",
    "  of which established labels:       ", n_distinct(b2$criterion[!b2$is_new]), "\n",
    "  of which newly coined by coder:    ", n_distinct(b2$criterion[b2$is_new]), "\n", sep = "")

# --- 3.1 criteria per paper ------------------------------------------------
per_paper <- b2 |>
  count(paper_id, name = "n_criteria") |>
  right_join(tibble(paper_id = corpus$paper_id), by = "paper_id") |>
  mutate(n_criteria = replace_na(n_criteria, 0L))

per_paper_summary <- per_paper |>
  summarise(
    papers      = n(),
    mean        = round(mean(n_criteria), 2),
    median      = median(n_criteria),
    sd          = round(sd(n_criteria), 2),
    min         = min(n_criteria),
    max         = max(n_criteria),
    zero_papers = sum(n_criteria == 0)
  )
report(per_paper_summary, "CRITERIA PER PAPER")
save_table(per_paper_summary, "01_criteria_per_paper_summary")
save_table(per_paper, "01_criteria_per_paper")

p_per_paper <- ggplot(per_paper, aes(n_criteria)) +
  geom_histogram(binwidth = 1, fill = "#4C72B0", colour = "white") +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(title = "How many criteria are extracted per paper?",
       subtitle = paste0("All ", N_CORPUS, " papers in the final set; ",
                         per_paper_summary$zero_papers,
                         " papers yielded no criterion"),
       x = "Criteria extracted", y = "Papers") +
  theme_socenrep()
save_fig(p_per_paper, "01_criteria_per_paper")

# --- 3.2 by dimension ------------------------------------------------------
by_dim <- b2 |>
  group_by(dimension) |>
  summarise(mentions = n(), papers = n_distinct(paper_id), .groups = "drop") |>
  mutate(pct_mentions = round(100 * mentions / sum(mentions), 1),
         pct_papers   = round(100 * papers / N_CORPUS, 1)) |>
  arrange(desc(mentions))
report(by_dim, "CRITERIA BY DIMENSION")
save_table(by_dim, "02_criteria_by_dimension")

p_dim <- ggplot(by_dim, aes(fct_reorder(dimension, mentions), mentions, fill = dimension)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(mentions, " (", pct_mentions, "%)")),
            hjust = -0.15, size = 3.4) +
  coord_flip() +
  scale_fill_manual(values = DIM_COLOURS, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(title = "Criterion mentions by reproducibility dimension",
       subtitle = paste0(nrow(b2), " mentions across ",
                         n_distinct(b2$paper_id), " papers"),
       x = NULL, y = "Mentions") +
  theme_socenrep()
save_fig(p_dim, "02_criteria_by_dimension", height = 3.2)

# --- 3.3 by criterion, grouped within dimension ----------------------------
by_criterion <- b2 |>
  group_by(dimension, criterion, is_new) |>
  summarise(mentions = n(), papers = n_distinct(paper_id), .groups = "drop") |>
  mutate(pct_papers = round(100 * papers / N_CORPUS, 1)) |>
  arrange(dimension, desc(mentions))
by_criterion |>
  mutate(criterion = str_trunc(criterion, 52)) |>
  report("CRITERIA BY LABEL, GROUPED BY DIMENSION")
save_table(by_criterion, "03_criteria_by_label")

p_criterion <- by_criterion |>
  mutate(criterion = str_trunc(criterion, 46)) |>
  ggplot(aes(fct_reorder(criterion, mentions), mentions, fill = dimension)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = mentions), hjust = -0.25, size = 3) +
  coord_flip() +
  facet_grid(dimension ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = DIM_COLOURS, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Which criteria are invoked, and how often?",
       subtitle = "Mentions per criterion, grouped by dimension",
       caption = "Criteria coded as 'new' were coined during coding; see table 04.",
       x = NULL, y = "Mentions") +
  theme_socenrep()
save_fig(p_criterion, "03_criteria_by_label", height = 8)

# --- 3.4 the new criteria --------------------------------------------------
new_criteria <- b2 |>
  filter(is_new) |>
  group_by(dimension, criterion) |>
  summarise(mentions = n(), papers = n_distinct(paper_id), .groups = "drop") |>
  arrange(desc(mentions))
new_criteria |>
  mutate(criterion = str_trunc(criterion, 52)) |>
  report("NEW CRITERIA COINED DURING CODING")
save_table(new_criteria, "04_new_criteria")

# --- 3.5 normative framing -------------------------------------------------
framing <- b2 |>
  count(dimension, `normative framing`, name = "mentions") |>
  group_by(dimension) |>
  mutate(pct_within_dimension = round(100 * mentions / sum(mentions), 1)) |>
  ungroup()
report(framing, "NORMATIVE FRAMING BY DIMENSION")
save_table(framing, "05_normative_framing")

framing_overall <- b2 |>
  count(`normative framing`, name = "mentions", sort = TRUE) |>
  mutate(pct = round(100 * mentions / sum(mentions), 1))
report(framing_overall, "NORMATIVE FRAMING OVERALL")
save_table(framing_overall, "05_normative_framing_overall")

p_framing <- ggplot(framing, aes(dimension, mentions, fill = `normative framing`)) +
  geom_col(position = "fill", width = 0.65) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "Set2", name = "Framing") +
  labs(title = "How are criteria framed?",
       subtitle = "Share of mentions per dimension",
       x = NULL, y = NULL) +
  theme_socenrep() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey92"))
save_fig(p_framing, "05_normative_framing", height = 4)

# --- 3.6 automation --------------------------------------------------------
automation <- b2 |>
  mutate(`can be automated` = replace_na(`can be automated`, "(not coded)")) |>
  count(dimension, `can be automated`, name = "mentions") |>
  group_by(dimension) |>
  mutate(pct_within_dimension = round(100 * mentions / sum(mentions), 1)) |>
  ungroup()
report(automation, "AUTOMATION POTENTIAL BY DIMENSION")
save_table(automation, "06_automation_by_dimension")

automation_by_criterion <- b2 |>
  filter(`can be automated` == "yes") |>
  count(dimension, criterion, name = "mentions_automatable", sort = TRUE)
automation_by_criterion |>
  mutate(criterion = str_trunc(criterion, 52)) |>
  report("CRITERIA JUDGED AUTOMATABLE")
save_table(automation_by_criterion, "06_automatable_criteria")

llm <- b2 |>
  filter(!is.na(`LLM involved`)) |>
  count(dimension, `LLM involved`, name = "mentions")
report(llm, "LLM INVOLVEMENT (only where automation was coded)")
save_table(llm, "06_llm_involvement")

p_auto <- ggplot(automation, aes(dimension, mentions, fill = `can be automated`)) +
  geom_col(position = "fill", width = 0.65) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("yes" = "#55A868", "no" = "#C44E52",
                               "unclear" = "grey75", "(not coded)" = "grey90"),
                    name = "Can be automated") +
  labs(title = "Can the criterion be automated?",
       subtitle = "As judged by the paper, share of mentions per dimension",
       caption = "'unclear' dominates: most papers do not state whether a criterion can be checked automatically.",
       x = NULL, y = NULL) +
  theme_socenrep() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey92"))
save_fig(p_auto, "06_automation", height = 4)

# --- 3.7 dependency structure (counts only; the DAG comes later) -----------
dependency_counts <- b2 |>
  summarise(
    mentions_total          = n(),
    with_prerequisite_field = sum(!is.na(`is prerequisite for`) & nzchar(`is prerequisite for`)),
    with_depends_on_field   = sum(!is.na(`depends on`) & nzchar(`depends on`))
  )
report(dependency_counts, "DEPENDENCY FIELDS FILLED")
save_table(dependency_counts, "07_dependency_field_counts")

# Edge list, so the meeting can see which links are asserted most often.
edges <- b2 |>
  select(paper_id, from = criterion, to = `is prerequisite for`) |>
  filter(!is.na(to), nzchar(to)) |>
  mutate(to = str_split(to, ";")) |>
  unnest(to) |>
  mutate(to = str_squish(to)) |>
  filter(nzchar(to)) |>
  count(from, to, name = "times_asserted", sort = TRUE)
report(head(edges, 25), "MOST FREQUENTLY ASSERTED PREREQUISITE LINKS (top 25)")
save_table(edges, "07_dependency_edges")

# The dependency fields are free text, so a target can be spelled differently
# from the controlled criterion label (e.g. "table/figure match" vs
# "table and figure match"). These need harmonising before the DAG is built.
known_criteria <- sort(unique(b2$criterion))
unmatched_targets <- edges |>
  group_by(to) |>
  summarise(times_asserted = sum(times_asserted), .groups = "drop") |>
  filter(!to %in% known_criteria) |>
  arrange(desc(times_asserted))
if (nrow(unmatched_targets) > 0) {
  report(unmatched_targets,
         "DEPENDENCY TARGETS THAT DO NOT MATCH A CRITERION LABEL (harmonise before the DAG)")
} else {
  cat("\nAll dependency targets match a criterion label - the DAG can be built ",
      "straight from 07_dependency_edges.csv.\n", sep = "")
}
save_table(unmatched_targets, "07_flag_unmatched_dependency_targets")

# --- 3.8 criteria over publication year ------------------------------------
by_year <- b2 |>
  filter(!is.na(year)) |>
  group_by(year, dimension) |>
  summarise(mentions = n(), .groups = "drop")
save_table(by_year, "08_criteria_by_year")

p_year <- ggplot(by_year, aes(year, mentions, fill = dimension)) +
  geom_col(width = 0.8) +
  scale_fill_manual(values = DIM_COLOURS, name = NULL) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(title = "Criterion mentions by publication year",
       subtitle = "Reflects both the literature and the corpus composition",
       x = NULL, y = "Mentions") +
  theme_socenrep() +
  theme(panel.grid.major.y = element_line(colour = "grey92"))
save_fig(p_year, "08_criteria_by_year", height = 4)

# ===========================================================================
# 4. PART B1 - OPERATIONALIZATIONS
# ===========================================================================

cat("\n\n", strrep("=", 70), "\n", "PART B1: OPERATIONALIZATION OF SUCCESS\n",
    strrep("=", 70), "\n", sep = "")

cat("\nRows: ", nrow(b1), " across ", n_distinct(b1$paper_id), " papers\n",
    "(a paper contributes more than one row when it reports several ",
    "operationalizations)\n", sep = "")

# A handful of papers report several operationalizations and so contribute more
# than one row per category; counts below are rows, not papers.
n_multi_op <- b1 |>
  filter(`operationalization category` == "Classification") |>
  count(paper_id) |>
  filter(n > 1) |>
  nrow()

b1_by_category <- b1 |>
  filter(`operationalization category` != "Statistical criterion") |>
  count(`operationalization category`, `operationalization value`,
        name = "operationalizations") |>
  group_by(`operationalization category`) |>
  mutate(pct = round(100 * operationalizations / sum(operationalizations), 1)) |>
  ungroup() |>
  arrange(`operationalization category`, desc(operationalizations))
report(b1_by_category, "OPERATIONALIZATION VALUES BY CATEGORY")
save_table(b1_by_category, "10_operationalizations")

p_b1 <- b1_by_category |>
  mutate(`operationalization value` = fct_reorder(`operationalization value`,
                                                  operationalizations)) |>
  ggplot(aes(`operationalization value`, operationalizations,
             fill = `operationalization value` == "not specified")) +
  geom_col(width = 0.7) +
  geom_text(aes(label = operationalizations), hjust = -0.25, size = 3) +
  coord_flip() +
  facet_wrap(~`operationalization category`, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = c("TRUE" = "grey75", "FALSE" = "#4C72B0"),
                    guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "How do papers operationalize reproducibility success?",
       subtitle = "Grey bars = 'not specified'",
       caption = paste0("Counts are operationalizations, not papers: ", n_multi_op,
                        " papers report more than one and contribute several rows."),
       x = NULL, y = "Operationalizations") +
  theme_socenrep()
save_fig(p_b1, "10_operationalizations", height = 6.5)

# Papers that specify something on all three axes are the ones with a fully
# articulated definition of success.
b1_specified <- b1 |>
  filter(`operationalization category` != "Statistical criterion") |>
  group_by(paper_id) |>
  summarise(axes_specified = sum(`operationalization value` != "not specified"),
            .groups = "drop") |>
  count(axes_specified, name = "papers") |>
  mutate(pct = round(100 * papers / sum(papers), 1))
report(b1_specified, "HOW MANY OF THE 3 AXES ARE SPECIFIED (classification / unit / coverage)")
save_table(b1_specified, "11_axes_specified")

stat_criteria <- b1 |>
  filter(`operationalization category` == "Statistical criterion",
         !is.na(`operationalization value`), nzchar(`operationalization value`)) |>
  select(paper_id, statistical_criterion = `operationalization value`)
cat("\nPapers stating an explicit statistical criterion: ", nrow(stat_criteria), "\n", sep = "")
save_table(stat_criteria, "12_statistical_criteria")

# ===========================================================================
# 5. PART A - DESCRIPTIVE CODING
# ===========================================================================

cat("\n\n", strrep("=", 70), "\n", "PART A: DESCRIPTIVE CODING\n",
    strrep("=", 70), "\n", sep = "")

# --- single-select fields --------------------------------------------------
single_fields <- c(
  B1  = "Discipline-specific focus",
  C1  = "Primary contribution type",
  C2  = "Secondary contribution type",
  D1  = "Provides a definition of reproducibility",
  D2  = "Discusses contextual variation",
  E1  = "Describes a workflow, guideline or tool",
  E1A = "Explicitness of the workflow",
  E1E = "Automation of the check",
  E1F = "LLMs involved in the automation",
  F1  = "Recommends scoring or badging"
)

single_summary <- lapply(names(single_fields), function(v) {
  if (!v %in% names(descriptive)) return(NULL)
  descriptive |>
    count(.data[[v]], name = "papers") |>
    rename(value = 1) |>
    mutate(variable = v,
           label    = single_fields[[v]],
           value    = replace_na(as.character(value), "(not answered)"),
           pct      = round(100 * papers / nrow(descriptive), 1)) |>
    select(variable, label, value, papers, pct) |>
    arrange(desc(papers))
}) |> bind_rows()

single_summary |>
  mutate(value = short_label(value, 44)) |>
  select(variable, value, papers, pct) |>
  report("SINGLE-SELECT FIELDS (full option text in the CSV)")
save_table(single_summary, "20_descriptive_single_select")

# C1/C2 allowed a free-text "Other", so near-duplicate contribution types crept
# in ("Conceptual/review paper" vs "conceptual/review paper"). List the rare
# values so they can be folded into the main categories before the paper.
rare_single <- single_summary |>
  filter(papers <= 2, value != "(not answered)") |>
  arrange(variable, desc(papers))
rare_single |>
  mutate(value = str_trunc(value, 52)) |>
  select(variable, value, papers) |>
  report("RARE / FREE-TEXT SINGLE-SELECT VALUES TO HARMONISE")
save_table(rare_single, "20_flag_rare_single_select")

p_contrib <- descriptive |>
  filter(!is.na(C1)) |>
  count(C1, name = "papers") |>
  mutate(C1 = fct_reorder(short_label(C1, 44), papers)) |>
  ggplot(aes(C1, papers)) +
  geom_col(fill = "#4C72B0", width = 0.7) +
  geom_text(aes(label = papers), hjust = -0.3, size = 3.2) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(title = "Primary type of contribution",
       subtitle = paste0(nrow(descriptive), " papers"),
       x = NULL, y = "Papers") +
  theme_socenrep()
save_fig(p_contrib, "20_contribution_type", height = 4.5)

# --- multi-select fields ---------------------------------------------------
multi_fields <- c(
  B1A = "Disciplines targeted",
  D2A = "Types of contextual variation",
  E1B = "Computational environment",
  E1C = "Programming languages",
  E1D = "Data types",
  F2  = "Who should perform the check",
  F3  = "When the check should happen"
)

multi_summary <- lapply(names(multi_fields), function(v) {
  if (!v %in% names(descriptive)) return(NULL)
  count_multi(descriptive, v) |>
    mutate(variable = v, label = multi_fields[[v]]) |>
    select(variable, label, option, n_papers, n_answered, pct_of_answered)
}) |> bind_rows()

multi_summary |>
  mutate(option = str_trunc(option, 46)) |>
  select(variable, option, n_papers, pct_of_answered) |>
  report("MULTI-SELECT FIELDS (checked options; full text in the CSV)")
save_table(multi_summary, "21_descriptive_multi_select")

# Free-text "Other" entries surface here as rare options. They need harmonising
# before these fields can be analysed properly, so list them explicitly.
rare_options <- multi_summary |>
  filter(n_papers <= 2) |>
  arrange(variable, desc(n_papers))
rare_options |>
  mutate(option = str_trunc(option, 52)) |>
  select(variable, option, n_papers) |>
  report("RARE / FREE-TEXT OPTIONS TO HARMONISE BEFORE THE NEXT ROUND")
save_table(rare_options, "21_flag_rare_options")

plot_multi <- function(v, title, height = 4) {
  d <- multi_summary |> filter(variable == v, n_papers > 1)
  if (nrow(d) == 0) return(invisible(NULL))
  p <- d |>
    mutate(option = fct_reorder(str_trunc(option, 46), n_papers)) |>
    ggplot(aes(option, n_papers)) +
    geom_col(fill = "#4C72B0", width = 0.7) +
    geom_text(aes(label = n_papers), hjust = -0.3, size = 3.2) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
    labs(title = title,
         subtitle = "Coders may select several options; entries mentioned once are omitted",
         x = NULL, y = "Papers") +
    theme_socenrep()
  save_fig(p, paste0("21_", str_to_lower(v)), height = height)
}

plot_multi("B1A", "Disciplines targeted", height = 3.5)
plot_multi("F2",  "Who should perform the reproducibility check?", height = 3.5)
plot_multi("F3",  "When should reproducibility be verified?", height = 3.5)
plot_multi("E1B", "Computational environment in focus", height = 3.5)
plot_multi("E1D", "Data types in focus", height = 3.5)

# --- corpus composition ----------------------------------------------------
corpus_summary <- corpus |>
  count(type, name = "papers", sort = TRUE) |>
  mutate(pct = round(100 * papers / sum(papers), 1))
report(corpus_summary, "CORPUS COMPOSITION BY PUBLICATION TYPE")
save_table(corpus_summary, "30_corpus_by_type")

# ===========================================================================
# 6. DONE
# ===========================================================================

cat("\n\n", strrep("=", 70), "\n",
    "Tables written to : ", TABLE_DIR, "\n",
    "Figures written to: ", FIG_DIR, "\n",
    strrep("=", 70), "\n", sep = "")
