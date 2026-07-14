#!/usr/bin/env Rscript

# P2_analysis_v1.1.R
# Interim quality-control rerun for Meta_Analysis_Input_v2.1.xlsx.
# The workbook contains locked inclusion decisions; this script never writes back to it.

analysis_version <- "P2_analysis_v1.1"
input_version <- "Meta_Analysis_Input_v2.1"
baseline_run_name <- "runP2v1_20260714_093232"
numeric_tolerance <- 1e-12
ordered_endpoint_cr2_rationale <- paste0(
  "每个 cluster 仅一条效应，标准 Hartung–Knapp 推断已适用，",
  "CR2 不适用"
)

required_packages <- c(
  "readxl", "dplyr", "tibble", "purrr", "stringr", "metafor", "clubSandwich"
)
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing R packages: ", paste(missing_packages, collapse = ", "), "\n",
      "Install them with:\ninstall.packages(c(",
      paste(sprintf("\"%s\"", missing_packages), collapse = ", "), "))"
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tibble)
  library(purrr)
  library(stringr)
  library(metafor)
  library(clubSandwich)
})

locate_script <- function() {
  command_line <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command_line, value = TRUE)
  if (length(file_arg) > 0) {
    return(sub("^--file=", "", file_arg[[1]]))
  }
  frames <- sys.frames()
  source_files <- vapply(
    frames,
    function(frame) if (!is.null(frame$ofile)) as.character(frame$ofile) else NA_character_,
    character(1)
  )
  source_files <- source_files[!is.na(source_files) & nzchar(source_files)]
  if (length(source_files) > 0) {
    return(tail(source_files, 1))
  }
  NA_character_
}

locate_project_root <- function(start) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    has_r_project <- length(list.files(current, pattern = "\\.Rproj$")) > 0
    if (dir.exists(file.path(current, ".git")) || has_r_project) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      return(NA_character_)
    }
    current <- parent
  }
}

resolve_local_path <- function(path, base = getwd(), must_work = FALSE) {
  path <- path.expand(trimws(path))
  is_absolute <- grepl("^/", path) ||
    grepl("^[A-Za-z]:[/\\\\]", path) ||
    grepl("^\\\\\\\\", path)
  if (!is_absolute) {
    path <- file.path(base, path)
  }
  normalizePath(path, winslash = "/", mustWork = must_work)
}

named_option <- function(name, args = commandArgs(trailingOnly = TRUE)) {
  prefix <- paste0("--", name, "=")
  matches <- args[startsWith(args, prefix)]
  if (length(matches) > 1) {
    stop("Specify ", prefix, " only once.", call. = FALSE)
  }
  if (length(matches) == 0) {
    return(NA_character_)
  }
  value <- substring(matches[[1]], nchar(prefix) + 1L)
  if (!nzchar(trimws(value))) {
    stop(prefix, " requires a non-empty path.", call. = FALSE)
  }
  value
}

script_path <- locate_script()
script_dir <- if (!is.na(script_path)) {
  dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
project_root <- locate_project_root(script_dir)

input_cli <- named_option("input")
input_env <- trimws(Sys.getenv("META_ANALYSIS_INPUT", unset = ""))
private_input <- if (!is.na(project_root)) {
  file.path(project_root, "data", "private", paste0(input_version, ".xlsx"))
} else {
  NA_character_
}
same_folder_input <- file.path(script_dir, paste0(input_version, ".xlsx"))

if (!is.na(input_cli)) {
  input_path <- resolve_local_path(input_cli)
  input_origin <- "--input"
} else if (nzchar(input_env)) {
  input_path <- resolve_local_path(input_env)
  input_origin <- "META_ANALYSIS_INPUT"
} else {
  fallback_inputs <- unique(na.omit(c(private_input, same_folder_input)))
  fallback_inputs <- vapply(
    fallback_inputs,
    resolve_local_path,
    character(1),
    must_work = FALSE
  )
  existing_inputs <- fallback_inputs[file.exists(fallback_inputs)]
  input_path <- if (length(existing_inputs) > 0) existing_inputs[[1]] else NA_character_
  input_origin <- if (
    !is.na(input_path) &&
      !is.na(private_input) &&
      identical(input_path, resolve_local_path(private_input))
  ) {
    "repository private-data fallback"
  } else {
    "script-folder compatibility fallback"
  }
}

if (is.na(input_path) || !file.exists(input_path)) {
  attempted <- unique(na.omit(c(
    if (!is.na(input_cli)) resolve_local_path(input_cli) else NA_character_,
    if (nzchar(input_env)) resolve_local_path(input_env) else NA_character_,
    if (!is.na(private_input)) resolve_local_path(private_input) else NA_character_,
    resolve_local_path(same_folder_input)
  )))
  stop(
    paste0(
      "Input workbook not found. Configured and fallback paths:\n- ",
      paste(attempted, collapse = "\n- "), "\n",
      "Use --input=/absolute/path/Meta_Analysis_Input_v2.1.xlsx, set ",
      "META_ANALYSIS_INPUT, place the workbook in data/private/, or place it ",
      "beside P2_analysis_v1.1.R."
    ),
    call. = FALSE
  )
}
input_path <- normalizePath(input_path, winslash = "/", mustWork = TRUE)

output_cli <- named_option("output-dir")
output_env <- trimws(Sys.getenv("META_ANALYSIS_OUTPUT_DIR", unset = ""))
default_output_root <- if (!is.na(project_root)) {
  file.path(project_root, "results")
} else {
  script_dir
}
output_root <- if (!is.na(output_cli)) {
  resolve_local_path(output_cli)
} else if (nzchar(output_env)) {
  resolve_local_path(output_env)
} else {
  resolve_local_path(default_output_root)
}
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(output_root)) {
  stop("Could not create output directory: ", output_root, call. = FALSE)
}
output_root <- normalizePath(output_root, winslash = "/", mustWork = TRUE)

baseline_cli <- named_option("baseline-run")
baseline_env <- trimws(Sys.getenv("P2_BASELINE_RUN_DIR", unset = ""))
default_baseline_run <- if (!is.na(project_root)) {
  file.path(project_root, "results", baseline_run_name)
} else {
  file.path(script_dir, baseline_run_name)
}
baseline_run <- if (!is.na(baseline_cli)) {
  resolve_local_path(baseline_cli)
} else if (nzchar(baseline_env)) {
  resolve_local_path(baseline_env)
} else {
  resolve_local_path(default_baseline_run)
}
baseline_summary_candidates <- c(
  file.path(baseline_run, "tables", "model_summary.csv"),
  file.path(baseline_run, "model_summary.csv")
)
baseline_summary_path <- baseline_summary_candidates[file.exists(baseline_summary_candidates)]
if (length(baseline_summary_path) == 0L) {
  stop(
    paste0(
      "Baseline model_summary.csv not found under: ", baseline_run, "\n",
      "Use --baseline-run=/absolute/path/", baseline_run_name,
      " or set P2_BASELINE_RUN_DIR."
    ),
    call. = FALSE
  )
}
baseline_summary_path <- normalizePath(
  baseline_summary_path[[1]], winslash = "/", mustWork = TRUE
)
baseline_run <- if (identical(basename(dirname(baseline_summary_path)), "tables")) {
  dirname(dirname(baseline_summary_path))
} else {
  dirname(baseline_summary_path)
}
baseline_cr2_path <- file.path(baseline_run, "tables", "CR2_decision_log.csv")
if (!file.exists(baseline_cr2_path)) {
  baseline_cr2_path <- file.path(baseline_run, "CR2_decision_log.csv")
}
if (!file.exists(baseline_cr2_path)) {
  stop("Baseline CR2_decision_log.csv not found under: ", baseline_run, call. = FALSE)
}
baseline_cr2_path <- normalizePath(baseline_cr2_path, winslash = "/", mustWork = TRUE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
run_dir <- file.path(output_root, paste0("runP2v1.1_", timestamp))
if (dir.exists(run_dir)) {
  stop("Run directory already exists: ", run_dir, call. = FALSE)
}
table_dir <- file.path(run_dir, "tables")
figure_dir <- file.path(run_dir, "figures")
model_dir <- file.path(run_dir, "models")
log_dir <- file.path(run_dir, "logs")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

read_input <- function(sheet) {
  readxl::read_excel(input_path, sheet = sheet, skip = 2, .name_repair = "unique")
}

sheet_names <- c(
  "Effect_Decision_Lock", "Main_LogOR_v2", "Main_SMD_v2", "Main_Nonlinear_v2",
  "Sensitivity_LogOR_v2", "Module_SMD_v2", "Module_Nonlinear_v2",
  "Event_Temporal_v2", "V_Matrix_A008", "Analysis_Manifest"
)
inputs <- setNames(lapply(sheet_names, read_input), sheet_names)

lock_table <- inputs$Effect_Decision_Lock
manifest <- inputs$Analysis_Manifest

validation <- tibble(
  severity = character(),
  sheet = character(),
  issue = character(),
  details = character()
)

add_validation <- function(severity, sheet, issue, details) {
  validation <<- bind_rows(
    validation,
    tibble(severity = severity, sheet = sheet, issue = issue, details = as.character(details))
  )
}

required_lock_statuses <- c(
  "LOCKED_PRIMARY", "LOCKED_SENSITIVITY", "LOCKED_ROBUSTNESS", "LOCKED_DESCRIPTIVE",
  "LOCKED_EXCLUDE_DUPLICATE", "PENDING_PRECISION"
)
expected_lock_counts <- c(
  LOCKED_PRIMARY = 21L,
  LOCKED_SENSITIVITY = 21L,
  LOCKED_ROBUSTNESS = 5L,
  LOCKED_DESCRIPTIVE = 12L,
  LOCKED_EXCLUDE_DUPLICATE = 1L,
  PENDING_PRECISION = 4L
)

if (nrow(lock_table) != 64) {
  add_validation("ERROR", "Effect_Decision_Lock", "Unexpected row count", paste("Expected 64; found", nrow(lock_table)))
} else {
  add_validation("PASS", "Effect_Decision_Lock", "Locked row count", "64 quantitative candidate rows")
}

duplicate_lock_ids <- lock_table$source_analysis_id[duplicated(lock_table$source_analysis_id)]
if (length(duplicate_lock_ids) > 0) {
  add_validation("ERROR", "Effect_Decision_Lock", "Duplicate source_analysis_id", paste(unique(duplicate_lock_ids), collapse = ", "))
} else {
  add_validation("PASS", "Effect_Decision_Lock", "Unique source_analysis_id", "No duplicates")
}

bad_lock_status <- setdiff(unique(lock_table$decision_status), required_lock_statuses)
if (length(bad_lock_status) > 0) {
  add_validation("ERROR", "Effect_Decision_Lock", "Unknown decision_status", paste(bad_lock_status, collapse = ", "))
} else {
  add_validation("PASS", "Effect_Decision_Lock", "Decision status vocabulary", "All statuses are recognized")
}

observed_lock_counts <- table(factor(
  lock_table$decision_status,
  levels = names(expected_lock_counts)
))
if (!identical(as.integer(observed_lock_counts), unname(expected_lock_counts))) {
  count_details <- paste0(
    names(expected_lock_counts),
    " expected=", unname(expected_lock_counts),
    " observed=", as.integer(observed_lock_counts)
  )
  add_validation(
    "ERROR", "Effect_Decision_Lock", "Decision status counts changed",
    paste(count_details, collapse = "; ")
  )
} else {
  add_validation(
    "PASS", "Effect_Decision_Lock", "Decision status counts",
    "21 primary; 21 sensitivity; 5 robustness; 12 descriptive; 1 duplicate exclusion; 4 pending precision"
  )
}

if (any(lock_table$decision_status == "LOCKED_EXCLUDE_DUPLICATE" & lock_table$analysis_include == "Yes", na.rm = TRUE)) {
  offenders <- lock_table$source_analysis_id[lock_table$decision_status == "LOCKED_EXCLUDE_DUPLICATE" & lock_table$analysis_include == "Yes"]
  add_validation("ERROR", "Effect_Decision_Lock", "Excluded duplicate marked included", paste(offenders, collapse = ", "))
} else {
  add_validation("PASS", "Effect_Decision_Lock", "Duplicate exclusion", "No excluded duplicate is quantitatively included")
}

a026_duplicate <- lock_table %>% filter(source_analysis_id == "P2L010")
if (
  nrow(a026_duplicate) != 1 ||
    !identical(as.character(a026_duplicate$decision_status[[1]]), "LOCKED_EXCLUDE_DUPLICATE") ||
    !identical(as.character(a026_duplicate$analysis_include[[1]]), "No")
) {
  add_validation(
    "ERROR", "Effect_Decision_Lock", "A026/P2L010 duplicate lock changed",
    "P2L010 must occur once with LOCKED_EXCLUDE_DUPLICATE and analysis_include=No"
  )
} else {
  add_validation(
    "PASS", "Effect_Decision_Lock", "A026/P2L010 duplicate lock",
    "P2L010 remains excluded as a duplicate reanalysis of A011"
  )
}

standardize_regular <- function(df, source_sheet) {
  df %>%
    transmute(
      effect_id = as.character(effect_id),
      article_id = as.character(article_id),
      study_id = as.character(study_id),
      sample_id = as.character(sample_id),
      author = as.character(author),
      year = as.numeric(year),
      module = as.character(module),
      outcome = as.character(outcome),
      contrast = as.character(contrast),
      effect_measure = as.character(effect_measure),
      yi = as.numeric(yi),
      sei = as.numeric(sei),
      vi = as.numeric(vi),
      analysis_stream = as.character(analysis_stream),
      model_id = as.character(model_id),
      analysis_include = as.character(analysis_include),
      decision_status = as.character(decision_status),
      dependency_cluster = as.character(dependency_cluster),
      shared_control_block = as.character(shared_control_block),
      source_sheet = source_sheet,
      notes = as.character(notes)
    )
}

standardize_nonlinear <- function(df, source_sheet) {
  df %>%
    transmute(
      effect_id = as.character(effect_id),
      article_id = as.character(article_id),
      study_id = as.character(study_id),
      sample_id = as.character(sample_id),
      author = as.character(author),
      year = as.numeric(year),
      module = as.character(module),
      outcome = as.character(outcome),
      contrast = "Quadratic PE term",
      effect_measure = "quadratic coefficient",
      yi = as.numeric(beta_quadratic),
      sei = as.numeric(se_quadratic),
      vi = as.numeric(vi_quadratic),
      analysis_stream = as.character(analysis_stream),
      model_id = as.character(model_id),
      analysis_include = as.character(analysis_include),
      decision_status = as.character(decision_status),
      dependency_cluster = as.character(dependency_cluster),
      shared_control_block = NA_character_,
      source_sheet = source_sheet,
      notes = as.character(notes)
    )
}

standardize_event <- function(df) {
  df %>%
    transmute(
      effect_id = as.character(event_id),
      article_id = as.character(article_id),
      study_id = as.character(study_id),
      sample_id = as.character(dependency_cluster),
      author = as.character(author),
      year = as.numeric(year),
      module = "B_Event_Temporal",
      outcome = as.character(outcome),
      contrast = paste(as.character(condition_1), "vs", as.character(condition_2)),
      effect_measure = "Gaussian coefficient",
      yi = as.numeric(yi),
      sei = as.numeric(sei),
      vi = as.numeric(vi),
      analysis_stream = as.character(analysis_role),
      model_id = as.character(model_id),
      analysis_include = as.character(analysis_include),
      decision_status = as.character(decision_status),
      dependency_cluster = as.character(dependency_cluster),
      shared_control_block = NA_character_,
      source_sheet = "Event_Temporal_v2",
      notes = as.character(notes)
    )
}

a020_expected_clusters <- c(
  LOGOR_006 = "A020_CHILD",
  LOGOR_007 = "A020_YOUNG",
  LOGOR_008 = "A020_OLDER"
)
a020_effect_ids <- names(a020_expected_clusters)
a020_raw_snapshot <- inputs$Main_LogOR_v2 %>%
  transmute(
    effect_id = as.character(effect_id),
    dependency_cluster = as.character(dependency_cluster),
    yi = as.numeric(yi),
    sei = as.numeric(sei),
    vi = as.numeric(vi)
  ) %>%
  filter(effect_id %in% a020_effect_ids) %>%
  arrange(match(effect_id, a020_effect_ids))

effects <- bind_rows(
  standardize_regular(inputs$Main_LogOR_v2, "Main_LogOR_v2"),
  standardize_regular(inputs$Main_SMD_v2, "Main_SMD_v2"),
  standardize_nonlinear(inputs$Main_Nonlinear_v2, "Main_Nonlinear_v2"),
  standardize_regular(inputs$Sensitivity_LogOR_v2, "Sensitivity_LogOR_v2"),
  standardize_regular(inputs$Module_SMD_v2, "Module_SMD_v2"),
  standardize_nonlinear(inputs$Module_Nonlinear_v2, "Module_Nonlinear_v2"),
  standardize_event(inputs$Event_Temporal_v2)
)

a020_combined_snapshot <- effects %>%
  filter(effect_id %in% a020_effect_ids) %>%
  select(effect_id, dependency_cluster, yi, sei, vi) %>%
  arrange(match(effect_id, a020_effect_ids))

a020_rows_ok <-
  nrow(a020_raw_snapshot) == length(a020_effect_ids) &&
  identical(a020_raw_snapshot$effect_id, a020_effect_ids) &&
  nrow(a020_combined_snapshot) == length(a020_effect_ids) &&
  identical(a020_combined_snapshot$effect_id, a020_effect_ids)
if (!a020_rows_ok) {
  add_validation(
    "ERROR", "Main_LogOR_v2", "A020 effect IDs changed",
    "LOGOR_006, LOGOR_007, and LOGOR_008 must each occur exactly once"
  )
} else {
  add_validation(
    "PASS", "Main_LogOR_v2", "A020 effect IDs",
    "LOGOR_006, LOGOR_007, and LOGOR_008 each occur exactly once"
  )
}

a020_mapping_ok <- a020_rows_ok && identical(
  a020_raw_snapshot$dependency_cluster,
  unname(a020_expected_clusters[a020_raw_snapshot$effect_id])
)
if (!a020_mapping_ok) {
  observed_mapping <- paste0(
    a020_raw_snapshot$effect_id, "=", a020_raw_snapshot$dependency_cluster,
    collapse = "; "
  )
  add_validation(
    "ERROR", "Main_LogOR_v2", "A020 dependency_cluster mapping changed",
    observed_mapping
  )
} else {
  add_validation(
    "PASS", "Main_LogOR_v2", "A020 dependency_cluster mapping",
    "LOGOR_006=A020_CHILD; LOGOR_007=A020_YOUNG; LOGOR_008=A020_OLDER"
  )
}

a020_clusters_independent <- a020_rows_ok &&
  dplyr::n_distinct(a020_raw_snapshot$dependency_cluster) == 3L
if (!a020_clusters_independent) {
  add_validation(
    "ERROR", "Main_LogOR_v2", "A020 clusters are not independent",
    "The three A020 dependency_cluster values must be non-missing and pairwise distinct"
  )
} else {
  add_validation(
    "PASS", "Main_LogOR_v2", "A020 independent clusters",
    "Three effects map to three pairwise-distinct dependency clusters"
  )
}

a020_precision_unchanged <- a020_rows_ok &&
  identical(a020_raw_snapshot$yi, a020_combined_snapshot$yi) &&
  identical(a020_raw_snapshot$sei, a020_combined_snapshot$sei) &&
  identical(a020_raw_snapshot$vi, a020_combined_snapshot$vi)
if (!a020_precision_unchanged) {
  add_validation(
    "ERROR", "Main_LogOR_v2", "A020 precision values changed in script",
    "yi, sei, and vi must pass directly from the workbook into the combined effects table"
  )
} else {
  add_validation(
    "PASS", "Main_LogOR_v2", "A020 precision values preserved",
    "yi, sei, and vi are read directly and remain byte-for-byte identical; none is recalculated or overwritten"
  )
}

duplicate_effect_ids <- effects$effect_id[duplicated(effects$effect_id)]
if (length(duplicate_effect_ids) > 0) {
  add_validation("ERROR", "Combined effects", "Duplicate effect_id across analysis sheets", paste(unique(duplicate_effect_ids), collapse = ", "))
} else {
  add_validation("PASS", "Combined effects", "Unique effect_id", "No duplicated quantitative row across analysis sheets")
}

valid_include <- c("Yes", "No")
for (sheet_name in unique(effects$source_sheet)) {
  d <- effects %>% filter(source_sheet == sheet_name)
  bad_values <- setdiff(unique(d$analysis_include), valid_include)
  if (length(bad_values) > 0 || any(is.na(d$analysis_include))) {
    add_validation("ERROR", sheet_name, "Invalid analysis_include", paste(c(bad_values, "NA"[any(is.na(d$analysis_include))]), collapse = ", "))
  } else {
    add_validation("PASS", sheet_name, "Explicit analysis_include", "All rows are Yes or No")
  }
  included <- d %>% filter(analysis_include == "Yes")
  bad_numeric <- included %>% filter(!is.finite(yi) | !is.finite(sei) | !is.finite(vi) | sei <= 0 | vi <= 0)
  if (nrow(bad_numeric) > 0) {
    add_validation("ERROR", sheet_name, "Included row lacks finite positive precision", paste(bad_numeric$effect_id, collapse = ", "))
  } else {
    add_validation("PASS", sheet_name, "Included precision", paste(nrow(included), "included rows have finite yi/sei/vi"))
  }
  bad_variance <- included %>% filter(is.finite(sei), is.finite(vi), abs(vi - sei^2) > pmax(1e-8, 1e-5 * abs(vi)))
  if (nrow(bad_variance) > 0) {
    add_validation("ERROR", sheet_name, "vi differs from sei^2", paste(bad_variance$effect_id, collapse = ", "))
  } else {
    add_validation("PASS", sheet_name, "Variance identity", "vi agrees with sei^2 within tolerance")
  }
}

manifest_check <- manifest %>%
  filter(run_model == "Yes") %>%
  transmute(model_id = as.character(model_id), expected_k = as.integer(expected_k)) %>%
  left_join(
    effects %>% filter(analysis_include == "Yes") %>% count(model_id, name = "observed_k"),
    by = "model_id"
  ) %>%
  mutate(observed_k = ifelse(is.na(observed_k), 0L, observed_k), match = expected_k == observed_k)

if (any(!manifest_check$match)) {
  mismatches <- manifest_check %>% filter(!match) %>% mutate(x = paste0(model_id, " expected=", expected_k, " observed=", observed_k)) %>% pull(x)
  add_validation("ERROR", "Analysis_Manifest", "Expected k mismatch", paste(mismatches, collapse = "; "))
} else {
  add_validation("PASS", "Analysis_Manifest", "Expected k", "All model blocks match the frozen manifest")
}

a008_effects <- effects %>%
  filter(
    analysis_include == "Yes",
    model_id == "SENS_SHORTTERM_SOURCE_MV"
  )
if (nrow(a008_effects) != 5) {
  add_validation(
    "ERROR", "Sensitivity_LogOR_v2", "A008 effect count changed",
    paste("SENS_SHORTTERM_SOURCE_MV must contain 5 effects; found", nrow(a008_effects))
  )
} else {
  add_validation(
    "PASS", "Sensitivity_LogOR_v2", "A008 effect count",
    "SENS_SHORTTERM_SOURCE_MV contains the locked 5 effects"
  )
}

a008_missing_cluster <- any(
  is.na(a008_effects$shared_control_block) |
    !nzchar(trimws(a008_effects$shared_control_block))
)
a008_control_clusters <- n_distinct(na.omit(a008_effects$shared_control_block))
if (a008_missing_cluster || a008_control_clusters != 2) {
  add_validation(
    "ERROR", "Sensitivity_LogOR_v2", "A008 shared-control structure changed",
    paste(
      "A008 requires a non-missing shared-control block for each effect and exactly 2 blocks; found",
      a008_control_clusters
    )
  )
} else {
  add_validation(
    "PASS", "Sensitivity_LogOR_v2", "A008 shared-control structure",
    "Two shared-control blocks; CR2 remains below the locked four-cluster threshold"
  )
}

write.csv(lock_table, file.path(table_dir, "effect_selection_audit.csv"), row.names = FALSE, na = "")
write.csv(manifest_check, file.path(table_dir, "manifest_k_check.csv"), row.names = FALSE, na = "")
write.csv(validation, file.path(table_dir, "analysis_validation.csv"), row.names = FALSE, na = "")

if (any(validation$severity == "ERROR")) {
  stop(
    paste0(
      "Input validation failed. See ", file.path(table_dir, "analysis_validation.csv"),
      " for exact effect IDs and columns. No models were run."
    ),
    call. = FALSE
  )
}

included_effects <- effects %>% filter(analysis_include == "Yes")
write.csv(included_effects, file.path(table_dir, "included_effects_long.csv"), row.names = FALSE, na = "")

model_summary <- tibble()
diagnostic_log <- tibble(model_id = character(), diagnostic = character(), status = character(), details = character())
cr2_log <- tibble(
  model_id = character(),
  k = integer(),
  independent_clusters = integer(),
  repeated_effect_clusters = integer(),
  decision = character(),
  rationale = character()
)
cr2_results <- tibble(
  model_id = character(),
  k = integer(),
  independent_clusters = integer(),
  coefficient = character(),
  estimate = numeric(),
  se = numeric(),
  t_stat = numeric(),
  df_satterthwaite = numeric(),
  p_value = numeric(),
  ci_lb = numeric(),
  ci_ub = numeric(),
  vcov = character(),
  test = character()
)
pubbias_log <- tibble(model_id = character(), k = integer(), decision = character(), details = character())
model_objects <- list()

single_effect_summary <- function(d, model_id) {
  z <- d$yi[[1]] / d$sei[[1]]
  tibble(
    model_id = model_id,
    k = 1L,
    method = "Descriptive single effect",
    estimate = d$yi[[1]],
    se = d$sei[[1]],
    ci_lb = d$yi[[1]] - 1.96 * d$sei[[1]],
    ci_ub = d$yi[[1]] + 1.96 * d$sei[[1]],
    p_value = 2 * pnorm(abs(z), lower.tail = FALSE),
    tau2 = NA_real_,
    I2 = NA_real_,
    Q = NA_real_,
    Q_p = NA_real_,
    inference = "Normal approximation for the archived single coefficient; no pooled inference"
  )
}

fit_hk_block <- function(d, model_id) {
  if (nrow(d) < 2) return(list(fit = NULL, summary = single_effect_summary(d, model_id), kind = "single"))
  fit <- metafor::rma.uni(yi = yi, vi = vi, data = d, method = "REML", test = "knha")
  out <- tibble(
    model_id = model_id,
    k = nrow(d),
    method = "REML random effects",
    estimate = as.numeric(fit$b),
    se = as.numeric(fit$se),
    ci_lb = as.numeric(fit$ci.lb),
    ci_ub = as.numeric(fit$ci.ub),
    p_value = as.numeric(fit$pval),
    tau2 = as.numeric(fit$tau2),
    I2 = as.numeric(fit$I2),
    Q = as.numeric(fit$QE),
    Q_p = as.numeric(fit$QEp),
    inference = "Hartung-Knapp"
  )
  list(fit = fit, summary = out, kind = "rma_uni")
}

fit_known_v_block <- function(d, model_id, v_long) {
  effect_order <- d$effect_id
  if (length(effect_order) != 5) {
    stop("A008 requires exactly five effects in its known 5x5 sampling V matrix.", call. = FALSE)
  }
  v_relevant <- v_long %>%
    transmute(
      row_effect_id = as.character(row_effect_id),
      col_effect_id = as.character(col_effect_id),
      covariance = as.numeric(covariance)
    ) %>%
    filter(row_effect_id %in% effect_order, col_effect_id %in% effect_order)
  v_pairs <- paste(v_relevant$row_effect_id, v_relevant$col_effect_id, sep = "::")
  if (nrow(v_relevant) != 25 || anyDuplicated(v_pairs)) {
    stop("V_Matrix_A008 must contain exactly one value for each cell of the known 5x5 matrix.", call. = FALSE)
  }
  V <- matrix(NA_real_, nrow = length(effect_order), ncol = length(effect_order), dimnames = list(effect_order, effect_order))
  for (i in seq_len(nrow(v_relevant))) {
    r <- v_relevant$row_effect_id[[i]]
    c <- v_relevant$col_effect_id[[i]]
    V[r, c] <- v_relevant$covariance[[i]]
  }
  if (any(!is.finite(V))) stop("V_Matrix_A008 is incomplete for the included A008 effects.", call. = FALSE)
  if (!isTRUE(all.equal(V, t(V), tolerance = 1e-10))) stop("V_Matrix_A008 is not symmetric.", call. = FALSE)
  if (any(abs(diag(V) - d$vi) > pmax(1e-8, 1e-5 * abs(d$vi)))) stop("V_Matrix_A008 diagonal does not match effect variances.", call. = FALSE)
  fit <- metafor::rma.mv(yi = yi, V = V, random = ~ 1 | effect_id, data = d, method = "REML", test = "t")
  tau2 <- if (length(fit$sigma2) > 0) as.numeric(fit$sigma2[[1]]) else NA_real_
  mean_sampling_variance <- mean(diag(V))
  I2 <- if (is.finite(tau2)) 100 * tau2 / (tau2 + mean_sampling_variance) else NA_real_
  out <- tibble(
    model_id = model_id,
    k = nrow(d),
    method = "REML multivariate random effects with known sampling V",
    estimate = as.numeric(fit$b),
    se = as.numeric(fit$se),
    ci_lb = as.numeric(fit$ci.lb),
    ci_ub = as.numeric(fit$ci.ub),
    p_value = as.numeric(fit$pval),
    tau2 = tau2,
    I2 = I2,
    Q = as.numeric(fit$QE),
    Q_p = as.numeric(fit$QEp),
    inference = "t-based rma.mv; CR2 not estimable with only two shared-control blocks"
  )
  list(fit = fit, summary = out, kind = "rma_mv_known_v", V = V)
}

fit_mv_cr2_if_eligible <- function(d, model_id) {
  clusters <- trimws(as.character(d$dependency_cluster))
  missing_cluster <- is.na(clusters) | !nzchar(clusters)
  observed_clusters <- clusters[!missing_cluster]
  cluster_sizes <- table(observed_clusters)
  n_clusters <- length(cluster_sizes)
  repeated_effect_clusters <- sum(cluster_sizes > 1L)

  decision_row <- function(decision, rationale) {
    tibble::tibble(
      model_id = model_id,
      k = nrow(d),
      independent_clusters = as.integer(n_clusters),
      repeated_effect_clusters = as.integer(repeated_effect_clusters),
      decision = decision,
      rationale = rationale
    )
  }

  if (nrow(d) < 2L) {
    return(list(
      fit = NULL,
      test = NULL,
      results = NULL,
      decision = decision_row(
        "NOT_APPLICABLE",
        "A single-effect block has no within-cluster dependence to correct"
      )
    ))
  }

  if (any(missing_cluster)) {
    return(list(
      fit = NULL,
      test = NULL,
      results = NULL,
      decision = decision_row(
        "SKIPPED",
        "At least one included effect lacks dependency_cluster, so CR2 eligibility cannot be established"
      )
    ))
  }

  if (repeated_effect_clusters == 0L) {
    return(list(
      fit = NULL,
      test = NULL,
      results = NULL,
      decision = decision_row(
        "NOT_APPLICABLE",
        "Every dependency_cluster contributes one effect; Hartung-Knapp is the locked inference"
      )
    ))
  }

  if (n_clusters < 4) {
    return(list(
      fit = NULL,
      test = NULL,
      results = NULL,
      decision = decision_row(
        "SKIPPED",
        "Dependent effects are present, but CR2 requires at least four independent clusters under the locked rule"
      )
    ))
  }

  fit <- metafor::rma.mv(yi = yi, V = vi, random = ~ 1 | dependency_cluster/effect_id, data = d, method = "REML")
  test <- clubSandwich::coef_test(fit, vcov = "CR2", cluster = d$dependency_cluster, test = "Satterthwaite")
  test_frame <- as.data.frame(test)

  extract_test_column <- function(candidates, label) {
    column <- intersect(candidates, names(test_frame))
    if (length(column) == 0L) {
      stop(
        "clubSandwich::coef_test() did not return the expected ", label,
        " column. Available columns: ", paste(names(test_frame), collapse = ", "),
        call. = FALSE
      )
    }
    as.numeric(test_frame[[column[[1]]]])
  }

  estimate <- extract_test_column(c("beta", "Estimate", "estimate"), "estimate")
  robust_se <- extract_test_column(c("SE", "se"), "standard error")
  t_stat <- extract_test_column(c("tstat", "t_stat", "t-stat"), "t statistic")
  df_satterthwaite <- extract_test_column(c("df_Satt", "df", "d.f."), "Satterthwaite df")
  p_value <- extract_test_column(c("p_Satt", "p_val", "p-value", "p"), "p value")
  coefficient <- names(stats::coef(fit))
  if (is.null(coefficient) || length(coefficient) != length(estimate)) {
    coefficient <- paste0("coefficient_", seq_along(estimate))
  }
  critical <- stats::qt(0.975, df = df_satterthwaite)
  results <- tibble::tibble(
    model_id = rep(model_id, length(estimate)),
    k = rep(nrow(d), length(estimate)),
    independent_clusters = rep(as.integer(n_clusters), length(estimate)),
    coefficient = coefficient,
    estimate = estimate,
    se = robust_se,
    t_stat = t_stat,
    df_satterthwaite = df_satterthwaite,
    p_value = p_value,
    ci_lb = estimate - critical * robust_se,
    ci_ub = estimate + critical * robust_se,
    vcov = rep("CR2", length(estimate)),
    test = rep("Satterthwaite", length(estimate))
  )

  list(
    fit = fit,
    test = test,
    results = results,
    decision = decision_row(
      "RUN",
      "Dependent effects and at least four independent clusters; rma.mv REML with CR2/Satterthwaite was run"
    )
  )
}

safe_filename <- function(x) str_replace_all(x, "[^A-Za-z0-9_-]", "_")

for (i in seq_len(nrow(manifest))) {
  m <- manifest[i, ]
  if (!identical(as.character(m$run_model), "Yes")) next
  model_id <- as.character(m$model_id)
  d <- included_effects %>% filter(.data$model_id == .env$model_id)
  if (nrow(d) == 0) next

  if (identical(model_id, "SENS_SHORTTERM_SOURCE_MV")) {
    result <- fit_known_v_block(d, model_id, inputs$V_Matrix_A008)
    a008_cluster_sizes <- table(d$shared_control_block)
    cr2_log <- bind_rows(
      cr2_log,
      tibble(
        model_id = model_id,
        k = nrow(d),
        independent_clusters = n_distinct(d$shared_control_block),
        repeated_effect_clusters = sum(a008_cluster_sizes > 1L),
        decision = "SKIPPED",
        rationale = "Known V matrix used; only two shared-control blocks, below the locked CR2 threshold of four"
      )
    )
  } else {
    result <- fit_hk_block(d, model_id)
    cr2_result <- fit_mv_cr2_if_eligible(d, model_id)
    if (
      identical(model_id, "MAIN_LOGOR_ORDERED_ENDPOINT") &&
        identical(cr2_result$decision$decision[[1]], "NOT_APPLICABLE")
    ) {
      cr2_result$decision$rationale <- ordered_endpoint_cr2_rationale
    }
    cr2_log <- bind_rows(cr2_log, cr2_result$decision)
    if (!is.null(cr2_result$results)) {
      cr2_results <- bind_rows(cr2_results, cr2_result$results)
      model_objects[[paste0(model_id, "__CR2")]] <- cr2_result$fit
    }
  }

  model_summary <- bind_rows(model_summary, result$summary)
  model_objects[[model_id]] <- result$fit

  if (!is.null(result$fit)) {
    pdf(file.path(figure_dir, paste0("forest_", safe_filename(model_id), ".pdf")), width = 8.5, height = max(5.5, 2.6 + 0.45 * nrow(d)))
    try(
      metafor::forest(
        result$fit,
        slab = paste(d$article_id, d$study_id, sep = " | "),
        xlab = unique(d$effect_measure)[[1]],
        main = model_id,
        cex = 0.85
      ),
      silent = TRUE
    )
    dev.off()
  }

  if (identical(result$kind, "rma_uni") && nrow(d) >= 4) {
    loo <- metafor::leave1out(result$fit)
    loo_df <- as.data.frame(loo) %>% rownames_to_column("deleted_effect")
    if (nrow(loo_df) == nrow(d)) loo_df$deleted_effect <- d$effect_id
    write.csv(loo_df, file.path(table_dir, paste0("leave_one_out_", safe_filename(model_id), ".csv")), row.names = FALSE, na = "")

    infl <- stats::influence(result$fit)
    infl_df <- as.data.frame(infl$inf) %>% rownames_to_column("effect_id")
    if (nrow(infl_df) == nrow(d)) infl_df$effect_id <- d$effect_id
    write.csv(infl_df, file.path(table_dir, paste0("influence_", safe_filename(model_id), ".csv")), row.names = FALSE, na = "")
    diagnostic_log <- bind_rows(diagnostic_log, tibble(model_id = model_id, diagnostic = "Influence and leave-one-out", status = "RUN", details = paste(nrow(d), "effects")))
  } else {
    diagnostic_log <- bind_rows(diagnostic_log, tibble(model_id = model_id, diagnostic = "Influence and leave-one-out", status = "SKIPPED", details = "Requires an rma.uni model with k>=4"))
  }

  if (identical(result$kind, "rma_uni") && nrow(d) >= 10) {
    egger <- metafor::regtest(result$fit, model = "rma")
    capture.output(egger, file = file.path(log_dir, paste0("egger_", safe_filename(model_id), ".txt")))
    pubbias_log <- bind_rows(pubbias_log, tibble(model_id = model_id, k = nrow(d), decision = "RUN", details = "Egger/regression test"))
  } else {
    pubbias_log <- bind_rows(pubbias_log, tibble(model_id = model_id, k = nrow(d), decision = "SKIPPED", details = "Locked rule requires a compatible rma.uni block with k>=10"))
  }
}

model_summary <- model_summary %>% arrange(match(model_id, manifest$model_id))
cr2_log <- cr2_log %>% arrange(match(model_id, manifest$model_id))
cr2_results <- cr2_results %>% arrange(match(model_id, manifest$model_id))

comparison_fields <- c(
  "estimate", "se", "ci_lb", "ci_ub", "p_value", "tau2", "I2", "Q", "Q_p"
)
baseline_model_summary <- read.csv(
  baseline_summary_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA")
)
required_summary_columns <- c("model_id", comparison_fields)
missing_baseline_columns <- setdiff(required_summary_columns, names(baseline_model_summary))
missing_current_columns <- setdiff(required_summary_columns, names(model_summary))
if (length(missing_baseline_columns) > 0L || length(missing_current_columns) > 0L) {
  stop(
    "Numeric comparison columns are missing. Baseline: ",
    paste(missing_baseline_columns, collapse = ", "),
    "; current: ", paste(missing_current_columns, collapse = ", "),
    call. = FALSE
  )
}

model_count_ok <- nrow(baseline_model_summary) == 26L && nrow(model_summary) == 26L
add_validation(
  if (model_count_ok) "PASS" else "ERROR",
  "P2 v1 versus v1.1",
  "Model count",
  paste0("baseline=", nrow(baseline_model_summary), "; current=", nrow(model_summary), "; expected=26")
)

model_id_set_ok <-
  !anyDuplicated(baseline_model_summary$model_id) &&
  !anyDuplicated(model_summary$model_id) &&
  identical(sort(baseline_model_summary$model_id), sort(model_summary$model_id))
add_validation(
  if (model_id_set_ok) "PASS" else "ERROR",
  "P2 v1 versus v1.1",
  "Model ID set",
  if (model_id_set_ok) {
    "The 26 model IDs are unique and identical"
  } else {
    paste0(
      "Only baseline: ",
      paste(setdiff(baseline_model_summary$model_id, model_summary$model_id), collapse = ", "),
      "; only current: ",
      paste(setdiff(model_summary$model_id, baseline_model_summary$model_id), collapse = ", ")
    )
  }
)

comparison_model_ids <- union(
  as.character(baseline_model_summary$model_id),
  as.character(model_summary$model_id)
)
comparison_rows <- vector("list", length(comparison_model_ids) * length(comparison_fields))
comparison_index <- 0L
for (comparison_model_id in comparison_model_ids) {
  old_index <- match(comparison_model_id, baseline_model_summary$model_id)
  new_index <- match(comparison_model_id, model_summary$model_id)
  model_present <- !is.na(old_index) && !is.na(new_index)
  for (comparison_field in comparison_fields) {
    comparison_index <- comparison_index + 1L
    old_value <- if (model_present) {
      as.numeric(baseline_model_summary[[comparison_field]][[old_index]])
    } else {
      NA_real_
    }
    new_value <- if (model_present) {
      as.numeric(model_summary[[comparison_field]][[new_index]])
    } else {
      NA_real_
    }
    both_missing <- model_present && is.na(old_value) && is.na(new_value)
    both_finite <- model_present && is.finite(old_value) && is.finite(new_value)
    absolute_difference <- if (both_missing) {
      0
    } else if (both_finite) {
      abs(old_value - new_value)
    } else {
      NA_real_
    }
    comparison_pass <- both_missing ||
      (both_finite && absolute_difference <= numeric_tolerance)
    comparison_rows[[comparison_index]] <- tibble(
      model_id = comparison_model_id,
      field = comparison_field,
      old_value = old_value,
      new_value = new_value,
      absolute_difference = absolute_difference,
      tolerance = numeric_tolerance,
      status = if (comparison_pass) "PASS" else "FAIL"
    )
  }
}
numeric_comparison <- bind_rows(comparison_rows)
numeric_comparison_ok <- nrow(numeric_comparison) == 26L * length(comparison_fields) &&
  all(numeric_comparison$status == "PASS")
add_validation(
  if (numeric_comparison_ok) "PASS" else "ERROR",
  "P2 v1 versus v1.1",
  "Nine-field numeric identity",
  paste0(
    sum(numeric_comparison$status == "PASS"), "/", nrow(numeric_comparison),
    " comparisons pass at absolute tolerance ", format(numeric_tolerance, scientific = TRUE)
  )
)

ordered_numeric <- numeric_comparison %>%
  filter(model_id == "MAIN_LOGOR_ORDERED_ENDPOINT")
ordered_numeric_ok <- nrow(ordered_numeric) == length(comparison_fields) &&
  all(ordered_numeric$status == "PASS")
add_validation(
  if (ordered_numeric_ok) "PASS" else "ERROR",
  "MAIN_LOGOR_ORDERED_ENDPOINT",
  "Numeric identity",
  "estimate, SE, CI, p, tau2, I2, Q, and Qp are unchanged within 1e-12"
)

ordered_cr2 <- cr2_log %>%
  filter(model_id == "MAIN_LOGOR_ORDERED_ENDPOINT")
ordered_cr2_ok <- nrow(ordered_cr2) == 1L &&
  as.integer(ordered_cr2$k[[1]]) == 3L &&
  as.integer(ordered_cr2$independent_clusters[[1]]) == 3L &&
  as.integer(ordered_cr2$repeated_effect_clusters[[1]]) == 0L &&
  identical(as.character(ordered_cr2$decision[[1]]), "NOT_APPLICABLE") &&
  identical(as.character(ordered_cr2$rationale[[1]]), ordered_endpoint_cr2_rationale)
add_validation(
  if (ordered_cr2_ok) "PASS" else "ERROR",
  "MAIN_LOGOR_ORDERED_ENDPOINT",
  "CR2 applicability",
  if (ordered_cr2_ok) {
    paste0(
      "k=3; independent_clusters=3; repeated_effect_clusters=0; ",
      "decision=NOT_APPLICABLE; ", ordered_endpoint_cr2_rationale
    )
  } else {
    "Expected k=3, three independent clusters, no repeated-effect cluster, and NOT_APPLICABLE"
  }
)

baseline_cr2 <- read.csv(
  baseline_cr2_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA")
)
baseline_ordered_cr2 <- baseline_cr2 %>%
  filter(model_id == "MAIN_LOGOR_ORDERED_ENDPOINT")
ordered_cr2_transition_ok <- nrow(baseline_ordered_cr2) == 1L &&
  identical(as.character(baseline_ordered_cr2$decision[[1]]), "SKIPPED") &&
  ordered_cr2_ok
add_validation(
  if (ordered_cr2_transition_ok) "PASS" else "ERROR",
  "MAIN_LOGOR_ORDERED_ENDPOINT",
  "CR2 status transition",
  "Baseline SKIPPED changed only to v1.1 NOT_APPLICABLE after correcting the three A020 clusters"
)

cr2_compare_columns <- c(
  "model_id", "k", "independent_clusters", "repeated_effect_clusters", "decision"
)
other_old_cr2 <- baseline_cr2 %>%
  filter(model_id != "MAIN_LOGOR_ORDERED_ENDPOINT") %>%
  select(all_of(cr2_compare_columns)) %>%
  arrange(model_id)
other_new_cr2 <- cr2_log %>%
  filter(model_id != "MAIN_LOGOR_ORDERED_ENDPOINT") %>%
  select(all_of(cr2_compare_columns)) %>%
  arrange(model_id)
other_cr2_unchanged <- isTRUE(all.equal(
  as.data.frame(other_old_cr2),
  as.data.frame(other_new_cr2),
  check.attributes = FALSE
))
add_validation(
  if (other_cr2_unchanged) "PASS" else "ERROR",
  "CR2 decision log",
  "Other-model decisions unchanged",
  "All non-A020 model CR2 decisions and dependence counts match the baseline"
)

a008_cr2 <- cr2_log %>% filter(model_id == "SENS_SHORTTERM_SOURCE_MV")
a008_cr2_ok <- nrow(a008_cr2) == 1L &&
  identical(as.character(a008_cr2$decision[[1]]), "SKIPPED") &&
  as.integer(a008_cr2$independent_clusters[[1]]) == 2L
add_validation(
  if (a008_cr2_ok) "PASS" else "ERROR",
  "SENS_SHORTTERM_SOURCE_MV",
  "A008 CR2 decision preserved",
  "Known 5x5 V-matrix model remains SKIPPED for CR2 with two shared-control blocks"
)

write.csv(model_summary, file.path(table_dir, "model_summary.csv"), row.names = FALSE, na = "")
write.csv(diagnostic_log, file.path(table_dir, "diagnostic_decisions.csv"), row.names = FALSE, na = "")
write.csv(cr2_log, file.path(table_dir, "CR2_decision_log.csv"), row.names = FALSE, na = "")
write.csv(cr2_results, file.path(table_dir, "CR2_results.csv"), row.names = FALSE, na = "")
write.csv(pubbias_log, file.path(table_dir, "publication_bias_decision_log.csv"), row.names = FALSE, na = "")
write.csv(
  numeric_comparison,
  file.path(table_dir, "P2_v1_vs_v1.1_numeric_comparison.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(validation, file.path(table_dir, "analysis_validation.csv"), row.names = FALSE, na = "")
saveRDS(model_objects, file.path(model_dir, "model_objects.rds"))

qc_status <- if (any(validation$severity == "ERROR")) "FAIL" else "PASS"
run_note <- c(
  paste(analysis_version, "completed with QC status", qc_status),
  paste("Input version:", input_version),
  paste("Input:", normalizePath(input_path)),
  paste("Input resolution:", input_origin),
  paste("Baseline run:", normalizePath(baseline_run)),
  paste("Run directory:", normalizePath(run_dir)),
  paste("Locked quantitative candidate rows:", nrow(lock_table)),
  paste("Included quantitative rows across model sheets:", nrow(included_effects)),
  paste("Model blocks summarized:", nrow(model_summary)),
  paste("Numeric comparisons passed:", sum(numeric_comparison$status == "PASS"), "of", nrow(numeric_comparison)),
  paste("CR2 models run:", sum(cr2_log$decision == "RUN")),
  "Independent compatible blocks: REML + Hartung-Knapp.",
  "Dependent-effect blocks: CR2/Satterthwaite only when at least four independent dependency clusters are available.",
  "A008: known 5x5 sampling covariance matrix with rma.mv; CR2 skipped because there are only two shared-control blocks.",
  "Single-effect blocks: descriptive estimates only.",
  "Influence/leave-one-out: only rma.uni blocks with k>=4.",
  "Publication-bias testing: only compatible rma.uni blocks with k>=10.",
  "The input workbook was not modified."
)
writeLines(run_note, file.path(run_dir, "RUN_NOTE.txt"))
capture.output(sessionInfo(), file = file.path(log_dir, "sessionInfo.txt"))

readme_run <- c(
  "This is an interim QC checkpoint, not the final meta-analysis freeze.",
  "The report universe remains 51 pending the decision on a second literature-inclusion wave.",
  "",
  paste("Analysis version:", analysis_version),
  paste("Input version:", input_version),
  paste("Baseline run:", basename(baseline_run)),
  paste("QC status:", qc_status),
  paste("Models:", nrow(model_summary)),
  paste("Numeric tolerance:", format(numeric_tolerance, scientific = TRUE)),
  paste("Numeric comparisons:", sum(numeric_comparison$status == "PASS"), "PASS of", nrow(numeric_comparison)),
  paste("Validation errors:", sum(validation$severity == "ERROR")),
  "No inclusion decision, effect direction, model formula, weight, or input precision value was changed.",
  "Meta_Analysis_Input_v2.1 is an interim QC input and is not labeled as the final analysis dataset."
)
writeLines(readme_run, file.path(run_dir, "README_run.txt"), useBytes = TRUE)

artifact_sources <- c(
  script_path,
  file.path(table_dir, "model_summary.csv"),
  file.path(table_dir, "CR2_results.csv"),
  file.path(table_dir, "CR2_decision_log.csv"),
  file.path(table_dir, "P2_v1_vs_v1.1_numeric_comparison.csv"),
  file.path(table_dir, "analysis_validation.csv"),
  file.path(log_dir, "sessionInfo.txt")
)
artifact_destinations <- file.path(
  run_dir,
  c(
    "P2_analysis_v1.1.R",
    "model_summary.csv",
    "CR2_results.csv",
    "CR2_decision_log.csv",
    "P2_v1_vs_v1.1_numeric_comparison.csv",
    "validation_results.csv",
    "sessionInfo.txt"
  )
)
artifact_copy_ok <- mapply(
  file.copy,
  from = artifact_sources,
  to = artifact_destinations,
  MoreArgs = list(overwrite = FALSE),
  USE.NAMES = FALSE
)
if (!all(artifact_copy_ok)) {
  stop("One or more required top-level run artifacts could not be copied.", call. = FALSE)
}

message("Analysis complete: ", run_dir)
message("Key result table: ", file.path(table_dir, "model_summary.csv"))
message("QC status: ", qc_status)
if (!identical(qc_status, "PASS")) {
  stop(
    "P2_analysis_v1.1 completed, but one or more QC validations failed. See validation_results.csv.",
    call. = FALSE
  )
}
