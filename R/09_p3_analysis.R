# P3/v3.1 analysis support -------------------------------------------------
#
# This module is sourced by analysis/P3_analysis_v1.0.R and by the synthetic
# test suite. It never downloads, edits, or writes back to a research workbook.

P3_ANALYSIS_VERSION <- "P3_analysis_v1.0"
P3_INPUT_VERSION <- "Meta_Analysis_Input_v3.1"
P3_INPUT_FILENAME <- paste0(P3_INPUT_VERSION, ".xlsx")
P3_RUN_PREFIX <- "runP3v1_"

P3_REQUIRED_PACKAGES <- c(
  "clubSandwich", "digest", "dplyr", "metafor", "purrr", "readxl",
  "stringr", "tibble"
)

P3_INCREMENT_SHEETS <- c(
  "QC_Summary_v3_1",
  "Raw_Effects_v3_1",
  "Analysis_Manifest_v3_1",
  "Main_LogOR_v3_1",
  "Main_SMD_v3_1",
  "Main_Nonlinear_v3_1",
  "Sensitivity_LogOR_v3_1",
  "Module_SMD_v3_1",
  "Module_Nonlinear_v3_1",
  "Precision_Covariance_v3_1",
  "Quarantined_Effects_v3_1",
  "Direction_Audit_v3_1",
  "Effect_Decision_Lock_v3_1"
)

P3_INHERITED_SHEETS <- c(
  "Analysis_Manifest_v3",
  "V_Matrix_A008",
  "Event_Temporal_v3",
  "Updating_v3",
  "MPT_Separate_v3",
  "Grey_LogOR_v3",
  "Module_Gaussian_v3"
)

P3_ANALYTIC_SHEETS <- c(
  "Main_LogOR_v3_1",
  "Main_SMD_v3_1",
  "Main_Nonlinear_v3_1",
  "Sensitivity_LogOR_v3_1",
  "Module_SMD_v3_1",
  "Module_Nonlinear_v3_1",
  "Event_Temporal_v3",
  "Updating_v3",
  "MPT_Separate_v3",
  "Grey_LogOR_v3",
  "Module_Gaussian_v3"
)

P3_RESCUE_SHEET_CANDIDATES <- c(
  "Repository_Rescue_Lock_v3_1",
  "Repository_Rescue_v3_1",
  "Rescue_Lock_v3_1"
)

p3_require_packages <- function() {
  missing <- P3_REQUIRED_PACKAGES[!vapply(
    P3_REQUIRED_PACKAGES,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )]
  if (length(missing) > 0L) {
    stop(
      "Missing required R packages: ", paste(missing, collapse = ", "),
      ". Install them outside this script, then rerun.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

p3_named_option <- function(name, args = commandArgs(trailingOnly = TRUE)) {
  prefix <- paste0("--", name, "=")
  matches <- args[startsWith(args, prefix)]
  if (length(matches) > 1L) {
    stop("Specify ", prefix, " only once.", call. = FALSE)
  }
  if (length(matches) == 0L) return(NA_character_)
  value <- substring(matches[[1]], nchar(prefix) + 1L)
  if (!nzchar(trimws(value))) {
    stop(prefix, " requires a non-empty value.", call. = FALSE)
  }
  value
}

p3_has_flag <- function(flag, args = commandArgs(trailingOnly = TRUE)) {
  identical(sum(args == paste0("--", flag)), 1L)
}

p3_resolve_local_path <- function(path, base = getwd(), must_work = FALSE) {
  path <- path.expand(trimws(path))
  is_absolute <- grepl("^/", path) ||
    grepl("^[A-Za-z]:[/\\\\]", path) ||
    grepl("^\\\\\\\\", path)
  if (!is_absolute) path <- file.path(base, path)
  normalizePath(path, winslash = "/", mustWork = must_work)
}

p3_locate_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    has_rproj <- length(list.files(current, pattern = "\\.Rproj$")) > 0L
    if (dir.exists(file.path(current, ".git")) || has_rproj) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) return(NA_character_)
    current <- parent
  }
}

p3_resolve_paths <- function(
  args = commandArgs(trailingOnly = TRUE),
  project_root = p3_locate_project_root(),
  script_dir = if (!is.na(project_root)) file.path(project_root, "analysis") else getwd(),
  timestamp = format(Sys.time(), "%Y%m%d_%H%M%S")
) {
  base <- if (!is.na(project_root)) project_root else getwd()

  input_cli <- p3_named_option("input", args)
  input_env <- trimws(Sys.getenv("META_ANALYSIS_INPUT", unset = ""))
  private_input <- if (!is.na(project_root)) {
    file.path(project_root, "data", "private", P3_INPUT_FILENAME)
  } else {
    NA_character_
  }
  compatibility_input <- file.path(script_dir, P3_INPUT_FILENAME)

  if (!is.na(input_cli)) {
    input_path <- p3_resolve_local_path(input_cli, base)
    input_origin <- "--input"
  } else if (nzchar(input_env)) {
    input_path <- p3_resolve_local_path(input_env, base)
    input_origin <- "META_ANALYSIS_INPUT"
  } else {
    candidates <- unique(na.omit(c(private_input, compatibility_input)))
    candidates <- vapply(
      candidates,
      p3_resolve_local_path,
      character(1),
      base = base,
      must_work = FALSE
    )
    existing <- candidates[file.exists(candidates)]
    input_path <- if (length(existing) > 0L) existing[[1]] else NA_character_
    input_origin <- if (
      !is.na(input_path) && !is.na(private_input) &&
        identical(input_path, p3_resolve_local_path(private_input, base))
    ) {
      "data/private fallback"
    } else {
      "script-directory compatibility fallback"
    }
  }

  if (is.na(input_path) || !file.exists(input_path)) {
    attempted <- unique(na.omit(c(
      if (!is.na(input_cli)) p3_resolve_local_path(input_cli, base) else NA_character_,
      if (nzchar(input_env)) p3_resolve_local_path(input_env, base) else NA_character_,
      if (!is.na(private_input)) p3_resolve_local_path(private_input, base) else NA_character_,
      p3_resolve_local_path(compatibility_input, base)
    )))
    stop(
      "Private input workbook not found. Checked:\n- ",
      paste(attempted, collapse = "\n- "),
      "\nProvide --input=, META_ANALYSIS_INPUT, data/private/", P3_INPUT_FILENAME,
      ", or a compatibility copy beside the P3 entry script.",
      call. = FALSE
    )
  }
  input_path <- normalizePath(input_path, winslash = "/", mustWork = TRUE)

  run_name <- paste0(P3_RUN_PREFIX, timestamp)
  output_cli <- p3_named_option("output-dir", args)
  output_env <- trimws(Sys.getenv("META_ANALYSIS_OUTPUT_DIR", unset = ""))
  output_requested <- if (!is.na(output_cli)) {
    p3_resolve_local_path(output_cli, base)
  } else if (nzchar(output_env)) {
    p3_resolve_local_path(output_env, base)
  } else if (!is.na(project_root)) {
    p3_resolve_local_path(file.path(project_root, "results"), base)
  } else {
    p3_resolve_local_path(file.path(tempdir(), "p3-results"), base)
  }
  explicit_run_dir <- grepl(paste0("^", P3_RUN_PREFIX), basename(output_requested))
  run_dir <- if (explicit_run_dir) output_requested else file.path(output_requested, run_name)

  rob_cli <- p3_named_option("rob-input", args)
  rob_env <- trimws(Sys.getenv("META_ANALYSIS_ROB_INPUT", unset = ""))
  rob_input <- if (!is.na(rob_cli)) {
    p3_resolve_local_path(rob_cli, base)
  } else if (nzchar(rob_env)) {
    p3_resolve_local_path(rob_env, base)
  } else {
    NA_character_
  }
  if (!is.na(rob_input) && !file.exists(rob_input)) {
    stop("Risk-of-bias input does not exist: ", rob_input, call. = FALSE)
  }

  list(
    input_path = input_path,
    input_origin = input_origin,
    run_dir = p3_resolve_local_path(run_dir, base),
    rob_input = rob_input,
    project_root = project_root,
    script_dir = normalizePath(script_dir, winslash = "/", mustWork = FALSE)
  )
}

p3_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

p3_find_column <- function(df, candidates, required = FALSE, table = "table") {
  available <- names(df)
  index <- match(tolower(candidates), tolower(available), nomatch = 0L)
  index <- index[index > 0L]
  if (length(index) > 0L) return(available[[index[[1]]]])
  if (required) {
    stop(
      table, " is missing a required column. Expected one of: ",
      paste(candidates, collapse = ", "),
      call. = FALSE
    )
  }
  NA_character_
}

p3_column <- function(df, candidates, default = NA_character_, table = "table") {
  column <- p3_find_column(df, candidates, required = FALSE, table = table)
  if (is.na(column)) return(rep(default, nrow(df)))
  df[[column]]
}

p3_text <- function(x) {
  out <- trimws(as.character(x))
  out[is.na(out)] <- ""
  out
}

p3_is_yes <- function(x) {
  toupper(p3_text(x)) %in% c("YES", "Y", "TRUE", "1", "INCLUDE", "INCLUDED")
}

p3_row_text <- function(df) {
  if (nrow(df) == 0L) return(character())
  atomic_columns <- !vapply(df, is.list, logical(1))
  if (!any(atomic_columns)) return(rep("", nrow(df)))
  column_names <- names(df)[atomic_columns]
  apply(
    df[, atomic_columns, drop = FALSE],
    1L,
    function(x) paste(paste0(column_names, "=", p3_text(x)), collapse = " | ")
  )
}

p3_nonempty_rows <- function(df, id_candidates, table) {
  id_col <- p3_find_column(df, id_candidates, required = TRUE, table = table)
  df[nzchar(p3_text(df[[id_col]])), , drop = FALSE]
}

p3_resolve_rescue_sheet <- function(sheet_names) {
  exact <- P3_RESCUE_SHEET_CANDIDATES[P3_RESCUE_SHEET_CANDIDATES %in% sheet_names]
  if (length(exact) > 0L) return(exact[[1]])
  regex <- sheet_names[grepl("rescue.*lock|lock.*rescue", sheet_names, ignore.case = TRUE)]
  if (length(regex) == 1L) return(regex[[1]])
  if (length(regex) > 1L) {
    stop("Multiple repository-rescue lock sheets were found: ", paste(regex, collapse = ", "), call. = FALSE)
  }
  stop(
    "Repository-rescue lock sheet not found. Expected one of: ",
    paste(P3_RESCUE_SHEET_CANDIDATES, collapse = ", "),
    call. = FALSE
  )
}

p3_read_workbook <- function(path) {
  sheet_names <- readxl::excel_sheets(path)
  required <- unique(c(P3_INCREMENT_SHEETS, P3_INHERITED_SHEETS))
  missing <- setdiff(required, sheet_names)
  if (length(missing) > 0L) {
    stop("Workbook is missing required sheets: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  rescue_sheet <- p3_resolve_rescue_sheet(sheet_names)
  requested <- unique(c(required, rescue_sheet))
  tables <- setNames(
    lapply(
      requested,
      function(sheet) readxl::read_excel(
        path,
        sheet = sheet,
        skip = 2,
        .name_repair = "unique"
      )
    ),
    requested
  )
  attr(tables, "rescue_sheet") <- rescue_sheet
  attr(tables, "available_sheets") <- sheet_names
  tables
}

p3_default_scale <- function(sheet) {
  if (grepl("LogOR", sheet, ignore.case = TRUE)) return("logOR")
  if (grepl("SMD", sheet, ignore.case = TRUE)) return("SMD")
  if (grepl("Nonlinear", sheet, ignore.case = TRUE)) return("nonlinear coefficient")
  if (grepl("Gaussian|Event_Temporal", sheet, ignore.case = TRUE)) return("Gaussian beta")
  if (grepl("MPT", sheet, ignore.case = TRUE)) return("Bayesian MPT parameter")
  "unspecified"
}

p3_standardize_effect_sheet <- function(df, source_sheet) {
  table <- source_sheet
  effect_id <- p3_column(
    df,
    c("effect_id", "event_id", "analysis_id", "effect_id_quadratic", "record_id"),
    table = table
  )
  yi <- p3_column(
    df,
    c("yi", "estimate", "beta_quadratic", "coefficient", "posterior_mean"),
    default = NA_real_,
    table = table
  )
  sei <- p3_column(
    df,
    c("sei", "se", "se_quadratic", "sei_quadratic", "posterior_sd"),
    default = NA_real_,
    table = table
  )
  vi <- p3_column(
    df,
    c("vi", "variance", "vi_quadratic", "sampling_variance", "posterior_variance"),
    default = NA_real_,
    table = table
  )

  data.frame(
    effect_id = p3_text(effect_id),
    article_id = p3_text(p3_column(df, c("article_id", "report_id", "source_id"), table = table)),
    study_id = p3_text(p3_column(df, c("study_id", "experiment_id", "study"), table = table)),
    sample_id = p3_text(p3_column(df, c("sample_id", "independent_sample_id"), table = table)),
    author = p3_text(p3_column(df, c("author", "first_author"), table = table)),
    year = suppressWarnings(as.numeric(p3_column(df, c("year", "publication_year"), default = NA_real_, table = table))),
    outcome = p3_text(p3_column(df, c("outcome", "memory_outcome", "memory_class", "dependent_variable"), table = table)),
    contrast = p3_text(p3_column(df, c("contrast", "comparison", "condition_contrast"), table = table)),
    effect_scale = p3_text(p3_column(
      df,
      c("effect_scale", "effect_measure", "smd_type", "coefficient_scale"),
      default = p3_default_scale(source_sheet),
      table = table
    )),
    estimand = p3_text(p3_column(
      df,
      c("estimand", "target_estimand", "coefficient_term"),
      default = if (grepl("Nonlinear", source_sheet, ignore.case = TRUE)) "quadratic term" else "unspecified",
      table = table
    )),
    pe_encoding = p3_text(p3_column(
      df,
      c("pe_encoding", "pe_sign_type", "predictor_encoding", "pe_scale"),
      default = "unspecified",
      table = table
    )),
    yi = suppressWarnings(as.numeric(yi)),
    sei = suppressWarnings(as.numeric(sei)),
    vi = suppressWarnings(as.numeric(vi)),
    model_id = p3_text(p3_column(df, c("model_id", "analysis_block", "pooling_block"), table = table)),
    base_model_id = p3_text(p3_column(df, c("base_model_id", "primary_model_id"), table = table)),
    analysis_role = p3_text(p3_column(
      df,
      c("analysis_role", "analysis_stream", "role"),
      default = if (grepl("Sensitivity", source_sheet, ignore.case = TRUE)) "SENSITIVITY" else "PRIMARY",
      table = table
    )),
    analysis_include = p3_text(p3_column(df, c("analysis_include", "include", "run_effect"), table = table)),
    decision_status = p3_text(p3_column(df, c("decision_status", "lock_status"), table = table)),
    dependency_cluster = p3_text(p3_column(
      df,
      c("dependency_cluster", "independent_cluster", "sample_id", "independent_sample_id"),
      table = table
    )),
    shared_control_block = p3_text(p3_column(df, c("shared_control_block", "control_block"), table = table)),
    replacement_for = p3_text(p3_column(
      df,
      c("replacement_for", "replaces_effect_id", "replace_effect_id"),
      table = table
    )),
    sensitivity_rule = p3_text(p3_column(
      df,
      c("sensitivity_rule", "replacement_rule", "analysis_note"),
      table = table
    )),
    paradigm = p3_text(p3_column(df, c("paradigm", "task_paradigm"), table = table)),
    experiment = p3_text(p3_column(df, c("experiment", "experiment_number", "experiment_id"), table = table)),
    source_sheet = source_sheet,
    notes = p3_text(p3_column(df, c("notes", "note", "limitations"), table = table)),
    row_text = p3_row_text(df),
    stringsAsFactors = FALSE
  )
}

p3_standardize_effects <- function(tables) {
  missing <- setdiff(P3_ANALYTIC_SHEETS, names(tables))
  if (length(missing) > 0L) {
    stop("Cannot standardize missing analytic sheets: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  out <- lapply(P3_ANALYTIC_SHEETS, function(sheet) {
    p3_standardize_effect_sheet(tables[[sheet]], sheet)
  })
  do.call(rbind, out)
}

p3_standardize_manifest <- function(df, version) {
  model_id <- p3_text(p3_column(df, c("model_id", "analysis_block"), table = version))
  keep <- nzchar(model_id)
  data.frame(
    model_id = model_id[keep],
    run_model = p3_text(p3_column(df, c("run_model", "analysis_include"), default = "Yes", table = version))[keep],
    analysis_role = p3_text(p3_column(df, c("analysis_role", "role"), default = "PRIMARY", table = version))[keep],
    source_sheet = p3_text(p3_column(df, c("source_sheet", "input_sheet"), table = version))[keep],
    effect_scale = p3_text(p3_column(df, c("effect_scale", "effect_measure"), table = version))[keep],
    estimand = p3_text(p3_column(df, c("estimand", "target_estimand"), table = version))[keep],
    pe_encoding = p3_text(p3_column(df, c("pe_encoding", "predictor_encoding"), table = version))[keep],
    outcome = p3_text(p3_column(df, c("outcome", "memory_outcome"), table = version))[keep],
    model_type = p3_text(p3_column(df, c("model_type", "analysis_method"), table = version))[keep],
    base_model_id = p3_text(p3_column(df, c("base_model_id", "primary_model_id"), table = version))[keep],
    sensitivity_rule = p3_text(p3_column(df, c("sensitivity_rule", "replacement_rule"), table = version))[keep],
    known_v_sheet = p3_text(p3_column(df, c("known_v_sheet", "covariance_sheet", "v_matrix_sheet"), table = version))[keep],
    allow_mixed_outcome = p3_text(p3_column(df, c("allow_mixed_outcome", "common_estimand_confirmed"), default = "No", table = version))[keep],
    critical = p3_text(p3_column(df, c("critical", "critical_model"), table = version))[keep],
    manifest_version = version,
    stringsAsFactors = FALSE
  )
}

p3_combine_manifests <- function(tables) {
  old <- p3_standardize_manifest(tables$Analysis_Manifest_v3, "v3")
  increment <- p3_standardize_manifest(tables$Analysis_Manifest_v3_1, "v3.1")
  combined <- rbind(old, increment)
  combined <- combined[!duplicated(combined$model_id, fromLast = TRUE), , drop = FALSE]
  rownames(combined) <- NULL
  combined
}

# Input QC -----------------------------------------------------------------

p3_qc_row <- function(check, status, details, critical = TRUE) {
  data.frame(
    check = check,
    status = status,
    critical = isTRUE(critical),
    details = as.character(details),
    stringsAsFactors = FALSE
  )
}

p3_qc_pass <- function(check, details, critical = TRUE) {
  p3_qc_row(check, "PASS", details, critical)
}

p3_qc_fail <- function(check, details, critical = TRUE) {
  p3_qc_row(check, "FAIL", details, critical)
}

p3_status_column <- function(df, table) {
  p3_find_column(
    df,
    c("status", "qc_status", "result", "check_status"),
    required = TRUE,
    table = table
  )
}

p3_rescue_contribution_counts <- function(rescue) {
  role_col <- p3_find_column(
    rescue,
    c("analysis_role", "role", "analysis_stream", "contribution_role"),
    required = TRUE,
    table = "repository-rescue lock"
  )
  role <- toupper(p3_text(rescue[[role_col]]))
  include_col <- p3_find_column(
    rescue,
    c("analysis_include", "include", "run_effect"),
    required = FALSE,
    table = "repository-rescue lock"
  )
  included <- if (is.na(include_col)) rep(TRUE, nrow(rescue)) else p3_is_yes(rescue[[include_col]])
  k_col <- p3_find_column(
    rescue,
    c("k_contribution", "independent_k_contribution", "contributes_k", "independent_k"),
    required = FALSE,
    table = "repository-rescue lock"
  )

  if (!is.na(k_col)) {
    contribution <- suppressWarnings(as.numeric(rescue[[k_col]]))
    contribution[!is.finite(contribution)] <- 0
    primary_k <- sum(contribution[included & grepl("PRIMARY", role)], na.rm = TRUE)
    sensitivity_k <- sum(contribution[included & grepl("SENSIT", role)], na.rm = TRUE)
    return(c(primary = as.integer(primary_k), sensitivity = as.integer(sensitivity_k)))
  }

  cluster_col <- p3_find_column(
    rescue,
    c("dependency_cluster", "independent_cluster", "sample_id", "independent_sample_id"),
    required = TRUE,
    table = "repository-rescue lock"
  )
  clusters <- p3_text(rescue[[cluster_col]])
  c(
    primary = length(unique(clusters[included & grepl("PRIMARY", role) & nzchar(clusters)])),
    sensitivity = length(unique(clusters[included & grepl("SENSIT", role) & nzchar(clusters)]))
  )
}

p3_direction_audit_status <- function(direction_audit) {
  status_col <- p3_find_column(
    direction_audit,
    c("status", "direction_status", "audit_status", "decision_status"),
    required = TRUE,
    table = "Direction_Audit_v3_1"
  )
  toupper(p3_text(direction_audit[[status_col]]))
}

p3_matches_study <- function(text, study_id) {
  grepl(paste0("(^|[^A-Za-z0-9])", study_id, "([^A-Za-z0-9]|$)"), text, ignore.case = TRUE)
}

p3_matches_experiment <- function(text, number) {
  grepl(
    paste0("(EXPERIMENT|EXP)[ _.:=-]*(NUMBER[ _.:=-]*)?", number, "([^0-9]|$)"),
    text,
    ignore.case = TRUE
  )
}

p3_matches_paradigm <- function(text, number) {
  grepl(
    paste0("PARADIGM[ _.:=-]*(NUMBER[ _.:=-]*)?", number, "([^0-9]|$)"),
    text,
    ignore.case = TRUE
  )
}

p3_validate_input_qc <- function(tables, effects, manifest) {
  qc <- list()
  add <- function(row) qc[[length(qc) + 1L]] <<- row

  qc_summary <- tables$QC_Summary_v3_1
  status_col <- p3_status_column(qc_summary, "QC_Summary_v3_1")
  statuses <- toupper(p3_text(qc_summary[[status_col]]))
  statuses <- statuses[nzchar(statuses)]
  if (length(statuses) > 0L && all(statuses == "PASS")) {
    add(p3_qc_pass("QC_Summary_v3_1", paste(length(statuses), "checks are PASS")))
  } else {
    add(p3_qc_fail(
      "QC_Summary_v3_1",
      paste("Non-PASS statuses:", paste(unique(statuses[statuses != "PASS"]), collapse = ", "))
    ))
  }

  raw <- p3_nonempty_rows(
    tables$Raw_Effects_v3_1,
    c("effect_id", "raw_effect_id", "record_id", "source_effect_id"),
    "Raw_Effects_v3_1"
  )
  if (nrow(raw) == 56L) {
    add(p3_qc_pass("Raw effects count", "56 valid Raw_Effects_v3_1 records"))
  } else {
    add(p3_qc_fail("Raw effects count", paste("Expected 56; found", nrow(raw))))
  }

  rescue_sheet <- attr(tables, "rescue_sheet")
  rescue <- p3_nonempty_rows(
    tables[[rescue_sheet]],
    c("rescue_id", "effect_id", "component_id", "record_id", "source_analysis_id"),
    rescue_sheet
  )
  if (nrow(rescue) == 21L) {
    add(p3_qc_pass("Repository-rescue lock count", "21 effect/component records"))
  } else {
    add(p3_qc_fail("Repository-rescue lock count", paste("Expected 21; found", nrow(rescue))))
  }

  atom_col <- p3_find_column(
    rescue,
    c("record_type", "component_type", "rescue_record_type", "is_new_atomic", "new_atomic_record"),
    required = TRUE,
    table = rescue_sheet
  )
  atom_values <- toupper(p3_text(rescue[[atom_col]]))
  new_atomic <- atom_values %in% c("YES", "Y", "TRUE", "1", "NEW_ATOMIC", "NEW ATOMIC") |
    grepl("NEW.*ATOM|ATOM.*NEW", atom_values)
  if (sum(new_atomic) == 17L) {
    add(p3_qc_pass("Repository-rescue atomic count", "17 new atomic records"))
  } else {
    add(p3_qc_fail(
      "Repository-rescue atomic count",
      paste("Expected 17; found", sum(new_atomic))
    ))
  }

  rescue_k <- p3_rescue_contribution_counts(rescue)
  if (identical(unname(rescue_k), c(9L, 4L))) {
    add(p3_qc_pass("Repository-rescue independent contributions", "primary k=9; sensitivity k=4"))
  } else {
    add(p3_qc_fail(
      "Repository-rescue independent contributions",
      paste0("Expected primary k=9 and sensitivity k=4; found primary k=", rescue_k[["primary"]],
             " and sensitivity k=", rescue_k[["sensitivity"]])
    ))
  }

  for (sheet in P3_ANALYTIC_SHEETS) {
    d <- effects[effects$source_sheet == sheet & nzchar(effects$effect_id), , drop = FALSE]
    duplicates <- unique(d$effect_id[duplicated(d$effect_id)])
    if (length(duplicates) == 0L) {
      add(p3_qc_pass(paste0("Unique effect_id: ", sheet), paste(nrow(d), "records")))
    } else {
      add(p3_qc_fail(
        paste0("Unique effect_id: ", sheet),
        paste("Duplicates:", paste(duplicates, collapse = ", "))
      ))
    }
  }

  included <- effects[p3_is_yes(effects$analysis_include), , drop = FALSE]
  precision_ok <- is.finite(included$yi) & is.finite(included$sei) & included$sei > 0 &
    is.finite(included$vi) & included$vi > 0
  variance_ok <- precision_ok & abs(included$vi - included$sei^2) <=
    pmax(1e-10, 1e-6 * abs(included$vi))
  if (nrow(included) > 0L && all(variance_ok)) {
    add(p3_qc_pass("Included precision", paste(nrow(included), "included records have valid yi/sei/vi")))
  } else {
    bad <- included$effect_id[!variance_ok]
    add(p3_qc_fail(
      "Included precision",
      paste("Invalid or inconsistent precision:", paste(bad, collapse = ", "))
    ))
  }

  missing_model <- included$effect_id[!nzchar(included$model_id)]
  unknown_model <- setdiff(unique(included$model_id[nzchar(included$model_id)]), manifest$model_id)
  if (length(missing_model) == 0L && length(unknown_model) == 0L) {
    add(p3_qc_pass("Manifest coverage", "Every included effect maps to a v3/v3.1 model ID"))
  } else {
    add(p3_qc_fail(
      "Manifest coverage",
      paste0(
        "Effects without model_id: ", paste(missing_model, collapse = ", "),
        "; model IDs absent from manifest: ", paste(unknown_model, collapse = ", ")
      )
    ))
  }

  replacement_rows <- included[nzchar(included$replacement_for), , drop = FALSE]
  replacement_models <- unique(replacement_rows$model_id)
  missing_replacement_base <- replacement_models[!vapply(
    replacement_models,
    function(model_id) {
      row <- manifest[manifest$model_id == model_id, , drop = FALSE]
      nrow(row) == 1L && nzchar(row$base_model_id[[1]])
    },
    logical(1)
  )]
  if (length(missing_replacement_base) == 0L) {
    add(p3_qc_pass("Replacement manifest bases", paste(nrow(replacement_rows), "replacement rows map to an explicit base model")))
  } else {
    add(p3_qc_fail(
      "Replacement manifest bases",
      paste("Sensitivity model lacks base_model_id:", paste(missing_replacement_base, collapse = ", "))
    ))
  }

  effect_lock <- tables$Effect_Decision_Lock_v3_1
  lock_id_col <- p3_find_column(
    effect_lock,
    c("effect_id", "source_analysis_id", "record_id", "source_effect_id"),
    required = TRUE,
    table = "Effect_Decision_Lock_v3_1"
  )
  lock_include_col <- p3_find_column(
    effect_lock,
    c("analysis_include", "include", "run_effect"),
    required = TRUE,
    table = "Effect_Decision_Lock_v3_1"
  )
  lock_ids <- p3_text(effect_lock[[lock_id_col]])
  lock_yes <- lock_ids[p3_is_yes(effect_lock[[lock_include_col]])]
  lock_no <- lock_ids[!p3_is_yes(effect_lock[[lock_include_col]]) & nzchar(lock_ids)]
  missing_lock <- setdiff(included$effect_id, lock_yes)
  excluded_lock_leak <- intersect(included$effect_id, lock_no)
  if (length(missing_lock) == 0L && length(excluded_lock_leak) == 0L) {
    add(p3_qc_pass("Effect decision lock", "All included effects are explicitly locked Yes; no locked exclusion leaked"))
  } else {
    add(p3_qc_fail(
      "Effect decision lock",
      paste0(
        "Included effects not locked Yes: ", paste(missing_lock, collapse = ", "),
        "; locked exclusions included: ", paste(excluded_lock_leak, collapse = ", ")
      )
    ))
  }

  preserved <- P3_INHERITED_SHEETS[P3_INHERITED_SHEETS %in% P3_ANALYTIC_SHEETS]
  preserved_missing <- preserved[!vapply(
    preserved,
    function(sheet) any(grepl(sheet, manifest$source_sheet, fixed = TRUE)),
    logical(1)
  )]
  if (length(preserved_missing) == 0L) {
    add(p3_qc_pass("Inherited module manifest coverage", "Event/Temporal, Updating, MPT, Grey, and Gaussian modules remain represented"))
  } else {
    add(p3_qc_fail(
      "Inherited module manifest coverage",
      paste("Manifest omits:", paste(preserved_missing, collapse = ", "))
    ))
  }

  main_smd <- effects[effects$source_sheet == "Main_SMD_v3_1", , drop = FALSE]
  s023_exp2_main <- p3_matches_study(main_smd$row_text, "S023") &
    p3_matches_experiment(main_smd$row_text, 2L)
  if (sum(s023_exp2_main) == 0L) {
    add(p3_qc_pass("S023 Experiment 2 main-SMD exclusion", "0 included records"))
  } else {
    add(p3_qc_fail("S023 Experiment 2 main-SMD exclusion", paste(sum(s023_exp2_main), "included records found")))
  }

  analytical_s023_exp3 <- p3_matches_study(effects$row_text, "S023") &
    p3_matches_experiment(effects$row_text, 3L)
  quarantine_text <- p3_row_text(tables$Quarantined_Effects_v3_1)
  quarantined_s023_exp3 <- p3_matches_study(quarantine_text, "S023") &
    p3_matches_experiment(quarantine_text, 3L)
  if (sum(analytical_s023_exp3) == 0L && sum(quarantined_s023_exp3) == 2L) {
    add(p3_qc_pass("S023 Experiment 3 quarantine", "2 quarantined records and 0 analytical records"))
  } else {
    add(p3_qc_fail(
      "S023 Experiment 3 quarantine",
      paste0("Expected analytical=0 and quarantined=2; found analytical=", sum(analytical_s023_exp3),
             " and quarantined=", sum(quarantined_s023_exp3))
    ))
  }

  s041 <- included[p3_matches_study(included$row_text, "S041"), , drop = FALSE]
  s041_primary <- s041[grepl("PRIMARY", toupper(s041$analysis_role)), , drop = FALSE]
  s041_p2_primary <- p3_matches_paradigm(s041_primary$row_text, 2L)
  s041_primary_clusters <- unique(s041_primary$dependency_cluster[nzchar(s041_primary$dependency_cluster)])
  if (nrow(s041_primary) > 0L && sum(s041_p2_primary) == 0L && length(s041_primary_clusters) == 1L) {
    add(p3_qc_pass("S041 primary paradigm", "Paradigm 1 is the sole primary representative"))
  } else {
    add(p3_qc_fail(
      "S041 primary paradigm",
      paste0("Paradigm 2 primary rows=", sum(s041_p2_primary),
             "; primary rows=", nrow(s041_primary),
             "; primary independent clusters=", length(s041_primary_clusters))
    ))
  }

  s016 <- included[
    included$source_sheet == "Main_Nonlinear_v3_1" &
      p3_matches_study(included$row_text, "S016"),
    , drop = FALSE
  ]
  s016_clusters <- unique(s016$dependency_cluster[nzchar(s016$dependency_cluster)])
  s016_quadratic <- grepl("QUADRATIC|二次", paste(s016$estimand, s016$row_text), ignore.case = TRUE)
  s016_s041_shared_models <- intersect(
    unique(s016$model_id[nzchar(s016$model_id)]),
    unique(s041$model_id[nzchar(s041$model_id)])
  )
  if (nrow(s016) == 4L && length(s016_clusters) == 4L && all(s016_quadratic) &&
      length(s016_s041_shared_models) == 0L) {
    add(p3_qc_pass("S016 quadratic primary structure", "Four independent quadratic effects; no pooling with S041"))
  } else {
    add(p3_qc_fail(
      "S016 quadratic primary structure",
      paste0(
        "Expected four rows/four clusters/quadratic estimand/no shared S041 model; found rows=",
        nrow(s016), ", clusters=", length(s016_clusters), ", all_quadratic=", all(s016_quadratic),
        ", shared_models=", paste(s016_s041_shared_models, collapse = ", ")
      )
    ))
  }

  s023 <- included[p3_matches_study(included$row_text, "S023"), , drop = FALSE]
  s023_exp1_recollection <- p3_matches_experiment(s023$row_text, 1L) &
    grepl("RECOLLECT", s023$row_text, ignore.case = TRUE) &
    grepl("PRIMARY", toupper(s023$analysis_role))
  if (sum(s023_exp1_recollection) >= 1L) {
    add(p3_qc_pass("S023 Experiment 1 primary outcome", "Recollection is present as the primary outcome"))
  } else {
    add(p3_qc_fail("S023 Experiment 1 primary outcome", "No primary Experiment 1 recollection record found"))
  }

  s023_exp2 <- s023[p3_matches_experiment(s023$row_text, 2L), , drop = FALSE]
  s023_exp2_ok <- nrow(s023_exp2) == 0L || all(grepl("SENSIT", toupper(s023_exp2$analysis_role)))
  if (s023_exp2_ok) {
    add(p3_qc_pass("S023 Experiment 2 role", paste(nrow(s023_exp2), "sensitivity-only records")))
  } else {
    add(p3_qc_fail("S023 Experiment 2 role", "Experiment 2 appeared outside sensitivity analysis"))
  }

  familiarity <- included[
    p3_matches_study(included$row_text, "S023") &
      grepl("FAMILIAR", included$row_text, ignore.case = TRUE),
    , drop = FALSE
  ]
  recollection <- included[
    p3_matches_study(included$row_text, "S023") &
      grepl("RECOLLECT", included$row_text, ignore.case = TRUE),
    , drop = FALSE
  ]
  familiarity_ok <- nrow(familiarity) == 0L || (
    nrow(recollection) > 0L &&
      all(familiarity$dependency_cluster %in% recollection$dependency_cluster) &&
      all(nzchar(familiarity$dependency_cluster))
  )
  if (familiarity_ok) {
    add(p3_qc_pass("S023 familiarity dependence", "Familiarity is absent or shares the recollection dependency cluster"))
  } else {
    add(p3_qc_fail("S023 familiarity dependence", "Familiarity would add an independent cluster or lacks a dependence mapping"))
  }

  s040 <- included[
    p3_matches_study(included$row_text, "S040") &
      grepl("PRIMARY", toupper(included$analysis_role)),
    , drop = FALSE
  ]
  s040_raw_paired <- any(grepl("RAW|PAIRED", s040$row_text, ignore.case = TRUE))
  s040_limitation <- any(grepl("ADJUST|REPRODUC|LIMIT", s040$row_text, ignore.case = TRUE))
  if (nrow(s040) > 0L && s040_raw_paired && s040_limitation) {
    add(p3_qc_pass("S040 reproducible primary effect", "Raw/paired behavior is primary and adjusted-model limitation is retained"))
  } else {
    add(p3_qc_fail(
      "S040 reproducible primary effect",
      paste0("primary_rows=", nrow(s040), "; raw_or_paired=", s040_raw_paired,
             "; adjusted_limitation_recorded=", s040_limitation)
    ))
  }

  replacement_patterns <- list(
    "S041 Paradigm 2" = p3_matches_study(included$row_text, "S041") & p3_matches_paradigm(included$row_text, 2L),
    "S010 N=71 subset" = p3_matches_study(included$row_text, "S010") & grepl("N[ =]*71|PUBLIC SUBSET", included$row_text, ignore.case = TRUE),
    "S021 N=35 subgroup" = p3_matches_study(included$row_text, "S021") & grepl("N[ =]*35|YOUNG", included$row_text, ignore.case = TRUE),
    "Boundary/alternative rows" = grepl("BOUNDARY.EXCLUD|ALTERNATIVE MODEL|ALTERNATIVE OUTCOME", included$row_text, ignore.case = TRUE)
  )
  for (label in names(replacement_patterns)) {
    rows <- included[replacement_patterns[[label]], , drop = FALSE]
    ok <- nrow(rows) == 0L || all(
      grepl("SENSIT", toupper(rows$analysis_role)) & nzchar(rows$replacement_for)
    )
    if (ok) {
      add(p3_qc_pass(paste0("Replacement rule: ", label), paste(nrow(rows), "eligible replacement records")))
    } else {
      add(p3_qc_fail(
        paste0("Replacement rule: ", label),
        paste("Rows must be sensitivity replacements:", paste(rows$effect_id, collapse = ", "))
      ))
    }
  }

  direction <- tables$Direction_Audit_v3_1
  direction_id_col <- p3_find_column(
    direction,
    c("effect_id", "source_effect_id", "record_id"),
    required = TRUE,
    table = "Direction_Audit_v3_1"
  )
  direction_ids <- p3_text(direction[[direction_id_col]])
  direction_status <- p3_direction_audit_status(direction)
  allowed_direction_status <- direction_status %in% c(
    "PASS", "LOCKED", "APPROVED", "VERIFIED", "DIRECTION_LOCKED"
  )
  missing_direction <- setdiff(included$effect_id, direction_ids[allowed_direction_status])
  if (length(missing_direction) == 0L) {
    add(p3_qc_pass("Direction audit coverage", "All included effects have a locked/approved direction audit"))
  } else {
    add(p3_qc_fail(
      "Direction audit coverage",
      paste("Missing approved audit:", paste(missing_direction, collapse = ", "))
    ))
  }

  add(p3_qc_pass(
    "analysis_include gate",
    paste0(
      nrow(included), " records eligible; ", nrow(effects) - nrow(included),
      " non-Yes records excluded before block construction"
    )
  ))

  out <- do.call(rbind, qc)
  rownames(out) <- NULL
  out
}

p3_qc_has_critical_failure <- function(qc) {
  any(qc$critical & qc$status != "PASS")
}

# Manifest and block construction ------------------------------------------

p3_manifest_row <- function(manifest, model_id) {
  row <- manifest[manifest$model_id == model_id, , drop = FALSE]
  if (nrow(row) != 1L) {
    stop("Expected exactly one manifest row for model_id ", model_id, call. = FALSE)
  }
  row
}

p3_build_replacement_dataset <- function(primary, replacements) {
  if (nrow(replacements) == 0L) return(primary)
  if (any(!nzchar(replacements$replacement_for))) {
    stop("Every replacement effect must name replacement_for.", call. = FALSE)
  }
  missing_targets <- setdiff(replacements$replacement_for, primary$effect_id)
  if (length(missing_targets) > 0L) {
    stop("Replacement targets are absent from the primary block: ", paste(missing_targets, collapse = ", "), call. = FALSE)
  }
  target_clusters <- primary$dependency_cluster[
    match(replacements$replacement_for, primary$effect_id)
  ]
  if (any(!nzchar(target_clusters)) ||
      any(replacements$dependency_cluster != target_clusters)) {
    stop(
      "Replacement rows must retain the dependency_cluster of the effect they replace and must not append a new k.",
      call. = FALSE
    )
  }
  kept <- primary[!primary$effect_id %in% replacements$replacement_for, , drop = FALSE]
  out <- rbind(kept, replacements)
  if (anyDuplicated(out$effect_id)) stop("Replacement dataset has duplicate effect_id values.", call. = FALSE)

  old_clusters <- unique(primary$dependency_cluster[nzchar(primary$dependency_cluster)])
  new_clusters <- unique(out$dependency_cluster[nzchar(out$dependency_cluster)])
  if (length(old_clusters) != length(new_clusters)) {
    stop(
      "Replacement sensitivity changed the independent-cluster count from ",
      length(old_clusters), " to ", length(new_clusters),
      ". Replacement rows must not append a new k.",
      call. = FALSE
    )
  }
  rownames(out) <- NULL
  out
}

p3_build_analysis_blocks <- function(effects, manifest) {
  included <- effects[p3_is_yes(effects$analysis_include), , drop = FALSE]
  runnable <- manifest[p3_is_yes(manifest$run_model), , drop = FALSE]
  blocks <- list()

  for (i in seq_len(nrow(runnable))) {
    spec <- runnable[i, , drop = FALSE]
    model_id <- spec$model_id[[1]]
    d <- included[included$model_id == model_id, , drop = FALSE]

    if (nzchar(spec$base_model_id[[1]]) && nrow(d) > 0L) {
      base <- included[included$model_id == spec$base_model_id[[1]], , drop = FALSE]
      d <- p3_build_replacement_dataset(base, d)
    }

    blocks[[model_id]] <- list(spec = spec, data = d)
  }
  blocks
}

p3_unique_nonblank <- function(x) unique(p3_text(x)[nzchar(p3_text(x))])

p3_check_compatibility <- function(d, spec) {
  if (nrow(d) == 0L) return(list(ok = FALSE, reason = "No included effects"))
  fields <- c("effect_scale", "estimand", "pe_encoding")
  problems <- character()
  for (field in fields) {
    values <- p3_unique_nonblank(d[[field]])
    expected <- p3_text(spec[[field]])
    if (length(values) > 1L) {
      problems <- c(problems, paste0(field, " mixes ", paste(values, collapse = " / ")))
    } else if (nzchar(expected) && length(values) == 1L && !identical(tolower(values), tolower(expected))) {
      problems <- c(problems, paste0(field, " does not match manifest"))
    }
  }
  outcomes <- p3_unique_nonblank(d$outcome)
  if (length(outcomes) > 1L && !p3_is_yes(spec$allow_mixed_outcome)) {
    problems <- c(problems, paste0("outcome mixes ", paste(outcomes, collapse = " / ")))
  }
  if (length(problems) > 0L) {
    return(list(ok = FALSE, reason = paste(problems, collapse = "; ")))
  }
  list(ok = TRUE, reason = "Manifest, scale, encoding, outcome, and estimand are compatible")
}

# Sampling covariance and model fitting -----------------------------------

p3_covariance_long <- function(df, table = "covariance table") {
  row_col <- p3_find_column(
    df,
    c("row_effect_id", "effect_id_row", "row_id"),
    required = FALSE,
    table = table
  )
  col_col <- p3_find_column(
    df,
    c("col_effect_id", "effect_id_col", "column_id", "col_id"),
    required = FALSE,
    table = table
  )
  value_col <- p3_find_column(
    df,
    c("covariance", "sampling_covariance", "cov", "value"),
    required = FALSE,
    table = table
  )
  model_col <- p3_find_column(
    df,
    c("model_id", "analysis_block"),
    required = FALSE,
    table = table
  )

  if (!is.na(row_col) && !is.na(col_col) && !is.na(value_col)) {
    return(data.frame(
      row_effect_id = p3_text(df[[row_col]]),
      col_effect_id = p3_text(df[[col_col]]),
      covariance = suppressWarnings(as.numeric(df[[value_col]])),
      model_id = if (is.na(model_col)) "" else p3_text(df[[model_col]]),
      stringsAsFactors = FALSE
    ))
  }

  id_col <- p3_find_column(
    df,
    c("effect_id", "row_effect_id", "row_id"),
    required = TRUE,
    table = table
  )
  value_names <- setdiff(names(df), id_col)
  if (length(value_names) == 0L) {
    stop(table, " is neither a long nor a wide covariance matrix.", call. = FALSE)
  }
  rows <- vector("list", nrow(df) * length(value_names))
  index <- 0L
  for (i in seq_len(nrow(df))) {
    for (column in value_names) {
      index <- index + 1L
      rows[[index]] <- data.frame(
        row_effect_id = p3_text(df[[id_col]][[i]]),
        col_effect_id = column,
        covariance = suppressWarnings(as.numeric(df[[column]][[i]])),
        model_id = "",
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

p3_lookup_covariance <- function(long, row_id, col_id, model_id = "") {
  relevant <- long[
    long$row_effect_id == row_id & long$col_effect_id == col_id &
      (!nzchar(long$model_id) | long$model_id == model_id),
    , drop = FALSE
  ]
  reverse <- long[
    long$row_effect_id == col_id & long$col_effect_id == row_id &
      (!nzchar(long$model_id) | long$model_id == model_id),
    , drop = FALSE
  ]
  values <- c(relevant$covariance, reverse$covariance)
  values <- values[is.finite(values)]
  if (length(values) == 0L) return(NA_real_)
  if (max(values) - min(values) > 1e-10) {
    stop("Conflicting covariance values for ", row_id, " and ", col_id, call. = FALSE)
  }
  values[[1]]
}

p3_build_sampling_v <- function(
  d,
  covariance_table,
  model_id,
  require_complete = FALSE,
  require_five = FALSE
) {
  if (anyDuplicated(d$effect_id)) {
    stop(model_id, " contains duplicate effect_id values.", call. = FALSE)
  }
  if (require_five && nrow(d) != 5L) {
    stop("A008 requires exactly five locked effects.", call. = FALSE)
  }
  if (any(!is.finite(d$vi) | d$vi <= 0)) {
    stop(model_id, " has missing or invalid sampling variances.", call. = FALSE)
  }

  long <- p3_covariance_long(covariance_table, table = paste0(model_id, " covariance"))
  ids <- d$effect_id
  V <- matrix(0, nrow = nrow(d), ncol = nrow(d), dimnames = list(ids, ids))
  diag(V) <- d$vi

  for (i in seq_len(nrow(d))) {
    for (j in seq_len(nrow(d))) {
      if (i == j) {
        supplied <- p3_lookup_covariance(long, ids[[i]], ids[[j]], model_id)
        if (is.finite(supplied) && abs(supplied - d$vi[[i]]) > pmax(1e-10, 1e-6 * abs(d$vi[[i]]))) {
          stop("Covariance diagonal does not match vi for ", ids[[i]], call. = FALSE)
        }
        if (require_complete && !is.finite(supplied)) {
          stop("Complete covariance matrix is missing diagonal cell for ", ids[[i]], call. = FALSE)
        }
        next
      }

      same_cluster <- nzchar(d$dependency_cluster[[i]]) &&
        identical(d$dependency_cluster[[i]], d$dependency_cluster[[j]])
      supplied <- p3_lookup_covariance(long, ids[[i]], ids[[j]], model_id)
      if (is.finite(supplied)) {
        V[i, j] <- supplied
      } else if (same_cluster || require_complete) {
        stop(
          "Explicit covariance is missing for dependent effects ", ids[[i]],
          " and ", ids[[j]], ". No zero-covariance substitution was made.",
          call. = FALSE
        )
      }
    }
  }

  if (!isTRUE(all.equal(V, t(V), tolerance = 1e-10))) {
    stop(model_id, " sampling covariance matrix is not symmetric.", call. = FALSE)
  }
  eigenvalues <- eigen(V, symmetric = TRUE, only.values = TRUE)$values
  if (min(eigenvalues) < -1e-8) {
    stop(model_id, " sampling covariance matrix is not positive semidefinite.", call. = FALSE)
  }
  V
}

p3_cluster_counts <- function(d) {
  clusters <- p3_text(d$dependency_cluster)
  missing <- !nzchar(clusters)
  sizes <- table(clusters[!missing])
  list(
    missing = any(missing),
    independent_clusters = as.integer(length(sizes)),
    repeated_effect_clusters = as.integer(sum(sizes > 1L)),
    sizes = sizes
  )
}

p3_cr2_eligibility <- function(d) {
  counts <- p3_cluster_counts(d)
  if (nrow(d) <= 1L || counts$repeated_effect_clusters == 0L) {
    return(list(
      status = "NOT_APPLICABLE",
      reason = "No dependency cluster contributes multiple effects",
      independent_clusters = counts$independent_clusters,
      repeated_effect_clusters = counts$repeated_effect_clusters
    ))
  }
  if (counts$missing) {
    return(list(
      status = "SKIPPED_MISSING_CLUSTER",
      reason = "At least one dependent effect lacks dependency_cluster",
      independent_clusters = counts$independent_clusters,
      repeated_effect_clusters = counts$repeated_effect_clusters
    ))
  }
  if (counts$independent_clusters < 4L) {
    return(list(
      status = "SKIPPED_LT4_CLUSTERS",
      reason = "CR2 requires at least four independent clusters",
      independent_clusters = counts$independent_clusters,
      repeated_effect_clusters = counts$repeated_effect_clusters
    ))
  }
  list(
    status = "ELIGIBLE",
    reason = "Dependent effects with at least four independent clusters",
    independent_clusters = counts$independent_clusters,
    repeated_effect_clusters = counts$repeated_effect_clusters
  )
}

p3_result_template <- function() {
  data.frame(
    analysis_block = character(),
    coefficient = character(),
    analysis_role = character(),
    effect_scale = character(),
    estimand = character(),
    actual_k = integer(),
    independent_clusters = integer(),
    model_type = character(),
    reml_status = character(),
    hk_status = character(),
    cr2_status = character(),
    estimate = numeric(),
    se = numeric(),
    ci_lb = numeric(),
    ci_ub = numeric(),
    p_value = numeric(),
    tau2 = numeric(),
    I2 = numeric(),
    Q = numeric(),
    Q_p = numeric(),
    input_sheet = character(),
    sensitivity_replacement_rule = character(),
    stringsAsFactors = FALSE
  )
}

p3_model_metadata <- function(d, spec) {
  counts <- p3_cluster_counts(d)
  first_or <- function(x, fallback) {
    values <- p3_unique_nonblank(x)
    if (length(values) > 0L) values[[1]] else fallback
  }
  list(
    role = if (nzchar(spec$analysis_role[[1]])) spec$analysis_role[[1]] else first_or(d$analysis_role, "UNSPECIFIED"),
    scale = if (nzchar(spec$effect_scale[[1]])) spec$effect_scale[[1]] else paste(p3_unique_nonblank(d$effect_scale), collapse = " | "),
    estimand = if (nzchar(spec$estimand[[1]])) spec$estimand[[1]] else paste(p3_unique_nonblank(d$estimand), collapse = " | "),
    k = nrow(d),
    clusters = counts$independent_clusters,
    sheets = paste(unique(d$source_sheet), collapse = " | "),
    sensitivity_rule = if (nzchar(spec$sensitivity_rule[[1]])) {
      spec$sensitivity_rule[[1]]
    } else {
      paste(p3_unique_nonblank(d$sensitivity_rule), collapse = " | ")
    }
  )
}

p3_descriptive_singleton <- function(d, spec) {
  meta <- p3_model_metadata(d, spec)
  z <- d$yi[[1]] / d$sei[[1]]
  result <- p3_result_template()
  result[1, ] <- list(
    spec$model_id[[1]],
    "intercept",
    meta$role,
    meta$scale,
    meta$estimand,
    1L,
    meta$clusters,
    "descriptive_singleton",
    "NOT_RUN",
    "NOT_APPLICABLE",
    "NOT_APPLICABLE",
    d$yi[[1]],
    d$sei[[1]],
    d$yi[[1]] - stats::qnorm(0.975) * d$sei[[1]],
    d$yi[[1]] + stats::qnorm(0.975) * d$sei[[1]],
    2 * stats::pnorm(abs(z), lower.tail = FALSE),
    NA_real_, NA_real_, NA_real_, NA_real_,
    meta$sheets,
    meta$sensitivity_rule
  )
  list(fit = NULL, result = result, model_kind = "singleton", V = NULL, cr2_test = NULL)
}

p3_fit_independent <- function(d, spec) {
  meta <- p3_model_metadata(d, spec)
  if (meta$clusters != nrow(d)) {
    stop("Independent model requested but k differs from independent cluster count.", call. = FALSE)
  }
  fit <- metafor::rma.uni(
    yi = d$yi,
    vi = d$vi,
    method = "REML",
    test = "knha"
  )
  result <- p3_result_template()
  result[1, ] <- list(
    spec$model_id[[1]], "intercept", meta$role, meta$scale, meta$estimand,
    nrow(d), meta$clusters, "rma.uni", "REML", "Hartung-Knapp",
    "NOT_APPLICABLE", as.numeric(fit$b), as.numeric(fit$se),
    as.numeric(fit$ci.lb), as.numeric(fit$ci.ub), as.numeric(fit$pval),
    as.numeric(fit$tau2), as.numeric(fit$I2), as.numeric(fit$QE), as.numeric(fit$QEp),
    meta$sheets, meta$sensitivity_rule
  )
  list(fit = fit, result = result, model_kind = "rma_uni", V = NULL, cr2_test = NULL)
}

p3_extract_cr2 <- function(test, fit, spec, d, base_result) {
  frame <- as.data.frame(test)
  get_column <- function(candidates, label) {
    column <- intersect(candidates, names(frame))
    if (length(column) == 0L) {
      stop(
        "CR2 output lacks ", label, ". Available columns: ",
        paste(names(frame), collapse = ", "),
        call. = FALSE
      )
    }
    as.numeric(frame[[column[[1]]]])
  }
  estimate <- get_column(c("beta", "Estimate", "estimate"), "estimate")
  se <- get_column(c("SE", "se"), "standard error")
  df <- get_column(c("df_Satt", "df", "d.f."), "Satterthwaite df")
  p <- get_column(c("p_Satt", "p_val", "p-value", "p"), "p value")
  coefficients <- names(stats::coef(fit))
  if (length(coefficients) != length(estimate)) coefficients <- paste0("coefficient_", seq_along(estimate))
  critical <- stats::qt(0.975, df = df)

  result <- base_result[rep(1L, length(estimate)), , drop = FALSE]
  result$coefficient <- coefficients
  result$cr2_status <- "CR2_SATTERTHWAITE"
  result$estimate <- estimate
  result$se <- se
  result$ci_lb <- estimate - critical * se
  result$ci_ub <- estimate + critical * se
  result$p_value <- p
  rownames(result) <- NULL
  result
}

p3_fit_dependent <- function(d, spec, V, a008 = FALSE) {
  meta <- p3_model_metadata(d, spec)
  eligibility <- p3_cr2_eligibility(d)
  fit <- if (a008) {
    metafor::rma.mv(
      yi = d$yi,
      V = V,
      random = ~ 1 | effect_id,
      data = d,
      method = "REML",
      test = "t"
    )
  } else {
    metafor::rma.mv(
      yi = d$yi,
      V = V,
      random = ~ 1 | dependency_cluster/effect_id,
      data = d,
      method = "REML",
      test = "t"
    )
  }
  tau2 <- if (length(fit$sigma2) > 0L) as.numeric(fit$sigma2[[1]]) else NA_real_
  mean_sampling <- mean(diag(V))
  i2 <- if (is.finite(tau2)) 100 * tau2 / (tau2 + mean_sampling) else NA_real_
  result <- p3_result_template()
  result[1, ] <- list(
    spec$model_id[[1]], "intercept", meta$role, meta$scale, meta$estimand,
    nrow(d), meta$clusters, "rma.mv_known_V", "REML", "NOT_APPLICABLE",
    eligibility$status, as.numeric(fit$b), as.numeric(fit$se),
    as.numeric(fit$ci.lb), as.numeric(fit$ci.ub), as.numeric(fit$pval),
    tau2, i2, as.numeric(fit$QE), as.numeric(fit$QEp),
    meta$sheets, meta$sensitivity_rule
  )

  cr2_test <- NULL
  if (identical(eligibility$status, "ELIGIBLE")) {
    cr2_test <- clubSandwich::coef_test(
      fit,
      vcov = "CR2",
      cluster = d$dependency_cluster,
      test = "Satterthwaite"
    )
    result <- p3_extract_cr2(cr2_test, fit, spec, d, result)
  }
  list(fit = fit, result = result, model_kind = "rma_mv", V = V, cr2_test = cr2_test)
}

p3_fit_block <- function(block, tables) {
  d <- block$data
  spec <- block$spec
  compatibility <- p3_check_compatibility(d, spec)
  if (!compatibility$ok) stop(compatibility$reason, call. = FALSE)
  if (any(!is.finite(d$yi) | !is.finite(d$sei) | d$sei <= 0 | !is.finite(d$vi) | d$vi <= 0)) {
    stop("Included block contains missing or invalid precision; no imputation was used.", call. = FALSE)
  }
  known_v_sheet <- spec$known_v_sheet[[1]]
  is_a008 <- identical(known_v_sheet, "V_Matrix_A008") ||
    grepl("A008", paste(d$row_text, collapse = " | "), ignore.case = TRUE)
  if (is_a008) {
    a008_clusters <- p3_text(d$shared_control_block)
    if (any(!nzchar(a008_clusters)) || length(unique(a008_clusters)) != 2L) {
      stop("A008 requires exactly two non-missing shared_control_block clusters.", call. = FALSE)
    }
    d$dependency_cluster <- a008_clusters
  }

  if (nrow(d) == 1L) {
    result <- p3_descriptive_singleton(d, spec)
    result$analysis_data <- d
    return(result)
  }

  counts <- p3_cluster_counts(d)
  if (counts$missing) stop("Included block lacks dependency_cluster.", call. = FALSE)
  force_known_v <- is_a008 || nzchar(known_v_sheet)
  if (counts$repeated_effect_clusters == 0L && !force_known_v) {
    result <- p3_fit_independent(d, spec)
    result$analysis_data <- d
    return(result)
  }

  covariance_sheet <- if (is_a008) {
    "V_Matrix_A008"
  } else if (nzchar(known_v_sheet)) {
    known_v_sheet
  } else {
    "Precision_Covariance_v3_1"
  }
  if (!covariance_sheet %in% names(tables)) {
    stop("Required covariance sheet is absent: ", covariance_sheet, call. = FALSE)
  }
  V <- p3_build_sampling_v(
    d,
    tables[[covariance_sheet]],
    model_id = spec$model_id[[1]],
    require_complete = is_a008,
    require_five = is_a008
  )
  result <- p3_fit_dependent(d, spec, V, a008 = is_a008)
  result$analysis_data <- d
  result
}

# Locked special cases -----------------------------------------------------

p3_build_s016_joint_data <- function(df) {
  text <- p3_row_text(df)
  rows <- df[p3_matches_study(text, "S016"), , drop = FALSE]
  include_col <- p3_find_column(
    rows,
    c("analysis_include", "include", "run_effect"),
    required = TRUE,
    table = "Main_Nonlinear_v3_1"
  )
  rows <- rows[p3_is_yes(rows[[include_col]]), , drop = FALSE]
  if (nrow(rows) != 4L) {
    stop("S016 joint L/Q model requires exactly four independent experiments; found ", nrow(rows), call. = FALSE)
  }

  id_col <- p3_find_column(
    rows,
    c("effect_id", "effect_id_quadratic", "analysis_id"),
    required = TRUE,
    table = "Main_Nonlinear_v3_1"
  )
  cluster_col <- p3_find_column(
    rows,
    c("dependency_cluster", "sample_id", "independent_sample_id"),
    required = TRUE,
    table = "Main_Nonlinear_v3_1"
  )
  linear_col <- p3_find_column(rows, c("beta_linear", "linear_estimate"), TRUE, "Main_Nonlinear_v3_1")
  linear_se_col <- p3_find_column(rows, c("se_linear", "sei_linear"), TRUE, "Main_Nonlinear_v3_1")
  linear_vi_col <- p3_find_column(rows, c("vi_linear", "variance_linear"), TRUE, "Main_Nonlinear_v3_1")
  quad_col <- p3_find_column(rows, c("beta_quadratic", "quadratic_estimate", "yi"), TRUE, "Main_Nonlinear_v3_1")
  quad_se_col <- p3_find_column(rows, c("se_quadratic", "sei_quadratic", "sei"), TRUE, "Main_Nonlinear_v3_1")
  quad_vi_col <- p3_find_column(rows, c("vi_quadratic", "variance_quadratic", "vi"), TRUE, "Main_Nonlinear_v3_1")
  cov_col <- p3_find_column(
    rows,
    c("cov_linear_quadratic", "linear_quadratic_covariance", "cov_lq"),
    TRUE,
    "Main_Nonlinear_v3_1"
  )

  ids <- p3_text(rows[[id_col]])
  clusters <- p3_text(rows[[cluster_col]])
  if (any(!nzchar(ids)) || any(!nzchar(clusters)) || length(unique(clusters)) != 4L) {
    stop("S016 must provide four non-missing, distinct dependency clusters.", call. = FALSE)
  }

  beta_l <- suppressWarnings(as.numeric(rows[[linear_col]]))
  se_l <- suppressWarnings(as.numeric(rows[[linear_se_col]]))
  vi_l <- suppressWarnings(as.numeric(rows[[linear_vi_col]]))
  beta_q <- suppressWarnings(as.numeric(rows[[quad_col]]))
  se_q <- suppressWarnings(as.numeric(rows[[quad_se_col]]))
  vi_q <- suppressWarnings(as.numeric(rows[[quad_vi_col]]))
  cov_lq <- suppressWarnings(as.numeric(rows[[cov_col]]))
  valid <- is.finite(beta_l) & is.finite(se_l) & se_l > 0 & is.finite(vi_l) & vi_l > 0 &
    is.finite(beta_q) & is.finite(se_q) & se_q > 0 & is.finite(vi_q) & vi_q > 0 &
    is.finite(cov_lq) & abs(vi_l - se_l^2) <= pmax(1e-10, 1e-6 * vi_l) &
    abs(vi_q - se_q^2) <= pmax(1e-10, 1e-6 * vi_q)
  if (!all(valid)) {
    stop("S016 joint L/Q precision or recorded covariance is incomplete; no value was imputed.", call. = FALSE)
  }

  joint <- data.frame(
    effect_id = as.vector(rbind(paste0(ids, "__L"), paste0(ids, "__Q"))),
    parent_effect_id = rep(ids, each = 2L),
    dependency_cluster = rep(clusters, each = 2L),
    term = factor(rep(c("linear", "quadratic"), times = 4L), levels = c("linear", "quadratic")),
    yi = as.vector(rbind(beta_l, beta_q)),
    vi = as.vector(rbind(vi_l, vi_q)),
    stringsAsFactors = FALSE
  )
  V <- matrix(0, nrow = 8L, ncol = 8L, dimnames = list(joint$effect_id, joint$effect_id))
  for (i in seq_len(4L)) {
    index <- (2L * i - 1L):(2L * i)
    V[index, index] <- matrix(c(vi_l[[i]], cov_lq[[i]], cov_lq[[i]], vi_q[[i]]), nrow = 2L)
  }
  if (min(eigen(V, symmetric = TRUE, only.values = TRUE)$values) < -1e-8) {
    stop("S016 recorded linear-quadratic covariance produces a non-PSD V matrix.", call. = FALSE)
  }
  list(data = joint, V = V)
}

p3_fit_s016_joint <- function(df) {
  joint <- p3_build_s016_joint_data(df)
  d <- joint$data
  fit <- metafor::rma.mv(
    yi = d$yi,
    V = joint$V,
    mods = ~ term - 1,
    random = ~ term | dependency_cluster,
    struct = "UN",
    data = d,
    method = "REML"
  )
  test <- clubSandwich::coef_test(
    fit,
    vcov = "CR2",
    cluster = d$dependency_cluster,
    test = "Satterthwaite"
  )
  frame <- as.data.frame(test)
  get_column <- function(candidates, label) {
    column <- intersect(candidates, names(frame))
    if (length(column) == 0L) stop("S016 CR2 output lacks ", label, call. = FALSE)
    as.numeric(frame[[column[[1]]]])
  }
  estimate <- get_column(c("beta", "Estimate", "estimate"), "estimate")
  se <- get_column(c("SE", "se"), "SE")
  df_satt <- get_column(c("df_Satt", "df", "d.f."), "Satterthwaite df")
  p <- get_column(c("p_Satt", "p_val", "p-value", "p"), "p value")
  critical <- stats::qt(0.975, df = df_satt)
  coefficients <- names(stats::coef(fit))
  tau2 <- if (length(fit$tau2) > 0L) mean(as.numeric(fit$tau2)) else NA_real_
  result <- p3_result_template()
  result <- result[rep(1L, length(estimate)), , drop = FALSE]
  result$analysis_block <- "S016_JOINT_LQ"
  result$coefficient <- coefficients
  result$analysis_role <- "ANCILLARY_JOINT_MODEL"
  result$effect_scale <- "S016 native polynomial coefficients"
  result$estimand <- "joint linear and quadratic terms"
  result$actual_k <- 8L
  result$independent_clusters <- 4L
  result$model_type <- "rma.mv_joint_LQ_known_V"
  result$reml_status <- "REML"
  result$hk_status <- "NOT_APPLICABLE"
  result$cr2_status <- "CR2_SATTERTHWAITE"
  result$estimate <- estimate
  result$se <- se
  result$ci_lb <- estimate - critical * se
  result$ci_ub <- estimate + critical * se
  result$p_value <- p
  result$tau2 <- tau2
  result$I2 <- NA_real_
  result$Q <- as.numeric(fit$QE)
  result$Q_p <- as.numeric(fit$QEp)
  result$input_sheet <- "Main_Nonlinear_v3_1"
  result$sensitivity_replacement_rule <- "Ancillary joint L/Q model; quadratic-only estimand remains primary"
  list(fit = fit, result = result, model_kind = "s016_joint", V = joint$V, cr2_test = test)
}

p3_validate_a008_order <- function(d, V) {
  identical(rownames(V), d$effect_id) && identical(colnames(V), d$effect_id) && nrow(V) == 5L
}

# Diagnostics, risk of bias, and output -----------------------------------

p3_safe_filename <- function(x) gsub("[^A-Za-z0-9_-]", "_", x)

p3_skip_template <- function() {
  data.frame(
    analysis_block = character(),
    analysis_role = character(),
    actual_k = integer(),
    independent_clusters = integer(),
    reason = character(),
    error_message = character(),
    critical = logical(),
    stringsAsFactors = FALSE
  )
}

p3_skip_row <- function(spec, d, reason, error_message = "") {
  counts <- p3_cluster_counts(d)
  role <- if (nrow(spec) > 0L) spec$analysis_role[[1]] else "UNKNOWN"
  critical_value <- if (nrow(spec) > 0L && nzchar(spec$critical[[1]])) {
    p3_is_yes(spec$critical[[1]])
  } else {
    grepl("PRIMARY", toupper(role))
  }
  data.frame(
    analysis_block = if (nrow(spec) > 0L) spec$model_id[[1]] else "UNKNOWN",
    analysis_role = role,
    actual_k = nrow(d),
    independent_clusters = counts$independent_clusters,
    reason = reason,
    error_message = error_message,
    critical = critical_value,
    stringsAsFactors = FALSE
  )
}

p3_dependency_row <- function(model_id, d, cr2_status, details = "") {
  counts <- p3_cluster_counts(d)
  data.frame(
    analysis_block = model_id,
    actual_k = nrow(d),
    independent_clusters = counts$independent_clusters,
    repeated_effect_clusters = counts$repeated_effect_clusters,
    missing_cluster = counts$missing,
    cr2_status = cr2_status,
    details = details,
    stringsAsFactors = FALSE
  )
}

p3_manifest_output_row <- function(spec, d, status) {
  counts <- p3_cluster_counts(d)
  data.frame(
    analysis_block = spec$model_id[[1]],
    manifest_version = spec$manifest_version[[1]],
    analysis_role = spec$analysis_role[[1]],
    effect_scale = spec$effect_scale[[1]],
    estimand = spec$estimand[[1]],
    pe_encoding = spec$pe_encoding[[1]],
    outcome = spec$outcome[[1]],
    input_sheet = paste(unique(d$source_sheet), collapse = " | "),
    actual_k = nrow(d),
    independent_clusters = counts$independent_clusters,
    sensitivity_replacement_rule = spec$sensitivity_rule[[1]],
    status = status,
    stringsAsFactors = FALSE
  )
}

p3_write_forest <- function(fit_result, d, figure_dir, model_id) {
  if (is.null(fit_result$fit)) return(invisible(character()))
  paths <- c(
    pdf = file.path(figure_dir, paste0("forest_", p3_safe_filename(model_id), ".pdf")),
    png = file.path(figure_dir, paste0("forest_", p3_safe_filename(model_id), ".png"))
  )
  slab <- paste(d$article_id, d$study_id, d$effect_id, sep = " | ")
  draw <- function() {
    metafor::forest(
      fit_result$fit,
      slab = slab,
      xlab = paste(p3_unique_nonblank(d$effect_scale), collapse = " | "),
      main = model_id,
      cex = 0.8
    )
  }
  render <- function(open_device) {
    open_device()
    on.exit(grDevices::dev.off(), add = TRUE)
    draw()
  }
  render(function() grDevices::pdf(
    paths[["pdf"]], width = 8.5, height = max(5.5, 2.8 + 0.42 * nrow(d))
  ))
  render(function() grDevices::png(
    paths[["png"]], width = 1800, height = max(1200, 700 + 90 * nrow(d)), res = 180
  ))
  invisible(paths)
}

p3_run_diagnostics <- function(fit_result, d, model_id, table_dir) {
  rows <- list()
  if (identical(fit_result$model_kind, "rma_uni") && nrow(d) >= 4L) {
    leave <- as.data.frame(metafor::leave1out(fit_result$fit))
    leave$deleted_effect_id <- d$effect_id
    utils::write.csv(
      leave,
      file.path(table_dir, paste0("leave_one_out_", p3_safe_filename(model_id), ".csv")),
      row.names = FALSE,
      na = ""
    )
    influence_object <- stats::influence(fit_result$fit)
    influence_frame <- as.data.frame(influence_object$inf)
    influence_frame$effect_id <- d$effect_id
    utils::write.csv(
      influence_frame,
      file.path(table_dir, paste0("influence_", p3_safe_filename(model_id), ".csv")),
      row.names = FALSE,
      na = ""
    )
    rows[[1]] <- data.frame(
      analysis_block = model_id,
      diagnostic = "influence_and_leave_one_out",
      status = "RUN",
      details = paste(nrow(d), "compatible independent effects"),
      stringsAsFactors = FALSE
    )
  } else {
    rows[[1]] <- data.frame(
      analysis_block = model_id,
      diagnostic = "influence_and_leave_one_out",
      status = "NOT_RUN",
      details = "Requires a compatible rma.uni model with k>=4",
      stringsAsFactors = FALSE
    )
  }

  if (identical(fit_result$model_kind, "rma_uni") && nrow(d) >= 10L) {
    egger <- metafor::regtest(fit_result$fit, model = "rma")
    rows[[2]] <- data.frame(
      analysis_block = model_id,
      diagnostic = "egger",
      status = "RUN",
      details = paste0("z=", format(as.numeric(egger$zval), digits = 6),
                       "; p=", format(as.numeric(egger$pval), digits = 6)),
      stringsAsFactors = FALSE
    )
  } else {
    rows[[2]] <- data.frame(
      analysis_block = model_id,
      diagnostic = "egger",
      status = "NOT_RUN",
      details = "Requires a compatible rma.uni block with k>=10; no inferential funnel plot generated",
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

p3_read_rob_status <- function(path) {
  if (is.na(path)) {
    return(list(status = "PROVISIONAL_NOT_LOCKED", table = NULL, details = "No final locked RoB input supplied"))
  }
  sheets <- readxl::excel_sheets(path)
  raw_tables <- setNames(lapply(sheets, function(sheet) {
    readxl::read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = "unique")
  }), sheets)
  tables <- setNames(lapply(sheets, function(sheet) {
    readxl::read_excel(path, sheet = sheet, skip = 2, .name_repair = "unique")
  }), sheets)
  all_text <- toupper(paste(unlist(lapply(raw_tables, p3_row_text)), collapse = " | "))
  if (!grepl("FINAL[_ ]LOCKED", all_text)) {
    return(list(status = "PROVISIONAL_NOT_LOCKED", table = NULL, details = "RoB input is not marked FINAL_LOCKED"))
  }
  candidate <- NULL
  for (table in tables) {
    rating <- p3_find_column(table, c("overall_risk", "risk_of_bias", "rob_rating"), FALSE)
    id <- p3_find_column(table, c("effect_id", "study_id", "article_id", "report_id"), FALSE)
    if (!is.na(rating) && !is.na(id)) {
      candidate <- table
      break
    }
  }
  if (is.null(candidate)) {
    stop("FINAL_LOCKED RoB input lacks an ID and overall risk rating table.", call. = FALSE)
  }
  list(status = "FINAL_LOCKED", table = candidate, details = "Final locked RoB table supplied")
}

p3_rob_skip_row <- function(status, details) {
  data.frame(
    analysis_block = "ROB_HIGH_EXCLUSION",
    analysis_role = "SENSITIVITY",
    actual_k = 0L,
    independent_clusters = 0L,
    reason = if (identical(status, "FINAL_LOCKED")) "ROB_SENSITIVITY_READY" else "ROB_SENSITIVITY_SKIPPED_PROVISIONAL",
    error_message = details,
    critical = FALSE,
    stringsAsFactors = FALSE
  )
}

p3_attach_rob <- function(effects, rob_table) {
  rating_col <- p3_find_column(
    rob_table,
    c("overall_risk", "risk_of_bias", "rob_rating"),
    TRUE,
    "final locked RoB table"
  )
  id_options <- c("effect_id", "study_id", "article_id", "report_id")
  id_col <- p3_find_column(rob_table, id_options, TRUE, "final locked RoB table")
  effect_field <- tolower(id_col)
  if (identical(effect_field, "report_id")) effect_field <- "article_id"
  if (!effect_field %in% names(effects)) {
    stop("Cannot map final locked RoB IDs to standardized effects: ", id_col, call. = FALSE)
  }
  map_id <- p3_text(rob_table[[id_col]])
  map_rating <- p3_text(rob_table[[rating_col]])
  if (anyDuplicated(map_id[nzchar(map_id)])) {
    stop("Final locked RoB table has duplicate IDs for ", id_col, call. = FALSE)
  }
  effects$rob_rating <- map_rating[match(effects[[effect_field]], map_id)]
  effects
}

p3_run_final_rob_sensitivity <- function(blocks, effects, rob_table, tables) {
  effects_with_rob <- p3_attach_rob(effects, rob_table)
  results <- list()
  skips <- list()
  models <- list()
  for (model_id in names(blocks)) {
    block <- blocks[[model_id]]
    role <- toupper(block$spec$analysis_role[[1]])
    if (!grepl("PRIMARY", role) || nrow(block$data) == 0L) next
    block_key <- paste(block$data$source_sheet, block$data$effect_id, sep = "::")
    effects_key <- paste(effects_with_rob$source_sheet, effects_with_rob$effect_id, sep = "::")
    matched <- effects_with_rob[match(block_key, effects_key), , drop = FALSE]
    if (any(is.na(matched$rob_rating) | !nzchar(matched$rob_rating))) {
      skips[[length(skips) + 1L]] <- p3_skip_row(
        block$spec,
        block$data,
        "ROB_SENSITIVITY_NOT_RUN_INCOMPLETE_FINAL_MAPPING",
        paste("Missing final RoB rating for:", paste(matched$effect_id[!nzchar(matched$rob_rating)], collapse = ", "))
      )
      next
    }
    high <- grepl("^HIGH|HIGH RISK", toupper(matched$rob_rating))
    if (!any(high)) {
      skips[[length(skips) + 1L]] <- p3_skip_row(
        block$spec,
        block$data,
        "ROB_SENSITIVITY_NOT_RUN_NO_HIGH_RISK",
        "No High-risk effect in this primary block"
      )
      next
    }
    restricted <- block$data[!high, , drop = FALSE]
    if (nrow(restricted) == 0L) {
      skips[[length(skips) + 1L]] <- p3_skip_row(
        block$spec,
        restricted,
        "ROB_SENSITIVITY_NOT_RUN_EMPTY",
        "All effects were High risk"
      )
      next
    }
    restricted_block <- list(spec = block$spec, data = restricted)
    fitted <- tryCatch(
      p3_fit_block(restricted_block, tables),
      error = function(error) error
    )
    if (inherits(fitted, "error")) {
      skips[[length(skips) + 1L]] <- p3_skip_row(
        block$spec,
        restricted,
        "ROB_SENSITIVITY_MODEL_FAILED",
        conditionMessage(fitted)
      )
      next
    }
    fitted$result$analysis_block <- paste0(model_id, "__ROB_EXCLUDE_HIGH")
    fitted$result$analysis_role <- "SENSITIVITY_ROB_FINAL_LOCKED"
    fitted$result$sensitivity_replacement_rule <- "Exclude final-locked High-risk records"
    results[[length(results) + 1L]] <- fitted$result
    models[[paste0(model_id, "__ROB_EXCLUDE_HIGH")]] <- fitted$fit
  }
  list(
    results = if (length(results) > 0L) do.call(rbind, results) else p3_result_template(),
    skips = if (length(skips) > 0L) do.call(rbind, skips) else p3_skip_template(),
    models = models
  )
}

p3_bind_or_template <- function(items, template) {
  if (length(items) == 0L) return(template)
  out <- do.call(rbind, items)
  rownames(out) <- NULL
  out
}

p3_write_csv <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

p3_write_analysis_summary <- function(
  path,
  qc,
  manifest_output,
  primary,
  sensitivity,
  singletons,
  skipped,
  rob_status,
  input_hash
) {
  lines <- c(
    paste0("# ", P3_ANALYSIS_VERSION, " run summary"),
    "",
    paste0("- Input version: `", P3_INPUT_VERSION, "`"),
    paste0("- Input SHA-256: `", input_hash, "`"),
    paste0("- QC: ", sum(qc$status == "PASS"), "/", nrow(qc), " PASS"),
    paste0("- Manifest blocks: ", nrow(manifest_output)),
    paste0("- Primary estimate rows: ", nrow(primary)),
    paste0("- Sensitivity estimate rows: ", nrow(sensitivity)),
    paste0("- Descriptive singleton rows: ", nrow(singletons)),
    paste0("- Skipped/failed rows: ", nrow(skipped)),
    paste0("- Risk-of-bias status: `", rob_status, "`"),
    "",
    "No input effect, direction, inclusion decision, sample mapping, or workbook cell was modified.",
    "P3 does not define a single pooled effect across incompatible effect scales or estimands."
  )
  writeLines(lines, path, useBytes = TRUE)
}

p3_run_analysis <- function(
  args = commandArgs(trailingOnly = TRUE),
  project_root = p3_locate_project_root(),
  script_dir = if (!is.na(project_root)) file.path(project_root, "analysis") else getwd()
) {
  p3_require_packages()
  paths <- p3_resolve_paths(args, project_root, script_dir)
  if (dir.exists(paths$run_dir)) {
    stop("Output run directory already exists: ", paths$run_dir, call. = FALSE)
  }
  dir.create(paths$run_dir, recursive = TRUE, showWarnings = FALSE)
  table_dir <- file.path(paths$run_dir, "tables")
  figure_dir <- file.path(paths$run_dir, "figures")
  model_dir <- file.path(paths$run_dir, "models")
  log_dir <- file.path(paths$run_dir, "logs")
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

  hash_before <- p3_sha256(paths$input_path)
  tables <- p3_read_workbook(paths$input_path)
  hash_after_read <- p3_sha256(paths$input_path)
  effects <- p3_standardize_effects(tables)
  manifest <- p3_combine_manifests(tables)
  qc <- p3_validate_input_qc(tables, effects, manifest)
  qc <- rbind(
    qc,
    if (identical(hash_before, hash_after_read)) {
      p3_qc_pass("Input hash after read", "SHA-256 unchanged")
    } else {
      p3_qc_fail("Input hash after read", "Input workbook changed while being read")
    }
  )
  p3_write_csv(qc, file.path(paths$run_dir, "qc_report.csv"))
  p3_write_csv(
    data.frame(
      stage = c("before_read", "after_read"),
      input_filename = basename(paths$input_path),
      sha256 = c(hash_before, hash_after_read),
      stringsAsFactors = FALSE
    ),
    file.path(paths$run_dir, "input_hashes.csv")
  )

  if (p3_qc_has_critical_failure(qc)) {
    capture.output(sessionInfo(), file = file.path(paths$run_dir, "sessionInfo.txt"))
    p3_write_analysis_summary(
      file.path(paths$run_dir, "analysis_summary.md"),
      qc,
      data.frame(),
      p3_result_template(),
      p3_result_template(),
      p3_result_template(),
      p3_skip_template(),
      "NOT_EVALUATED_QC_FAILURE",
      hash_before
    )
    stop(
      "Critical P3 input QC failed. No models were fitted. See ",
      file.path(paths$run_dir, "qc_report.csv"),
      call. = FALSE
    )
  }

  blocks <- p3_build_analysis_blocks(effects, manifest)
  primary_rows <- list()
  sensitivity_rows <- list()
  singleton_rows <- list()
  skipped_rows <- list()
  dependency_rows <- list()
  manifest_rows <- list()
  diagnostic_rows <- list()
  model_objects <- list()

  for (model_id in names(blocks)) {
    block <- blocks[[model_id]]
    spec <- block$spec
    d <- block$data
    is_a008_block <- identical(spec$known_v_sheet[[1]], "V_Matrix_A008") ||
      grepl("A008", paste(d$row_text, collapse = " | "), ignore.case = TRUE)
    if (nrow(d) == 0L) {
      failed_row <- p3_skip_row(spec, d, "NO_INCLUDED_EFFECTS")
      if (is_a008_block) failed_row$critical <- TRUE
      skipped_rows[[length(skipped_rows) + 1L]] <- failed_row
      manifest_rows[[length(manifest_rows) + 1L]] <- p3_manifest_output_row(spec, d, "SKIPPED")
      dependency_rows[[length(dependency_rows) + 1L]] <- p3_dependency_row(model_id, d, "NOT_EVALUATED", "No included effects")
      next
    }

    fitted <- tryCatch(p3_fit_block(block, tables), error = function(error) error)
    if (inherits(fitted, "error")) {
      failed_row <- p3_skip_row(
        spec,
        d,
        "MODEL_FAILED_OR_INCOMPATIBLE",
        conditionMessage(fitted)
      )
      if (is_a008_block) failed_row$critical <- TRUE
      skipped_rows[[length(skipped_rows) + 1L]] <- failed_row
      manifest_rows[[length(manifest_rows) + 1L]] <- p3_manifest_output_row(spec, d, "FAILED")
      dependency_rows[[length(dependency_rows) + 1L]] <- p3_dependency_row(
        model_id,
        d,
        "MODEL_NOT_RUN",
        conditionMessage(fitted)
      )
      next
    }

    result <- fitted$result
    fit_data <- fitted$analysis_data
    if (identical(fitted$model_kind, "singleton")) {
      singleton_rows[[length(singleton_rows) + 1L]] <- result
    } else if (grepl("SENSIT|ROBUST", toupper(result$analysis_role[[1]]))) {
      sensitivity_rows[[length(sensitivity_rows) + 1L]] <- result
    } else {
      primary_rows[[length(primary_rows) + 1L]] <- result
    }
    model_objects[[model_id]] <- fitted$fit
    manifest_rows[[length(manifest_rows) + 1L]] <- p3_manifest_output_row(spec, d, "RUN")
    dependency_rows[[length(dependency_rows) + 1L]] <- p3_dependency_row(
      model_id,
      fit_data,
      result$cr2_status[[1]],
      if (!is.null(fitted$V)) "Explicit sampling V used" else "Independent-effect block"
    )

    plot_error <- tryCatch(
      {
        p3_write_forest(fitted, fit_data, figure_dir, model_id)
        NULL
      },
      error = function(error) error
    )
    if (inherits(plot_error, "error")) {
      skipped_rows[[length(skipped_rows) + 1L]] <- p3_skip_row(
        transform(spec, critical = "No"),
        d,
        "FOREST_PLOT_FAILED",
        conditionMessage(plot_error)
      )
    }

    diagnostic_error <- tryCatch(
      p3_run_diagnostics(fitted, fit_data, model_id, table_dir),
      error = function(error) error
    )
    if (inherits(diagnostic_error, "error")) {
      skipped_rows[[length(skipped_rows) + 1L]] <- p3_skip_row(
        transform(spec, critical = "No"),
        d,
        "DIAGNOSTIC_FAILED",
        conditionMessage(diagnostic_error)
      )
    } else {
      diagnostic_rows[[length(diagnostic_rows) + 1L]] <- diagnostic_error
    }

    if (is_a008_block) {
      if (is.null(fitted$V) || !p3_validate_a008_order(fit_data, fitted$V) ||
          p3_cluster_counts(fit_data)$independent_clusters != 2L ||
          !identical(result$cr2_status[[1]], "SKIPPED_LT4_CLUSTERS")) {
        failed_row <- p3_skip_row(
          spec,
          d,
          "A008_V_ORDER_VALIDATION_FAILED",
          "V matrix rows and columns must exactly match the five locked effect IDs"
        )
        failed_row$critical <- TRUE
        skipped_rows[[length(skipped_rows) + 1L]] <- failed_row
      }
    }
  }

  s016 <- tryCatch(p3_fit_s016_joint(tables$Main_Nonlinear_v3_1), error = function(error) error)
  if (inherits(s016, "error")) {
    skipped_rows[[length(skipped_rows) + 1L]] <- data.frame(
      analysis_block = "S016_JOINT_LQ",
      analysis_role = "ANCILLARY_JOINT_MODEL",
      actual_k = 0L,
      independent_clusters = 0L,
      reason = "S016_JOINT_NOT_IDENTIFIABLE",
      error_message = conditionMessage(s016),
      critical = FALSE,
      stringsAsFactors = FALSE
    )
  } else {
    sensitivity_rows[[length(sensitivity_rows) + 1L]] <- s016$result
    model_objects$S016_JOINT_LQ <- s016$fit
    dependency_rows[[length(dependency_rows) + 1L]] <- data.frame(
      analysis_block = "S016_JOINT_LQ",
      actual_k = 8L,
      independent_clusters = 4L,
      repeated_effect_clusters = 4L,
      missing_cluster = FALSE,
      cr2_status = "CR2_SATTERTHWAITE",
      details = "Four S016 experiments; recorded within-experiment linear-quadratic covariance",
      stringsAsFactors = FALSE
    )
  }

  rob <- p3_read_rob_status(paths$rob_input)
  if (identical(rob$status, "FINAL_LOCKED")) {
    rob_run <- p3_run_final_rob_sensitivity(blocks, effects, rob$table, tables)
    if (nrow(rob_run$results) > 0L) sensitivity_rows[[length(sensitivity_rows) + 1L]] <- rob_run$results
    if (nrow(rob_run$skips) > 0L) skipped_rows[[length(skipped_rows) + 1L]] <- rob_run$skips
    model_objects <- c(model_objects, rob_run$models)
  } else {
    skipped_rows[[length(skipped_rows) + 1L]] <- p3_rob_skip_row(rob$status, rob$details)
  }

  primary <- p3_bind_or_template(primary_rows, p3_result_template())
  sensitivity <- p3_bind_or_template(sensitivity_rows, p3_result_template())
  singletons <- p3_bind_or_template(singleton_rows, p3_result_template())
  skipped <- p3_bind_or_template(skipped_rows, p3_skip_template())
  dependency <- p3_bind_or_template(
    dependency_rows,
    data.frame(
      analysis_block = character(), actual_k = integer(), independent_clusters = integer(),
      repeated_effect_clusters = integer(), missing_cluster = logical(), cr2_status = character(),
      details = character(), stringsAsFactors = FALSE
    )
  )
  manifest_output <- p3_bind_or_template(
    manifest_rows,
    data.frame(
      analysis_block = character(), manifest_version = character(), analysis_role = character(),
      effect_scale = character(), estimand = character(), pe_encoding = character(), outcome = character(),
      input_sheet = character(), actual_k = integer(), independent_clusters = integer(),
      sensitivity_replacement_rule = character(), status = character(), stringsAsFactors = FALSE
    )
  )
  diagnostics <- p3_bind_or_template(
    diagnostic_rows,
    data.frame(
      analysis_block = character(), diagnostic = character(), status = character(),
      details = character(), stringsAsFactors = FALSE
    )
  )

  hash_after_run <- p3_sha256(paths$input_path)
  hash_ok <- identical(hash_before, hash_after_read) && identical(hash_before, hash_after_run)
  qc <- rbind(
    qc,
    if (hash_ok) {
      p3_qc_pass("Input hash after analysis", "SHA-256 unchanged before read, after read, and after analysis")
    } else {
      p3_qc_fail("Input hash after analysis", "Input workbook hash changed during the run")
    }
  )
  p3_write_csv(qc, file.path(paths$run_dir, "qc_report.csv"))
  p3_write_csv(
    data.frame(
      stage = c("before_read", "after_read", "after_analysis"),
      input_filename = basename(paths$input_path),
      sha256 = c(hash_before, hash_after_read, hash_after_run),
      stringsAsFactors = FALSE
    ),
    file.path(paths$run_dir, "input_hashes.csv")
  )
  p3_write_csv(manifest_output, file.path(paths$run_dir, "model_manifest.csv"))
  p3_write_csv(primary, file.path(paths$run_dir, "primary_estimates.csv"))
  p3_write_csv(sensitivity, file.path(paths$run_dir, "sensitivity_estimates.csv"))
  p3_write_csv(singletons, file.path(paths$run_dir, "descriptive_singletons.csv"))
  p3_write_csv(skipped, file.path(paths$run_dir, "skipped_models.csv"))
  p3_write_csv(dependency, file.path(paths$run_dir, "dependency_checks.csv"))
  p3_write_csv(diagnostics, file.path(paths$run_dir, "diagnostic_checks.csv"))
  saveRDS(model_objects, file.path(model_dir, "model_objects.rds"))
  capture.output(sessionInfo(), file = file.path(paths$run_dir, "sessionInfo.txt"))
  p3_write_analysis_summary(
    file.path(paths$run_dir, "analysis_summary.md"),
    qc,
    manifest_output,
    primary,
    sensitivity,
    singletons,
    skipped,
    rob$status,
    hash_before
  )

  critical_model_failure <- nrow(skipped) > 0L && any(skipped$critical)
  if (!hash_ok || p3_qc_has_critical_failure(qc) || critical_model_failure) {
    stop(
      "P3 run completed with a critical failure. Review qc_report.csv and skipped_models.csv in ",
      paths$run_dir,
      call. = FALSE
    )
  }

  message("P3 analysis complete: ", paths$run_dir)
  message("Input SHA-256 remained unchanged: ", hash_after_run)
  invisible(list(
    run_dir = paths$run_dir,
    qc = qc,
    primary = primary,
    sensitivity = sensitivity,
    singletons = singletons,
    skipped = skipped
  ))
}

# Purely synthetic smoke test ----------------------------------------------

p3_synthetic_spec <- function(
  model_id,
  role = "PRIMARY",
  scale = "logOR",
  estimand = "synthetic estimand",
  known_v_sheet = ""
) {
  data.frame(
    model_id = model_id,
    run_model = "Yes",
    analysis_role = role,
    source_sheet = "synthetic",
    effect_scale = scale,
    estimand = estimand,
    pe_encoding = "synthetic coding",
    outcome = "synthetic memory",
    model_type = "",
    base_model_id = "",
    sensitivity_rule = "",
    known_v_sheet = known_v_sheet,
    allow_mixed_outcome = "No",
    critical = "Yes",
    manifest_version = "synthetic",
    stringsAsFactors = FALSE
  )
}

p3_synthetic_effects <- function(ids, clusters, model_id, yi, vi) {
  n <- length(ids)
  data.frame(
    effect_id = ids,
    article_id = paste0("R", seq_len(n)),
    study_id = paste0("SYN", seq_len(n)),
    sample_id = clusters,
    author = "Synthetic",
    year = 2026,
    outcome = "synthetic memory",
    contrast = "synthetic contrast",
    effect_scale = "logOR",
    estimand = "synthetic estimand",
    pe_encoding = "synthetic coding",
    yi = yi,
    sei = sqrt(vi),
    vi = vi,
    model_id = model_id,
    base_model_id = "",
    analysis_role = "PRIMARY",
    analysis_include = "Yes",
    decision_status = "LOCKED_SYNTHETIC",
    dependency_cluster = clusters,
    shared_control_block = "",
    replacement_for = "",
    sensitivity_rule = "",
    paradigm = "",
    experiment = "",
    source_sheet = "synthetic",
    notes = "Synthetic smoke-test data only",
    row_text = paste("synthetic", ids),
    stringsAsFactors = FALSE
  )
}

p3_full_covariance_long <- function(ids, V, model_id = "") {
  rows <- vector("list", length(ids)^2)
  index <- 0L
  for (i in seq_along(ids)) {
    for (j in seq_along(ids)) {
      index <- index + 1L
      rows[[index]] <- data.frame(
        row_effect_id = ids[[i]],
        col_effect_id = ids[[j]],
        covariance = V[i, j],
        model_id = model_id,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

p3_smoke_test <- function() {
  p3_require_packages()

  independent <- p3_synthetic_effects(
    paste0("I", 1:4),
    paste0("C", 1:4),
    "SYN_INDEPENDENT",
    c(0.10, 0.18, -0.02, 0.12),
    c(0.04, 0.05, 0.03, 0.04)
  )
  independent_fit <- p3_fit_block(
    list(spec = p3_synthetic_spec("SYN_INDEPENDENT"), data = independent),
    list()
  )
  stopifnot(
    identical(independent_fit$model_kind, "rma_uni"),
    identical(independent_fit$result$hk_status[[1]], "Hartung-Knapp"),
    identical(independent_fit$result$cr2_status[[1]], "NOT_APPLICABLE")
  )

  dependent <- p3_synthetic_effects(
    paste0("D", 1:8),
    rep(paste0("K", 1:4), each = 2L),
    "SYN_DEPENDENT",
    c(0.10, 0.16, -0.04, 0.03, 0.20, 0.14, -0.08, 0.01),
    rep(0.04, 8L)
  )
  V_dep <- diag(dependent$vi)
  for (i in seq(1L, 8L, by = 2L)) V_dep[i, i + 1L] <- V_dep[i + 1L, i] <- 0.01
  dep_cov <- p3_full_covariance_long(dependent$effect_id, V_dep, "SYN_DEPENDENT")
  dependent_fit <- p3_fit_block(
    list(spec = p3_synthetic_spec("SYN_DEPENDENT"), data = dependent),
    list(Precision_Covariance_v3_1 = dep_cov)
  )
  stopifnot(
    identical(dependent_fit$model_kind, "rma_mv"),
    identical(dependent_fit$result$cr2_status[[1]], "CR2_SATTERTHWAITE"),
    identical(dependent_fit$result$independent_clusters[[1]], 4L)
  )

  a008 <- p3_synthetic_effects(
    paste0("A008_E", 1:5),
    c("A008_C1", "A008_C1", "A008_C1", "A008_C2", "A008_C2"),
    "SYN_A008",
    c(0.10, 0.14, 0.08, -0.03, 0.02),
    rep(0.04, 5L)
  )
  a008$row_text <- paste("A008 synthetic", a008$effect_id)
  a008$shared_control_block <- a008$dependency_cluster
  V_a008 <- diag(a008$vi)
  V_a008[1:3, 1:3] <- 0.008
  diag(V_a008)[1:3] <- 0.04
  V_a008[4:5, 4:5] <- 0.008
  diag(V_a008)[4:5] <- 0.04
  a008_fit <- p3_fit_block(
    list(
      spec = p3_synthetic_spec("SYN_A008", known_v_sheet = "V_Matrix_A008"),
      data = a008
    ),
    list(V_Matrix_A008 = p3_full_covariance_long(a008$effect_id, V_a008, "SYN_A008"))
  )
  stopifnot(
    identical(a008_fit$model_kind, "rma_mv"),
    identical(a008_fit$result$cr2_status[[1]], "SKIPPED_LT4_CLUSTERS"),
    p3_validate_a008_order(a008, a008_fit$V)
  )

  replacements <- independent[1, , drop = FALSE]
  replacements$effect_id <- "I1_ALT"
  replacements$replacement_for <- "I1"
  replacements$analysis_role <- "SENSITIVITY"
  replacement_set <- p3_build_replacement_dataset(independent, replacements)
  stopifnot(
    nrow(replacement_set) == nrow(independent),
    length(unique(replacement_set$dependency_cluster)) == length(unique(independent$dependency_cluster)),
    !"I1" %in% replacement_set$effect_id,
    "I1_ALT" %in% replacement_set$effect_id
  )

  incompatible <- independent
  incompatible$effect_scale[[2]] <- "SMD"
  stopifnot(!p3_check_compatibility(incompatible, p3_synthetic_spec("SYN_INDEPENDENT"))$ok)

  message("P3 synthetic smoke test passed.")
  invisible(TRUE)
}
