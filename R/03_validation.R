pem_issue <- function(severity, check, stream = "all", detail) {
  data.frame(
    severity = severity,
    check = check,
    stream = stream,
    detail = detail,
    stringsAsFactors = FALSE
  )
}

pem_append_issue <- function(issues, severity, check, stream = "all", detail) {
  dplyr::bind_rows(issues, pem_issue(severity, check, stream, detail))
}

pem_check_se_variance <- function(data, stream, tolerance = 1e-8) {
  valid <- is.finite(data$sei_native) & is.finite(data$vi_native)
  discrepancy <- abs(data$vi_native - data$sei_native^2)
  bad <- valid & discrepancy > tolerance

  if (!any(bad)) return(data.frame())

  pem_issue(
    "error",
    "vi_equals_se_squared",
    stream,
    paste0(
      sum(bad), " row(s) have vi inconsistent with SE^2 beyond tolerance ",
      format(tolerance, scientific = TRUE), ". IDs: ",
      paste(data$analysis_id[bad], collapse = ", ")
    )
  )
}

pem_audit_inputs <- function(raw, prepared, config = pem_analysis_config(),
                             strict_freeze = TRUE) {
  issues <- data.frame()

  raw_counts <- c(
    logor = nrow(raw$logor),
    smd = nrow(raw$smd),
    nonlinear = nrow(raw$nonlinear)
  )

  if (strict_freeze) {
    for (stream in names(config$expected_counts)) {
      observed <- unname(raw_counts[[stream]])
      expected <- unname(config$expected_counts[[stream]])
      if (!identical(as.integer(observed), as.integer(expected))) {
        issues <- pem_append_issue(
          issues,
          "error",
          "frozen_stream_count",
          stream,
          paste0("Expected ", expected, " rows; observed ", observed, ".")
        )
      }
    }
  }

  raw_minimal <- dplyr::bind_rows(
    dplyr::transmute(
      raw$logor,
      stream = "logOR",
      analysis_id = as.character(.data$analysis_id),
      freeze_id = as.character(.data$freeze_id),
      sample_id = as.character(.data$sample_id)
    ),
    dplyr::transmute(
      raw$smd,
      stream = "SMD",
      analysis_id = as.character(.data$analysis_id),
      freeze_id = as.character(.data$freeze_id),
      sample_id = as.character(.data$sample_id)
    ),
    dplyr::transmute(
      raw$nonlinear,
      stream = "nonlinear",
      analysis_id = as.character(.data$analysis_id),
      freeze_id = as.character(.data$freeze_id),
      sample_id = as.character(.data$sample_id)
    )
  )

  if (strict_freeze && nrow(raw_minimal) != config$expected_total) {
    issues <- pem_append_issue(
      issues,
      "error",
      "frozen_total_count",
      detail = paste0(
        "Expected ", config$expected_total,
        " strict primary effects; observed ", nrow(raw_minimal), "."
      )
    )
  }

  duplicated_freeze <- unique(raw_minimal$freeze_id[duplicated(raw_minimal$freeze_id)])
  if (length(duplicated_freeze) > 0L) {
    issues <- pem_append_issue(
      issues,
      "error",
      "unique_freeze_id",
      detail = paste("Duplicated freeze_id:", paste(duplicated_freeze, collapse = ", "))
    )
  }

  duplicated_analysis <- raw_minimal |>
    dplyr::count(.data$stream, .data$analysis_id, name = "n") |>
    dplyr::filter(.data$n > 1L)
  if (nrow(duplicated_analysis) > 0L) {
    issues <- pem_append_issue(
      issues,
      "error",
      "unique_analysis_id",
      detail = paste0(nrow(duplicated_analysis), " duplicated stream/analysis_id pair(s).")
    )
  }

  duplicated_sample <- raw_minimal |>
    dplyr::count(.data$sample_id, name = "n") |>
    dplyr::filter(.data$n > 1L)
  if (nrow(duplicated_sample) > 0L) {
    issues <- pem_append_issue(
      issues,
      if (strict_freeze) "error" else "warning",
      "one_primary_effect_per_sample",
      detail = paste0(
        "Multiple frozen primary rows found for sample_id: ",
        paste(duplicated_sample$sample_id, collapse = ", "), "."
      )
    )
  }

  prepared_all <- dplyr::bind_rows(prepared)

  if (nrow(prepared_all) != nrow(raw_minimal)) {
    issues <- pem_append_issue(
      issues,
      "error",
      "join_row_preservation",
      detail = paste0(
        "Preparing/joining inputs changed the row count from ",
        nrow(raw_minimal), " to ", nrow(prepared_all), "."
      )
    )
  }

  missing_key <- !stats::complete.cases(
    prepared_all[, c(
      "analysis_id", "freeze_id", "study_id", "sample_id", "report_id",
      "effect_id_analysis", "pooling_block"
    )]
  )
  if (any(missing_key)) {
    issues <- pem_append_issue(
      issues,
      "error",
      "complete_identifiers",
      detail = paste0(
        sum(missing_key), " row(s) have a missing analysis, sample, report, effect, ",
        "or pooling-block identifier."
      )
    )
  }

  bad_numeric <- !is.finite(prepared_all$yi_analysis) |
    !is.finite(prepared_all$vi_analysis) |
    prepared_all$vi_analysis <= 0
  if (any(bad_numeric)) {
    issues <- pem_append_issue(
      issues,
      "error",
      "finite_positive_effect_inputs",
      detail = paste0(
        sum(bad_numeric), " row(s) have non-finite yi/vi or non-positive vi. IDs: ",
        paste(prepared_all$analysis_id[bad_numeric], collapse = ", ")
      )
    )
  }

  issues <- dplyr::bind_rows(
    issues,
    pem_check_se_variance(prepared$logor, "logOR"),
    pem_check_se_variance(prepared$smd, "SMD"),
    pem_check_se_variance(prepared$nonlinear, "nonlinear")
  )

  endpoint_missing <- prepared$logor$endpoint_used &
    (!is.finite(prepared$logor$yi_analysis) | !is.finite(prepared$logor$vi_analysis))
  if (any(endpoint_missing)) {
    issues <- pem_append_issue(
      issues,
      "error",
      "endpoint_columns_complete",
      "logOR",
      paste0(
        "Endpoint pooling requested but endpoint yi/vi are missing for: ",
        paste(prepared$logor$analysis_id[endpoint_missing], collapse = ", ")
      )
    )
  }

  non_main <- !is.na(prepared_all$module) & prepared_all$module != "A_Main_Encoding"
  if (any(non_main)) {
    issues <- pem_append_issue(
      issues,
      "error",
      "primary_module_boundary",
      detail = paste0(
        "Strict primary input includes non-A_Main_Encoding rows: ",
        paste(prepared_all$analysis_id[non_main], collapse = ", ")
      )
    )
  }

  provisional_smd <- grepl("PROVISIONAL", prepared$smd$pooling_block, fixed = TRUE)
  if (any(provisional_smd)) {
    issues <- pem_append_issue(
      issues,
      "warning",
      "provisional_smd_harmonization",
      "SMD",
      paste0(
        "Provisional SMD denominator remains isolated: ",
        paste(prepared$smd$analysis_id[provisional_smd], collapse = ", "), "."
      )
    )
  }

  missing_risk <- is.na(prepared_all$overall_risk) |
    !nzchar(trimws(prepared_all$overall_risk))
  if (any(missing_risk)) {
    issues <- pem_append_issue(
      issues,
      "warning",
      "risk_of_bias_mapping",
      detail = paste0(
        sum(missing_risk),
        " primary row(s) lack a mapped Overall_Risk judgment; risk-restricted ",
        "sensitivity analysis will remain disabled for affected blocks."
      )
    )
  }

  if (!config$moderator_thresholds_frozen) {
    issues <- pem_append_issue(
      issues,
      "info",
      "moderator_threshold_guard",
      detail = paste(
        "Moderator fitting is disabled because numeric minimum-sample rules",
        "have not been frozen in the available protocol text."
      )
    )
  }

  issues <- pem_append_issue(
    issues,
    "info",
    "nonlinear_joint_model_guard",
    "nonlinear",
    paste(
      "Linear and quadratic terms are not jointly synthesized unless their",
      "sampling covariance is available."
    )
  )

  if (nrow(issues) == 0L) {
    issues <- pem_issue("info", "audit_complete", detail = "No audit issues detected.")
  }

  block_manifest <- prepared_all |>
    dplyr::group_by(.data$stream, .data$pooling_block) |>
    dplyr::summarise(
      n_effects = dplyr::n(),
      n_samples = dplyr::n_distinct(.data$sample_id),
      n_reports = dplyr::n_distinct(.data$report_id),
      .groups = "drop"
    )

  summary <- data.frame(
    item = c(
      "logor_rows", "smd_rows", "nonlinear_rows", "total_rows",
      "independent_samples", "reports", "fatal_errors", "warnings"
    ),
    value = c(
      raw_counts[["logor"]], raw_counts[["smd"]], raw_counts[["nonlinear"]],
      nrow(prepared_all), dplyr::n_distinct(prepared_all$sample_id),
      dplyr::n_distinct(prepared_all$report_id),
      sum(issues$severity == "error"), sum(issues$severity == "warning")
    ),
    stringsAsFactors = FALSE
  )

  list(
    passed = !any(issues$severity == "error"),
    issues = issues,
    summary = summary,
    block_manifest = block_manifest,
    analysis_manifest = prepared_all |>
      dplyr::select(
        .data$stream, .data$analysis_id, .data$freeze_id, .data$study_id,
        .data$sample_id, .data$report_id, .data$effect_id_analysis,
        .data$pooling_block, .data$yi_analysis, .data$vi_analysis,
        .data$endpoint_used, .data$module
      )
  )
}

pem_stop_on_audit_error <- function(audit) {
  if (isTRUE(audit$passed)) return(invisible(TRUE))

  fatal <- audit$issues[audit$issues$severity == "error", , drop = FALSE]
  pem_abort(paste0(
    "Input audit failed with ", nrow(fatal),
    " fatal issue(s). Inspect results/audit_issues.csv before modeling."
  ))
}
