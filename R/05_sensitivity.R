pem_rho_sensitivity <- function(data, config = pem_analysis_config(),
                                min_samples = config$min_quantitative_samples) {
  split_data <- split(data, data$pooling_block, drop = TRUE)
  output <- list()

  for (block in names(split_data)) {
    block_data <- split_data[[block]]
    multiple_effects <- any(table(block_data$sample_id) > 1L)

    if (!multiple_effects) {
      output[[block]] <- data.frame(
        stream = unique(block_data$stream),
        pooling_block = block,
        rho = config$rho_grid,
        status = "not_applicable_single_effect_per_sample",
        model_type = "rma.uni",
        inference_method = "Hartung-Knapp",
        estimate = NA_real_,
        se = NA_real_,
        ci_lb = NA_real_,
        ci_ub = NA_real_,
        p = NA_real_,
        note = paste(
          "The frozen block contains one effect per Sample_ID; changing rho",
          "cannot change its diagonal sampling covariance matrix."
        ),
        stringsAsFactors = FALSE
      )
      next
    }

    rows <- lapply(config$rho_grid, function(rho) {
      fit <- pem_fit_block(
        block_data,
        rho = rho,
        min_samples = min_samples,
        min_cr2_clusters = config$min_cr2_clusters
      )
      row <- pem_model_summary_row(fit)
      data.frame(
        stream = row$stream,
        pooling_block = row$pooling_block,
        rho = rho,
        status = row$status,
        model_type = row$model_type,
        inference_method = row$inference_method,
        estimate = row$primary_estimate,
        se = row$primary_se,
        ci_lb = row$primary_ci_lb,
        ci_ub = row$primary_ci_ub,
        p = row$primary_p,
        note = row$notes,
        stringsAsFactors = FALSE
      )
    })
    output[[block]] <- dplyr::bind_rows(rows)
  }

  dplyr::bind_rows(output)
}

pem_leave_one_unit_out <- function(data, unit = c("sample_id", "report_id"),
                                   config = pem_analysis_config(),
                                   min_samples = config$min_quantitative_samples) {
  unit <- match.arg(unit)
  split_data <- split(data, data$pooling_block, drop = TRUE)
  output <- list()

  for (block in names(split_data)) {
    block_data <- split_data[[block]]
    units <- unique(as.character(block_data[[unit]]))

    rows <- lapply(units, function(omitted) {
      keep <- as.character(block_data[[unit]]) != omitted
      reduced <- block_data[keep, , drop = FALSE]

      if (nrow(reduced) == 0L) {
        return(data.frame(
          stream = unique(block_data$stream),
          pooling_block = block,
          omitted_unit = unit,
          omitted_id = omitted,
          status = "no_data_remaining",
          n_samples_remaining = 0L,
          estimate = NA_real_,
          ci_lb = NA_real_,
          ci_ub = NA_real_,
          note = "No effects remain after omission.",
          stringsAsFactors = FALSE
        ))
      }

      fit <- pem_fit_block(
        reduced,
        rho = config$primary_rho,
        min_samples = min_samples,
        min_cr2_clusters = config$min_cr2_clusters
      )
      row <- pem_model_summary_row(fit)

      data.frame(
        stream = row$stream,
        pooling_block = row$pooling_block,
        omitted_unit = unit,
        omitted_id = omitted,
        status = row$status,
        n_samples_remaining = row$n_samples,
        model_type = row$model_type,
        inference_method = row$inference_method,
        estimate = row$primary_estimate,
        ci_lb = row$primary_ci_lb,
        ci_ub = row$primary_ci_ub,
        note = row$notes,
        stringsAsFactors = FALSE
      )
    })

    output[[block]] <- dplyr::bind_rows(rows)
  }

  dplyr::bind_rows(output)
}

pem_logor_to_hedges_g <- function(log_or, vi_log_or, df) {
  if (any(!is.finite(df)) || any(df <= 0)) {
    pem_abort(
      "A defensible effective df is required before applying a Hedges correction."
    )
  }

  scale <- sqrt(3) / pi
  correction <- 1 - 3 / (4 * df - 1)

  data.frame(
    g = correction * scale * log_or,
    vi_g = (correction * scale)^2 * vi_log_or,
    hedges_correction = correction,
    stringsAsFactors = FALSE
  )
}

pem_equivalence_classification <- function(ci_lb, ci_ub, metric,
                                           config = pem_analysis_config()) {
  if (!identical(metric, "Hedges_g")) {
    return("not_evaluated_outside_defensible_g_scale")
  }

  bounds <- config$equivalence_bounds_g
  if (!is.finite(ci_lb) || !is.finite(ci_ub)) return("insufficient_interval")

  if (ci_lb >= bounds[[1]] && ci_ub <= bounds[[2]]) {
    "compatible_with_practically_trivial_interval"
  } else {
    "interval_not_fully_inside_trivial_range"
  }
}

pem_canonical_risk <- function(x) {
  value <- tolower(trimws(as.character(x)))
  out <- rep(NA_character_, length(value))
  out[grepl("^low( risk)?$", value)] <- "low"
  out[grepl("^some concerns$", value)] <- "some_concerns"
  out[grepl("^high( risk)?$", value)] <- "high"
  out[grepl("^not applicable$|^n/a$", value)] <- "not_applicable"
  out
}

pem_risk_of_bias_sensitivity <- function(
    data,
    config = pem_analysis_config(),
    min_samples = config$min_quantitative_samples) {
  split_data <- split(data, data$pooling_block, drop = TRUE)
  output <- list()

  for (block in names(split_data)) {
    block_data <- split_data[[block]] |>
      dplyr::mutate(risk_canonical = pem_canonical_risk(.data$overall_risk))

    unresolved <- is.na(block_data$risk_canonical)
    if (any(unresolved)) {
      output[[block]] <- data.frame(
        stream = unique(block_data$stream),
        pooling_block = block,
        status = "not_run_incomplete_or_nonfinal_risk_ratings",
        n_samples_original = dplyr::n_distinct(block_data$sample_id),
        n_samples_included = NA_integer_,
        n_high_risk_excluded = NA_integer_,
        estimate = NA_real_,
        ci_lb = NA_real_,
        ci_ub = NA_real_,
        note = paste0(
          "Unresolved Overall_Risk values: ",
          paste(unique(block_data$overall_risk[unresolved]), collapse = "; "),
          ". Finalize ratings before this sensitivity analysis."
        ),
        stringsAsFactors = FALSE
      )
      next
    }

    keep <- block_data$risk_canonical %in% c("low", "some_concerns")
    reduced <- block_data[keep, , drop = FALSE]
    n_high <- dplyr::n_distinct(
      as.character(block_data$sample_id[block_data$risk_canonical == "high"])
    )

    if (nrow(reduced) == 0L) {
      output[[block]] <- data.frame(
        stream = unique(block_data$stream),
        pooling_block = block,
        status = "no_low_or_some_concerns_effects",
        n_samples_original = dplyr::n_distinct(block_data$sample_id),
        n_samples_included = 0L,
        n_high_risk_excluded = n_high,
        estimate = NA_real_,
        ci_lb = NA_real_,
        ci_ub = NA_real_,
        note = "No effects remained after the prespecified risk restriction.",
        stringsAsFactors = FALSE
      )
      next
    }

    fit <- pem_fit_block(
      reduced,
      rho = config$primary_rho,
      min_samples = min_samples,
      min_cr2_clusters = config$min_cr2_clusters
    )
    row <- pem_model_summary_row(fit)

    output[[block]] <- data.frame(
      stream = row$stream,
      pooling_block = row$pooling_block,
      status = row$status,
      n_samples_original = dplyr::n_distinct(block_data$sample_id),
      n_samples_included = row$n_samples,
      n_high_risk_excluded = n_high,
      model_type = row$model_type,
      inference_method = row$inference_method,
      estimate = row$primary_estimate,
      ci_lb = row$primary_ci_lb,
      ci_ub = row$primary_ci_ub,
      note = row$notes,
      stringsAsFactors = FALSE
    )
  }

  dplyr::bind_rows(output)
}

pem_fit_moderator <- function(...) {
  config <- pem_analysis_config()
  if (!config$moderator_thresholds_frozen) {
    pem_abort(paste(
      "Moderator fitting is intentionally disabled. Freeze numeric minimum-sample",
      "rules in the protocol or a dated amendment before enabling this function."
    ))
  }
  pem_abort("Moderator thresholds are marked frozen but no implementation is configured.")
}
