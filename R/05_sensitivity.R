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
        estimate = NA_real_,
        se_cr2 = NA_real_,
        ci_lb_cr2 = NA_real_,
        ci_ub_cr2 = NA_real_,
        p_cr2 = NA_real_,
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
        min_cr2_samples = config$min_cr2_samples
      )
      row <- pem_model_summary_row(fit)
      data.frame(
        stream = row$stream,
        pooling_block = row$pooling_block,
        rho = rho,
        status = row$status,
        estimate = row$estimate,
        se_cr2 = row$se_cr2,
        ci_lb_cr2 = row$ci_lb_cr2,
        ci_ub_cr2 = row$ci_ub_cr2,
        p_cr2 = row$p_cr2,
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
        min_cr2_samples = config$min_cr2_samples
      )
      row <- pem_model_summary_row(fit)
      preferred_lb <- ifelse(
        is.finite(row$ci_lb_cr2), row$ci_lb_cr2, row$ci_lb_conventional
      )
      preferred_ub <- ifelse(
        is.finite(row$ci_ub_cr2), row$ci_ub_cr2, row$ci_ub_conventional
      )

      data.frame(
        stream = row$stream,
        pooling_block = row$pooling_block,
        omitted_unit = unit,
        omitted_id = omitted,
        status = row$status,
        n_samples_remaining = row$n_samples,
        estimate = row$estimate,
        ci_lb = preferred_lb,
        ci_ub = preferred_ub,
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

