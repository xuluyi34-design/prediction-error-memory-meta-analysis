pem_publication_bias_eligibility <- function(data, config = pem_analysis_config()) {
  data |>
    dplyr::group_by(.data$stream, .data$pooling_block) |>
    dplyr::summarise(
      n_effects = dplyr::n(),
      n_samples = dplyr::n_distinct(.data$sample_id),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      minimum_required = config$min_publication_bias_samples,
      eligible = .data$n_samples >= .data$minimum_required,
      status = ifelse(
        .data$eligible,
        "eligible_for_prespecified_small_study_checks",
        "not_eligible_too_few_independent_samples"
      )
    )
}

pem_fit_small_study_effect <- function(data, config = pem_analysis_config()) {
  n_samples <- dplyr::n_distinct(data$sample_id)
  if (n_samples < config$min_publication_bias_samples) {
    return(list(
      status = "not_eligible_too_few_independent_samples",
      model = NULL,
      cr2 = NULL,
      n_samples = n_samples,
      error = NULL,
      warnings = character()
    ))
  }

  data <- data |>
    dplyr::mutate(
      sei_analysis = sqrt(.data$vi_analysis),
      report_id = factor(.data$report_id),
      sample_id = factor(.data$sample_id),
      effect_id_analysis = factor(.data$effect_id_analysis)
    )

  V_capture <- pem_capture_conditions(
    pem_build_sampling_v(data, rho = config$primary_rho)
  )
  if (!is.null(V_capture$error)) {
    return(list(
      status = "model_failed",
      model = NULL,
      cr2 = NULL,
      n_samples = n_samples,
      error = V_capture$error,
      warnings = V_capture$warnings
    ))
  }

  structure <- pem_random_structure(data)
  model_capture <- pem_capture_conditions(
    metafor::rma.mv(
      yi = yi_analysis,
      V = V_capture$value,
      mods = ~ sei_analysis,
      random = structure$random,
      data = data,
      method = "REML"
    )
  )

  if (!is.null(model_capture$error)) {
    return(list(
      status = "model_failed",
      model = NULL,
      cr2 = NULL,
      n_samples = n_samples,
      error = model_capture$error,
      warnings = unique(c(V_capture$warnings, model_capture$warnings))
    ))
  }

  cr2_capture <- pem_capture_conditions(
    clubSandwich::coef_test(
      model_capture$value,
      vcov = "CR2",
      cluster = data$sample_id,
      test = "Satterthwaite"
    )
  )

  list(
    status = "model_fitted",
    model = model_capture$value,
    cr2 = if (is.null(cr2_capture$error)) cr2_capture$value else NULL,
    n_samples = n_samples,
    error = cr2_capture$error,
    warnings = unique(c(
      V_capture$warnings,
      model_capture$warnings,
      cr2_capture$warnings
    )),
    notes = structure$notes
  )
}

