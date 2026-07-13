pem_read_sheet <- function(workbook, sheet, skip = 1L) {
  if (!file.exists(workbook)) {
    pem_abort(paste0("Workbook not found: ", workbook))
  }

  data <- readxl::read_xlsx(
    path = workbook,
    sheet = sheet,
    skip = skip,
    .name_repair = "minimal"
  )

  names(data) <- pem_normalise_names(names(data))
  data <- pem_drop_empty_rows(as.data.frame(data, stringsAsFactors = FALSE))
  rownames(data) <- NULL
  data
}

pem_load_inputs <- function(workbook, config = pem_analysis_config()) {
  list(
    sample_map = pem_read_sheet(workbook, config$sheets$sample_map),
    risk_of_bias = pem_read_sheet(
      workbook,
      config$sheets$risk_of_bias,
      skip = 0L
    ),
    logor = pem_read_sheet(workbook, config$sheets$logor),
    smd = pem_read_sheet(workbook, config$sheets$smd),
    nonlinear = pem_read_sheet(workbook, config$sheets$nonlinear)
  )
}

pem_risk_map <- function(risk_of_bias) {
  pem_require_columns(
    risk_of_bias,
    c("study_id", "overall_risk"),
    "Risk_of_Bias"
  )

  risk_of_bias |>
    dplyr::transmute(
      study_id = as.character(.data$study_id),
      overall_risk = as.character(.data$overall_risk)
    ) |>
    dplyr::distinct()
}

pem_sample_report_map <- function(sample_map) {
  pem_require_columns(
    sample_map,
    c("study_id", "sample_id_provisional", "report_id"),
    "Study_Sample_Map"
  )

  sample_map |>
    dplyr::transmute(
      study_id = as.character(.data$study_id),
      sample_id = as.character(.data$sample_id_provisional),
      report_id = as.character(.data$report_id),
      module = if ("module" %in% names(sample_map)) as.character(.data$module) else NA_character_,
      retention_interval = if ("memory_test_delay" %in% names(sample_map)) {
        as.character(.data$memory_test_delay)
      } else {
        NA_character_
      },
      age_group = if ("age_group" %in% names(sample_map)) {
        as.character(.data$age_group)
      } else {
        NA_character_
      },
      design = if ("design" %in% names(sample_map)) as.character(.data$design) else NA_character_,
      preregistration = if ("preregistration" %in% names(sample_map)) {
        as.character(.data$preregistration)
      } else {
        NA_character_
      },
      open_data_code = if ("open_data_code" %in% names(sample_map)) {
        as.character(.data$open_data_code)
      } else {
        NA_character_
      }
    ) |>
    dplyr::distinct()
}

pem_attach_sample_metadata <- function(data, sample_map) {
  map <- pem_sample_report_map(sample_map)

  data |>
    dplyr::mutate(
      study_id = as.character(.data$study_id),
      sample_id = as.character(.data$sample_id)
    ) |>
    dplyr::left_join(map, by = c("study_id", "sample_id"))
}

pem_prepare_logor <- function(data, sample_map, config = pem_analysis_config()) {
  pem_require_columns(
    data,
    c(
      "analysis_id", "freeze_id", "study_id", "sample_id", "effect_id",
      "yi", "sei", "vi", "yi_endpoint", "vi_endpoint", "pooling_block",
      "pe_sign_type", "memory_class"
    ),
    "MAI_LogOR_v1"
  )

  out <- data |>
    dplyr::mutate(
      stream = "logOR",
      effect_id_analysis = as.character(.data$effect_id),
      yi_native = pem_as_numeric(.data$yi),
      sei_native = pem_as_numeric(.data$sei),
      vi_native = pem_as_numeric(.data$vi),
      yi_endpoint = pem_as_numeric(.data$yi_endpoint),
      vi_endpoint = pem_as_numeric(.data$vi_endpoint),
      endpoint_used = .data$pooling_block %in% config$logor_endpoint_blocks,
      yi_analysis = ifelse(.data$endpoint_used, .data$yi_endpoint, .data$yi_native),
      vi_analysis = ifelse(.data$endpoint_used, .data$vi_endpoint, .data$vi_native),
      display_measure = "odds_ratio"
    )

  pem_attach_sample_metadata(out, sample_map)
}

pem_prepare_smd <- function(data, sample_map) {
  pem_require_columns(
    data,
    c(
      "analysis_id", "freeze_id", "study_id", "sample_id", "effect_id",
      "yi", "sei", "vi", "smd_type", "pooling_block", "pe_sign_type",
      "memory_class"
    ),
    "MAI_SMD_v1"
  )

  out <- data |>
    dplyr::mutate(
      stream = "SMD",
      effect_id_analysis = as.character(.data$effect_id),
      yi_native = pem_as_numeric(.data$yi),
      sei_native = pem_as_numeric(.data$sei),
      vi_native = pem_as_numeric(.data$vi),
      endpoint_used = FALSE,
      yi_analysis = .data$yi_native,
      vi_analysis = .data$vi_native,
      display_measure = "hedges_g"
    )

  pem_attach_sample_metadata(out, sample_map)
}

pem_prepare_nonlinear <- function(data, sample_map) {
  pem_require_columns(
    data,
    c(
      "analysis_id", "freeze_id", "study_id", "sample_id",
      "effect_id_quadratic", "beta_quadratic", "sei_quadratic",
      "vi_quadratic", "joint_model_status"
    ),
    "MAI_Nonlinear_v1"
  )

  out <- data |>
    dplyr::mutate(
      stream = "nonlinear",
      pooling_block = "NONLINEAR_QUADRATIC",
      effect_id_analysis = as.character(.data$effect_id_quadratic),
      yi_native = pem_as_numeric(.data$beta_quadratic),
      sei_native = pem_as_numeric(.data$sei_quadratic),
      vi_native = pem_as_numeric(.data$vi_quadratic),
      endpoint_used = FALSE,
      yi_analysis = .data$yi_native,
      vi_analysis = .data$vi_native,
      pe_sign_type = "nonlinear_quadratic",
      memory_class = if ("memory_class" %in% names(data)) {
        as.character(.data$memory_class)
      } else {
        as.character(.data$memory_outcome)
      },
      display_measure = "quadratic_logit_coefficient"
    )

  pem_attach_sample_metadata(out, sample_map)
}

pem_prepare_inputs <- function(raw, config = pem_analysis_config()) {
  prepared <- list(
    logor = pem_prepare_logor(raw$logor, raw$sample_map, config),
    smd = pem_prepare_smd(raw$smd, raw$sample_map),
    nonlinear = pem_prepare_nonlinear(raw$nonlinear, raw$sample_map)
  )

  risk <- pem_risk_map(raw$risk_of_bias)
  lapply(
    prepared,
    function(data) dplyr::left_join(data, risk, by = "study_id")
  )
}
