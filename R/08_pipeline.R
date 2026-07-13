pem_run_analysis <- function(workbook,
                             output_root = "results",
                             strict_freeze = TRUE,
                             run_id = pem_run_id(),
                             config = pem_analysis_config()) {
  pem_check_dependencies(config)

  run_dir <- pem_make_dir(file.path(
    output_root,
    paste0(config$analysis_method_version, "_", run_id)
  ))
  message("Reading frozen analysis inputs...")
  raw <- pem_load_inputs(workbook, config)
  prepared <- pem_prepare_inputs(raw, config)

  message("Running input and freeze audit...")
  audit <- pem_audit_inputs(
    raw = raw,
    prepared = prepared,
    config = config,
    strict_freeze = strict_freeze,
    workbook = workbook
  )
  pem_write_audit(audit, run_dir, workbook)
  pem_stop_on_audit_error(audit)

  readiness <- dplyr::bind_rows(
    pem_block_readiness(
      prepared$logor,
      config$min_quantitative_samples,
      config$min_cr2_clusters
    ),
    pem_block_readiness(
      prepared$smd,
      config$min_quantitative_samples,
      config$min_cr2_clusters
    ),
    pem_block_readiness(
      prepared$nonlinear,
      config$min_nonlinear_samples,
      config$min_cr2_clusters
    )
  )
  pem_write_csv(readiness, file.path(run_dir, "block_readiness.csv"))

  message("Fitting compatible block-specific models...")
  fits_logor <- pem_fit_stream(prepared$logor, config)
  fits_smd <- pem_fit_stream(prepared$smd, config)
  fits_nonlinear <- pem_fit_stream(
    prepared$nonlinear,
    config,
    min_samples = config$min_nonlinear_samples
  )
  names(fits_logor) <- paste0("logor__", names(fits_logor))
  names(fits_smd) <- paste0("smd__", names(fits_smd))
  names(fits_nonlinear) <- paste0("nonlinear__", names(fits_nonlinear))
  fits <- c(fits_logor, fits_smd, fits_nonlinear)

  model_summary <- pem_write_model_outputs(fits, run_dir)

  sensitivity_dir <- pem_make_dir(file.path(run_dir, "sensitivity"))
  rho_sensitivity <- dplyr::bind_rows(
    pem_rho_sensitivity(prepared$logor, config),
    pem_rho_sensitivity(prepared$smd, config),
    pem_rho_sensitivity(
      prepared$nonlinear,
      config,
      min_samples = config$min_nonlinear_samples
    )
  )
  pem_write_csv(
    rho_sensitivity,
    file.path(sensitivity_dir, "rho_sensitivity.csv")
  )

  leave_sample <- dplyr::bind_rows(
    pem_leave_one_unit_out(prepared$logor, "sample_id", config),
    pem_leave_one_unit_out(prepared$smd, "sample_id", config),
    pem_leave_one_unit_out(
      prepared$nonlinear,
      "sample_id",
      config,
      min_samples = config$min_nonlinear_samples
    )
  )
  pem_write_csv(
    leave_sample,
    file.path(sensitivity_dir, "leave_one_sample_out.csv")
  )

  leave_report <- dplyr::bind_rows(
    pem_leave_one_unit_out(prepared$logor, "report_id", config),
    pem_leave_one_unit_out(prepared$smd, "report_id", config),
    pem_leave_one_unit_out(
      prepared$nonlinear,
      "report_id",
      config,
      min_samples = config$min_nonlinear_samples
    )
  )
  pem_write_csv(
    leave_report,
    file.path(sensitivity_dir, "leave_one_report_out.csv")
  )

  risk_sensitivity <- dplyr::bind_rows(
    pem_risk_of_bias_sensitivity(prepared$logor, config),
    pem_risk_of_bias_sensitivity(prepared$smd, config),
    pem_risk_of_bias_sensitivity(
      prepared$nonlinear,
      config,
      min_samples = config$min_nonlinear_samples
    )
  )
  pem_write_csv(
    risk_sensitivity,
    file.path(sensitivity_dir, "risk_of_bias_sensitivity.csv")
  )

  all_prepared <- dplyr::bind_rows(prepared)
  publication_bias <- pem_publication_bias_eligibility(all_prepared, config)
  pem_write_csv(
    publication_bias,
    file.path(run_dir, "publication_bias_eligibility.csv")
  )

  snapshot_dir <- pem_make_dir(file.path(run_dir, "analysis_input_snapshot"))
  pem_write_csv(prepared$logor, file.path(snapshot_dir, "logor_analysis_input.csv"))
  pem_write_csv(prepared$smd, file.path(snapshot_dir, "smd_analysis_input.csv"))
  pem_write_csv(
    prepared$nonlinear,
    file.path(snapshot_dir, "nonlinear_analysis_input.csv")
  )

  pem_write_session_info(run_dir)
  pem_write_run_note(run_dir, config, workbook)
  pem_write_run_metadata(run_dir, config, workbook)
  writeLines(
    normalizePath(run_dir, winslash = "/", mustWork = TRUE),
    con = file.path(output_root, "latest_run.txt"),
    useBytes = TRUE
  )

  message("Analysis completed. Results: ", run_dir)
  invisible(list(
    run_dir = run_dir,
    config = config,
    raw = raw,
    prepared = prepared,
    audit = audit,
    readiness = readiness,
    fits = fits,
    model_summary = model_summary,
    rho_sensitivity = rho_sensitivity,
    leave_one_sample_out = leave_sample,
    leave_one_report_out = leave_report,
    risk_of_bias_sensitivity = risk_sensitivity,
    publication_bias_eligibility = publication_bias
  ))
}
