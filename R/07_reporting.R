pem_write_audit <- function(audit, run_dir, workbook) {
  audit_dir <- pem_make_dir(file.path(run_dir, "audit"))
  pem_write_csv(audit$summary, file.path(audit_dir, "audit_summary.csv"))
  pem_write_csv(audit$issues, file.path(audit_dir, "audit_issues.csv"))
  pem_write_csv(audit$block_manifest, file.path(audit_dir, "block_manifest.csv"))
  pem_write_csv(audit$analysis_manifest, file.path(audit_dir, "analysis_manifest.csv"))

  fingerprint <- data.frame(
    workbook_path = normalizePath(workbook, winslash = "/", mustWork = TRUE),
    workbook_md5 = unname(tools::md5sum(workbook)),
    analysis_time = format(Sys.time(), tz = "UTC", usetz = TRUE),
    stringsAsFactors = FALSE
  )
  pem_write_csv(fingerprint, file.path(audit_dir, "workbook_fingerprint.csv"))
  invisible(audit_dir)
}

pem_plot_forest <- function(fit, path) {
  data <- fit$data
  if (nrow(data) == 0L) return(invisible(NULL))

  labels <- paste0(
    data$first_author %||% data$study_id,
    " (", data$year %||% "", "; ", as.character(data$sample_id), ")"
  )
  use_or <- identical(fit$stream, "logOR")

  grDevices::png(
    filename = path,
    width = 1800,
    height = max(1200, 230 + 130 * nrow(data)),
    res = 180
  )
  on.exit(grDevices::dev.off(), add = TRUE)

  if (!is.null(fit$model)) {
    metafor::forest(
      fit$model,
      slab = labels,
      atransf = if (use_or) exp else NULL,
      xlab = if (use_or) "Odds ratio" else "Effect estimate",
      header = c("Study / sample", if (use_or) "OR [95% CI]" else "Estimate [95% CI]")
    )
  } else {
    metafor::forest(
      x = data$yi_analysis,
      vi = data$vi_analysis,
      slab = labels,
      atransf = if (use_or) exp else NULL,
      xlab = if (use_or) "Odds ratio" else "Effect estimate",
      header = c("Study / sample", if (use_or) "OR [95% CI]" else "Estimate [95% CI]")
    )
  }

  graphics::title(main = paste(fit$stream, fit$block, sep = " — "))
  invisible(path)
}

pem_descriptive_effects <- function(fits) {
  rows <- lapply(fits, function(fit) {
    if (!identical(fit$status, "descriptive_only")) return(data.frame())

    data <- fit$data
    ci_lb <- data$yi_analysis - stats::qnorm(0.975) * sqrt(data$vi_analysis)
    ci_ub <- data$yi_analysis + stats::qnorm(0.975) * sqrt(data$vi_analysis)
    use_or <- identical(fit$stream, "logOR")

    data.frame(
      stream = fit$stream,
      pooling_block = fit$block,
      status = fit$status,
      synthesis_scope = fit$synthesis_scope,
      evidence_status = fit$evidence_status,
      analysis_id = as.character(data$analysis_id %||% NA_character_),
      study_id = as.character(data$study_id %||% NA_character_),
      sample_id = as.character(data$sample_id),
      report_id = as.character(data$report_id),
      effect_id = as.character(data$effect_id_analysis),
      estimate = as.numeric(data$yi_analysis),
      se = sqrt(as.numeric(data$vi_analysis)),
      ci_lb = ci_lb,
      ci_ub = ci_ub,
      estimate_display = if (use_or) exp(data$yi_analysis) else data$yi_analysis,
      ci_lb_display = if (use_or) exp(ci_lb) else ci_lb,
      ci_ub_display = if (use_or) exp(ci_ub) else ci_ub,
      display_scale = if (use_or) "OR" else "coefficient",
      interpretation = paste(fit$notes, collapse = " | "),
      stringsAsFactors = FALSE
    )
  })

  pem_bind_rows(rows)
}

pem_write_model_outputs <- function(fits, run_dir) {
  model_dir <- pem_make_dir(file.path(run_dir, "models"))
  plot_dir <- pem_make_dir(file.path(run_dir, "plots"))

  summaries <- pem_model_summary(fits)
  pem_write_csv(summaries, file.path(model_dir, "model_summary.csv"))

  descriptive <- pem_descriptive_effects(fits)
  pem_write_csv(
    descriptive,
    file.path(model_dir, "descriptive_effects.csv")
  )

  variance <- pem_bind_rows(lapply(fits, pem_variance_components))
  pem_write_csv(variance, file.path(model_dir, "variance_components.csv"))

  i2 <- pem_bind_rows(lapply(fits, pem_i2_decomposition))
  pem_write_csv(i2, file.path(model_dir, "i2_decomposition.csv"))

  saveRDS(fits, file.path(model_dir, "model_objects.rds"))

  for (name in names(fits)) {
    filename <- paste0("forest_", pem_safe_filename(name), ".png")
    pem_plot_forest(fits[[name]], file.path(plot_dir, filename))
  }

  invisible(summaries)
}

pem_write_session_info <- function(run_dir) {
  path <- file.path(run_dir, "sessionInfo.txt")
  capture.output(utils::sessionInfo(), file = path)
  invisible(path)
}

pem_write_run_note <- function(run_dir, config, workbook) {
  note <- c(
    config$project_title,
    paste0("Protocol version: ", config$protocol_version),
    paste0("Protocol freeze date: ", config$protocol_freeze_date),
    paste0("Analysis data version: ", config$analysis_data_version),
    paste0("Analysis method/output version: ", config$analysis_method_version),
    paste0("Input workbook: ", basename(workbook)),
    paste0(
      "Expected frozen counts: ",
      config$expected_counts[["logor"]], " logOR + ",
      config$expected_counts[["smd"]], " SMD + ",
      config$expected_counts[["nonlinear"]], " nonlinear = ",
      config$expected_total
    ),
    paste0("Run completed: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    "",
    "Interpret all outputs together with the audit tables and protocol.",
    "A descriptive_only status means that no pooled estimate was forced.",
    paste(
      "One effect per sample is analyzed with rma.uni REML plus Hartung-Knapp;",
      "dependent effects require rma.mv plus CR2 clustered by Sample_ID."
    )
  )
  writeLines(note, con = file.path(run_dir, "RUN_NOTE.txt"), useBytes = TRUE)
}

pem_write_run_metadata <- function(run_dir, config, workbook) {
  metadata <- data.frame(
    analysis_data_version = config$analysis_data_version,
    analysis_method_version = config$analysis_method_version,
    input_workbook = basename(workbook),
    expected_logor = unname(config$expected_counts[["logor"]]),
    expected_smd = unname(config$expected_counts[["smd"]]),
    expected_nonlinear = unname(config$expected_counts[["nonlinear"]]),
    expected_total = config$expected_total,
    primary_one_effect_model = "rma.uni REML",
    primary_one_effect_inference = "Hartung-Knapp",
    dependent_effect_model = "rma.mv REML",
    dependent_effect_inference = "CR2/Satterthwaite by sample_id",
    stringsAsFactors = FALSE
  )
  pem_write_csv(metadata, file.path(run_dir, "run_metadata.csv"))
}
