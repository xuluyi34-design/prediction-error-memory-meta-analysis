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

pem_write_model_outputs <- function(fits, run_dir) {
  model_dir <- pem_make_dir(file.path(run_dir, "models"))
  plot_dir <- pem_make_dir(file.path(run_dir, "plots"))

  summaries <- pem_model_summary(fits)
  pem_write_csv(summaries, file.path(model_dir, "model_summary.csv"))

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

pem_write_run_note <- function(run_dir, config) {
  note <- c(
    config$project_title,
    paste0("Protocol version: ", config$protocol_version),
    paste0("Protocol freeze date: ", config$protocol_freeze_date),
    paste0("Analysis data version: ", config$analysis_data_version),
    paste0("Run completed: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    "",
    "Interpret all outputs together with the audit tables and protocol.",
    "A descriptive_only status means that no pooled estimate was forced."
  )
  writeLines(note, con = file.path(run_dir, "RUN_NOTE.txt"), useBytes = TRUE)
}
