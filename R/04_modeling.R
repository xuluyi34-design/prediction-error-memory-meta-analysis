pem_dependence_profile <- function(data) {
  pem_require_columns(
    data,
    c("sample_id", "report_id", "effect_id_analysis"),
    "model data"
  )

  effects_per_sample <- table(as.character(data$sample_id))
  list(
    n_clusters = length(effects_per_sample),
    n_reports = dplyr::n_distinct(data$report_id),
    max_effects_per_sample = max(effects_per_sample),
    has_dependent_effects = any(effects_per_sample > 1L)
  )
}

pem_model_decision <- function(data, min_samples = 3L,
                               min_cr2_clusters = 4L) {
  profile <- pem_dependence_profile(data)

  if (profile$n_clusters < min_samples) {
    return(c(
      profile,
      list(
        status = "descriptive_only",
        model_type = "none",
        inference_method = "none",
        reason = paste0(
          "Only ", profile$n_clusters,
          " independent sample cluster(s); minimum for pooling is ",
          min_samples, "."
        )
      )
    ))
  }

  if (
    profile$has_dependent_effects &&
      profile$n_clusters < min_cr2_clusters
  ) {
    return(c(
      profile,
      list(
        status = "descriptive_only",
        model_type = "none",
        inference_method = "none",
        reason = paste0(
          "The block contains multiple effects from at least one sample, but only ",
          profile$n_clusters, " independent Sample_ID cluster(s); CR2 requires at ",
          "least ", min_cr2_clusters, "."
        )
      )
    ))
  }

  if (profile$has_dependent_effects) {
    return(c(
      profile,
      list(
        status = "model_eligible",
        model_type = "rma.mv",
        inference_method = "CR2/Satterthwaite",
        reason = paste0(
          "At least one sample contributes multiple correlated effects and ",
          profile$n_clusters,
          " independent Sample_ID clusters are available."
        )
      )
    ))
  }

  c(
    profile,
    list(
      status = "model_eligible",
      model_type = "rma.uni",
      inference_method = "Hartung-Knapp",
      reason = paste0(
        "Every sample contributes one effect; fit a single-level REML model ",
        "with Hartung-Knapp inference across ", profile$n_clusters,
        " independent samples."
      )
    )
  )
}

pem_block_interpretation <- function(data, status) {
  block <- unique(as.character(data$pooling_block))[[1]]
  n_samples <- dplyr::n_distinct(data$sample_id)
  n_reports <- dplyr::n_distinct(data$report_id)

  if (!identical(status, "model_fitted")) {
    return(list(
      synthesis_scope = "individual_effects_only",
      evidence_status = "descriptive",
      note = "No pooled estimate is interpreted; report the individual effects only."
    ))
  }

  synthesis_scope <- if (n_reports == 1L) "within_report" else "cross_report"
  evidence_status <- "pooled"
  note <- if (n_reports == 1L) {
    paste0(
      "Within-report synthesis across ", n_samples,
      " independent samples; this is not a cross-study conclusion."
    )
  } else {
    paste0(
      "Cross-report synthesis across ", n_samples, " independent samples from ",
      n_reports, " reports."
    )
  }

  if (identical(block, "LOGOR_BINARY_CAT")) {
    evidence_status <- "preliminary"
    note <- paste0(
      "Cross-report synthesis across ", n_samples, " independent samples from ",
      n_reports, " reports; the evidence remains preliminary because the block ",
      "is small."
    )
  } else if (
    identical(block, "LOGOR_ORDERED_ENDPOINT") && n_reports == 1L
  ) {
    evidence_status <- "within_report_only"
    note <- paste0(
      "Single-report synthesis across ", n_samples,
      " age samples; do not present it as a cross-study conclusion."
    )
  } else if (
    identical(block, "LOGOR_RAW_URPE_POINT") && n_reports == 1L
  ) {
    evidence_status <- "within_report_only"
    note <- paste0(
      "Single-report synthesis across ", n_samples,
      " experiments; do not present it as a cross-study conclusion."
    )
  } else if (identical(block, "PAIRED_GZ")) {
    evidence_status <- "exploratory"
    note <- paste0(
      if (n_reports == 1L) "Within-report" else "Cross-report",
      " PAIRED_GZ synthesis across ", n_samples,
      " independent samples; interpret this pooled result as exploratory."
    )
  }

  list(
    synthesis_scope = synthesis_scope,
    evidence_status = evidence_status,
    note = note
  )
}

pem_block_readiness <- function(data, min_samples,
                                min_cr2_clusters = 4L) {
  split_data <- split(data, data$pooling_block, drop = TRUE)
  rows <- lapply(split_data, function(block_data) {
    decision <- pem_model_decision(
      block_data,
      min_samples = min_samples,
      min_cr2_clusters = min_cr2_clusters
    )
    scope <- if (decision$status == "model_eligible") {
      if (decision$n_reports == 1L) "within_report" else "cross_report"
    } else {
      "individual_effects_only"
    }

    data.frame(
      stream = unique(block_data$stream),
      pooling_block = unique(block_data$pooling_block),
      n_effects = nrow(block_data),
      n_samples = decision$n_clusters,
      n_reports = decision$n_reports,
      n_clusters = decision$n_clusters,
      cluster_variable = "sample_id",
      max_effects_per_sample = decision$max_effects_per_sample,
      has_dependent_effects = decision$has_dependent_effects,
      minimum_required = as.integer(min_samples),
      minimum_cr2_clusters = as.integer(min_cr2_clusters),
      status = decision$status,
      selected_model = decision$model_type,
      inference_method = decision$inference_method,
      synthesis_scope = scope,
      reason = decision$reason,
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(rows)
}

pem_build_sampling_v <- function(data, rho = 0.50) {
  pem_require_columns(
    data,
    c("vi_analysis", "sample_id", "effect_id_analysis"),
    "model data"
  )

  metafor::vcalc(
    vi = vi_analysis,
    cluster = sample_id,
    obs = effect_id_analysis,
    data = data,
    rho = rho,
    checkpd = TRUE
  )
}

pem_random_structure <- function(data) {
  multiple_effects <- any(table(data$sample_id) > 1L)
  if (!multiple_effects) {
    pem_abort(
      "A multilevel random structure is only used when a sample contributes multiple effects."
    )
  }

  report_estimable <- dplyr::n_distinct(data$report_id) > 1L
  random <- list()
  labels <- character()
  notes <- character()

  if (report_estimable) {
    random <- c(random, list(stats::as.formula("~ 1 | report_id")))
    labels <- c(labels, "report_id")
  } else {
    notes <- c(
      notes,
      "Report-level heterogeneity is not estimable because the block has one report."
    )
  }

  random <- c(
    random,
    list(stats::as.formula("~ 1 | sample_id/effect_id_analysis"))
  )
  labels <- c(labels, "sample_id/effect_id_analysis")

  list(random = random, labels = labels, notes = notes)
}

pem_finalize_fit <- function(fit) {
  interpretation <- pem_block_interpretation(fit$data, fit$status)
  fit$synthesis_scope <- interpretation$synthesis_scope
  fit$evidence_status <- interpretation$evidence_status
  fit$notes <- unique(c(fit$notes, interpretation$note))
  class(fit) <- "pem_meta_fit"
  fit
}

pem_fit_block <- function(data, rho = 0.50, min_samples = 3L,
                          min_cr2_clusters = 4L) {
  blocks <- unique(data$pooling_block)
  streams <- unique(data$stream)

  if (length(blocks) != 1L || length(streams) != 1L) {
    pem_abort("pem_fit_block() requires exactly one stream and one pooling_block.")
  }

  data <- data |>
    dplyr::arrange(.data$sample_id, .data$effect_id_analysis) |>
    dplyr::mutate(
      report_id = factor(.data$report_id),
      sample_id = factor(.data$sample_id),
      effect_id_analysis = factor(.data$effect_id_analysis)
    )

  decision <- pem_model_decision(
    data,
    min_samples = min_samples,
    min_cr2_clusters = min_cr2_clusters
  )
  dependence_structure <- if (decision$has_dependent_effects) {
    "multiple_effects_within_sample"
  } else {
    "one_effect_per_sample"
  }

  base <- list(
    stream = streams[[1]],
    block = blocks[[1]],
    data = data,
    rho = if (decision$has_dependent_effects) rho else NA_real_,
    n_effects = nrow(data),
    n_samples = decision$n_clusters,
    n_reports = decision$n_reports,
    n_clusters = decision$n_clusters,
    cluster_variable = "sample_id",
    dependence_structure = dependence_structure,
    model_type = decision$model_type,
    inference_method = decision$inference_method,
    random_structure = NA_character_,
    model = NULL,
    diagnostic_model = NULL,
    V = NULL,
    cr2 = NULL,
    warnings = character(),
    error = NULL,
    notes = character()
  )

  if (decision$status == "descriptive_only") {
    base$status <- "descriptive_only"
    base$notes <- decision$reason
    return(pem_finalize_fit(base))
  }

  if (decision$model_type == "rma.uni") {
    base$V <- diag(as.numeric(data$vi_analysis), nrow = nrow(data))
    base$random_structure <- "single_between_effect_tau2"
    model_capture <- pem_capture_conditions(
      metafor::rma.uni(
        yi = yi_analysis,
        vi = vi_analysis,
        data = data,
        method = "REML",
        test = "knha"
      )
    )

    base$warnings <- model_capture$warnings
    if (!is.null(model_capture$error)) {
      base$status <- "model_failed"
      base$error <- model_capture$error
      return(pem_finalize_fit(base))
    }

    base$model <- model_capture$value
    base$status <- "model_fitted"
    base$notes <- c(
      decision$reason,
      paste(
        "CR2 is not applicable because no sample contributes multiple effects;",
        "Report_ID is not substituted as the CR2 cluster variable."
      )
    )
    return(pem_finalize_fit(base))
  }

  V_capture <- pem_capture_conditions(pem_build_sampling_v(data, rho = rho))
  if (!is.null(V_capture$error)) {
    base$status <- "model_failed"
    base$error <- V_capture$error
    base$warnings <- V_capture$warnings
    return(pem_finalize_fit(base))
  }

  base$V <- V_capture$value
  structure <- pem_random_structure(data)
  base$notes <- c(decision$reason, structure$notes)
  base$random_structure <- paste(structure$labels, collapse = " + ")

  model_capture <- pem_capture_conditions(
    metafor::rma.mv(
      yi = yi_analysis,
      V = base$V,
      random = structure$random,
      data = data,
      method = "REML"
    )
  )

  base$warnings <- unique(c(V_capture$warnings, model_capture$warnings))
  if (!is.null(model_capture$error)) {
    base$status <- "model_failed"
    base$error <- model_capture$error
    return(pem_finalize_fit(base))
  }

  multilevel_model <- model_capture$value
  cr2_capture <- pem_capture_conditions(
    clubSandwich::coef_test(
      multilevel_model,
      vcov = "CR2",
      cluster = data$sample_id,
      test = "Satterthwaite"
    )
  )
  base$warnings <- unique(c(base$warnings, cr2_capture$warnings))

  if (!is.null(cr2_capture$error)) {
    base$status <- "model_failed"
    base$error <- paste("CR2 failed:", cr2_capture$error)
    base$diagnostic_model <- multilevel_model
    return(pem_finalize_fit(base))
  }

  base$model <- multilevel_model
  base$cr2 <- cr2_capture$value
  base$status <- "model_fitted"
  pem_finalize_fit(base)
}

pem_fit_stream <- function(data, config = pem_analysis_config(),
                           min_samples = config$min_quantitative_samples,
                           rho = config$primary_rho) {
  split_data <- split(data, data$pooling_block, drop = TRUE)
  lapply(
    split_data,
    pem_fit_block,
    rho = rho,
    min_samples = min_samples,
    min_cr2_clusters = config$min_cr2_clusters
  )
}

pem_extract_named <- function(data, candidates, default = NA_real_) {
  if (is.null(data)) return(default)
  for (candidate in candidates) {
    if (candidate %in% names(data)) return(as.numeric(data[[candidate]][[1]]))
  }
  default
}

pem_prediction_interval <- function(model) {
  captured <- pem_capture_conditions(stats::predict(model, level = 95))
  if (!is.null(captured$error)) {
    return(c(pi_lb = NA_real_, pi_ub = NA_real_))
  }

  prediction <- captured$value
  c(
    pi_lb = as.numeric(prediction$pi.lb %||% NA_real_),
    pi_ub = as.numeric(prediction$pi.ub %||% NA_real_)
  )
}

pem_model_summary_row <- function(fit) {
  base <- data.frame(
    stream = fit$stream,
    pooling_block = fit$block,
    status = fit$status,
    n_effects = fit$n_effects,
    n_samples = fit$n_samples,
    n_reports = fit$n_reports,
    n_clusters = fit$n_clusters,
    cluster_variable = fit$cluster_variable,
    dependence_structure = fit$dependence_structure,
    model_type = fit$model_type,
    inference_method = fit$inference_method,
    synthesis_scope = fit$synthesis_scope,
    evidence_status = fit$evidence_status,
    rho = fit$rho,
    random_structure = fit$random_structure %||% NA_character_,
    primary_estimate = NA_real_,
    primary_se = NA_real_,
    primary_df = NA_real_,
    primary_ci_lb = NA_real_,
    primary_ci_ub = NA_real_,
    primary_p = NA_real_,
    estimate = NA_real_,
    se_conventional = NA_real_,
    ci_lb_conventional = NA_real_,
    ci_ub_conventional = NA_real_,
    p_conventional = NA_real_,
    se_cr2 = NA_real_,
    df_cr2 = NA_real_,
    ci_lb_cr2 = NA_real_,
    ci_ub_cr2 = NA_real_,
    p_cr2 = NA_real_,
    pi_lb = NA_real_,
    pi_ub = NA_real_,
    qe = NA_real_,
    qe_p = NA_real_,
    estimate_display = NA_real_,
    ci_lb_display = NA_real_,
    ci_ub_display = NA_real_,
    display_scale = ifelse(fit$stream == "logOR", "OR", "coefficient"),
    warnings = paste(fit$warnings, collapse = " | "),
    notes = paste(fit$notes, collapse = " | "),
    error = fit$error %||% NA_character_,
    stringsAsFactors = FALSE
  )

  if (is.null(fit$model)) return(base)

  model <- fit$model
  base$estimate <- as.numeric(model$b[[1]])
  base$se_conventional <- as.numeric(model$se[[1]])
  base$ci_lb_conventional <- as.numeric(model$ci.lb[[1]])
  base$ci_ub_conventional <- as.numeric(model$ci.ub[[1]])
  base$p_conventional <- as.numeric(model$pval[[1]])
  base$qe <- as.numeric(model$QE %||% NA_real_)
  base$qe_p <- as.numeric(model$QEp %||% NA_real_)

  pi <- pem_prediction_interval(model)
  base$pi_lb <- unname(pi[["pi_lb"]])
  base$pi_ub <- unname(pi[["pi_ub"]])

  if (!is.null(fit$cr2)) {
    cr2 <- as.data.frame(fit$cr2)
    base$se_cr2 <- pem_extract_named(cr2, c("SE", "se"))
    base$df_cr2 <- pem_extract_named(cr2, c("df_Satt", "df", "d.f."))
    base$p_cr2 <- pem_extract_named(cr2, c("p_Satt", "p_val", "p"))

    if (is.finite(base$se_cr2) && is.finite(base$df_cr2) && base$df_cr2 > 0) {
      critical <- stats::qt(0.975, df = base$df_cr2)
      base$ci_lb_cr2 <- base$estimate - critical * base$se_cr2
      base$ci_ub_cr2 <- base$estimate + critical * base$se_cr2
    }
  }

  if (identical(fit$model_type, "rma.uni")) {
    model_parameters <- as.numeric(model$p %||% 1L)
    base$primary_estimate <- base$estimate
    base$primary_se <- base$se_conventional
    base$primary_df <- max(fit$n_effects - model_parameters, 0)
    base$primary_ci_lb <- base$ci_lb_conventional
    base$primary_ci_ub <- base$ci_ub_conventional
    base$primary_p <- base$p_conventional
  } else if (!is.null(fit$cr2)) {
    base$primary_estimate <- base$estimate
    base$primary_se <- base$se_cr2
    base$primary_df <- base$df_cr2
    base$primary_ci_lb <- base$ci_lb_cr2
    base$primary_ci_ub <- base$ci_ub_cr2
    base$primary_p <- base$p_cr2
  }

  if (fit$stream == "logOR") {
    base$estimate_display <- exp(base$primary_estimate)
    base$ci_lb_display <- exp(base$primary_ci_lb)
    base$ci_ub_display <- exp(base$primary_ci_ub)
  } else {
    base$estimate_display <- base$primary_estimate
    base$ci_lb_display <- base$primary_ci_lb
    base$ci_ub_display <- base$primary_ci_ub
  }

  base
}

pem_model_summary <- function(fits) {
  pem_bind_rows(lapply(fits, pem_model_summary_row))
}

pem_heterogeneity_components <- function(fit) {
  if (is.null(fit$model)) return(data.frame())

  if (identical(fit$model_type, "rma.uni")) {
    return(data.frame(
      component = "between_effect_tau2",
      variance = as.numeric(fit$model$tau2),
      stringsAsFactors = FALSE
    ))
  }

  sigma2 <- as.numeric(fit$model$sigma2)
  labels <- fit$model$s.names %||% paste0("sigma2_", seq_along(sigma2))
  data.frame(
    component = labels,
    variance = sigma2,
    stringsAsFactors = FALSE
  )
}

pem_variance_components <- function(fit) {
  components <- pem_heterogeneity_components(fit)
  if (nrow(components) == 0L) return(data.frame())

  data.frame(
    stream = fit$stream,
    pooling_block = fit$block,
    model_type = fit$model_type,
    component = components$component,
    variance = components$variance,
    stringsAsFactors = FALSE
  )
}

pem_i2_decomposition <- function(fit) {
  if (is.null(fit$model) || is.null(fit$V)) return(data.frame())

  model <- fit$model
  V <- as.matrix(fit$V)
  X <- as.matrix(model$X)

  sampling_capture <- pem_capture_conditions({
    W <- solve(V)
    middle <- solve(crossprod(X, W %*% X))
    P <- W - W %*% X %*% middle %*% t(X) %*% W
    (nrow(V) - ncol(X)) / sum(diag(P))
  })

  sampling_variance <- if (is.null(sampling_capture$error)) {
    as.numeric(sampling_capture$value)
  } else {
    NA_real_
  }

  components <- pem_heterogeneity_components(fit)
  total <- sum(components$variance, na.rm = TRUE) + sampling_variance
  if (!is.finite(total) || total <= 0) return(data.frame())

  component_rows <- data.frame(
    stream = fit$stream,
    pooling_block = fit$block,
    model_type = fit$model_type,
    component = components$component,
    i2_percent = 100 * components$variance / total,
    sampling_variance_typical = sampling_variance,
    stringsAsFactors = FALSE
  )

  dplyr::bind_rows(
    component_rows,
    data.frame(
      stream = fit$stream,
      pooling_block = fit$block,
      model_type = fit$model_type,
      component = "total_heterogeneity",
      i2_percent = 100 * sum(components$variance, na.rm = TRUE) / total,
      sampling_variance_typical = sampling_variance,
      stringsAsFactors = FALSE
    )
  )
}
