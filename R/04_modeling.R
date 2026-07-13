pem_block_readiness <- function(data, min_samples) {
  data |>
    dplyr::group_by(.data$stream, .data$pooling_block) |>
    dplyr::summarise(
      n_effects = dplyr::n(),
      n_samples = dplyr::n_distinct(.data$sample_id),
      n_reports = dplyr::n_distinct(.data$report_id),
      max_effects_per_sample = max(table(.data$sample_id)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      minimum_required = as.integer(min_samples),
      status = ifelse(
        .data$n_samples >= .data$minimum_required,
        "model_eligible",
        "descriptive_only"
      ),
      reason = ifelse(
        .data$status == "model_eligible",
        "Compatible block meets the current independent-sample minimum.",
        paste0(
          "Only ", .data$n_samples, " independent sample(s); minimum is ",
          .data$minimum_required, "."
        )
      )
    )
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
  report_estimable <- dplyr::n_distinct(data$report_id) > 1L
  multiple_effects <- any(table(data$sample_id) > 1L)

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

  if (multiple_effects) {
    random <- c(random, list(stats::as.formula("~ 1 | sample_id/effect_id_analysis")))
    labels <- c(labels, "sample_id/effect_id_analysis")
  } else {
    random <- c(random, list(stats::as.formula("~ 1 | sample_id")))
    labels <- c(labels, "sample_id")
    notes <- c(
      notes,
      paste(
        "The nested Effect_ID component is algebraically redundant because",
        "every sample contributes one effect in this block; a single sample-level",
        "heterogeneity component is fitted."
      )
    )
  }

  list(random = random, labels = labels, notes = notes)
}

pem_fit_block <- function(data, rho = 0.50, min_samples = 3L,
                          min_cr2_samples = 4L) {
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

  n_samples <- dplyr::n_distinct(data$sample_id)
  base <- list(
    stream = streams[[1]],
    block = blocks[[1]],
    data = data,
    rho = rho,
    n_effects = nrow(data),
    n_samples = n_samples,
    n_reports = dplyr::n_distinct(data$report_id),
    model = NULL,
    V = NULL,
    cr2 = NULL,
    warnings = character(),
    error = NULL,
    notes = character()
  )

  if (n_samples < min_samples) {
    base$status <- "descriptive_only"
    base$notes <- paste0(
      "Only ", n_samples, " independent sample(s); minimum is ", min_samples, "."
    )
    class(base) <- "pem_meta_fit"
    return(base)
  }

  V_capture <- pem_capture_conditions(pem_build_sampling_v(data, rho = rho))
  if (!is.null(V_capture$error)) {
    base$status <- "model_failed"
    base$error <- V_capture$error
    base$warnings <- V_capture$warnings
    class(base) <- "pem_meta_fit"
    return(base)
  }

  base$V <- V_capture$value
  structure <- pem_random_structure(data)
  base$notes <- structure$notes
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
    class(base) <- "pem_meta_fit"
    return(base)
  }

  base$model <- model_capture$value
  base$status <- "model_fitted"

  if (n_samples >= min_cr2_samples) {
    cr2_capture <- pem_capture_conditions(
      clubSandwich::coef_test(
        base$model,
        vcov = "CR2",
        cluster = data$sample_id,
        test = "Satterthwaite"
      )
    )
    base$warnings <- unique(c(base$warnings, cr2_capture$warnings))
    if (is.null(cr2_capture$error)) {
      base$cr2 <- cr2_capture$value
    } else {
      base$notes <- c(base$notes, paste("CR2 failed:", cr2_capture$error))
    }
  } else {
    base$notes <- c(
      base$notes,
      paste0(
        "CR2 not attempted: ", n_samples,
        " independent samples; minimum is ", min_cr2_samples, "."
      )
    )
  }

  class(base) <- "pem_meta_fit"
  base
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
    min_cr2_samples = config$min_cr2_samples
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
    rho = fit$rho,
    random_structure = fit$random_structure %||% NA_character_,
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

  preferred_lb <- ifelse(is.finite(base$ci_lb_cr2), base$ci_lb_cr2, base$ci_lb_conventional)
  preferred_ub <- ifelse(is.finite(base$ci_ub_cr2), base$ci_ub_cr2, base$ci_ub_conventional)

  if (fit$stream == "logOR") {
    base$estimate_display <- exp(base$estimate)
    base$ci_lb_display <- exp(preferred_lb)
    base$ci_ub_display <- exp(preferred_ub)
  } else {
    base$estimate_display <- base$estimate
    base$ci_lb_display <- preferred_lb
    base$ci_ub_display <- preferred_ub
  }

  base
}

pem_model_summary <- function(fits) {
  pem_bind_rows(lapply(fits, pem_model_summary_row))
}

pem_variance_components <- function(fit) {
  if (is.null(fit$model)) return(data.frame())

  sigma2 <- as.numeric(fit$model$sigma2)
  labels <- fit$model$s.names %||% paste0("sigma2_", seq_along(sigma2))

  data.frame(
    stream = fit$stream,
    pooling_block = fit$block,
    component = labels,
    variance = sigma2,
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

  sigma2 <- as.numeric(model$sigma2)
  labels <- model$s.names %||% paste0("sigma2_", seq_along(sigma2))
  total <- sum(sigma2, na.rm = TRUE) + sampling_variance

  if (!is.finite(total) || total <= 0) return(data.frame())

  components <- data.frame(
    stream = fit$stream,
    pooling_block = fit$block,
    component = labels,
    i2_percent = 100 * sigma2 / total,
    sampling_variance_typical = sampling_variance,
    stringsAsFactors = FALSE
  )

  dplyr::bind_rows(
    components,
    data.frame(
      stream = fit$stream,
      pooling_block = fit$block,
      component = "total_heterogeneity",
      i2_percent = 100 * sum(sigma2, na.rm = TRUE) / total,
      sampling_variance_typical = sampling_variance,
      stringsAsFactors = FALSE
    )
  )
}

