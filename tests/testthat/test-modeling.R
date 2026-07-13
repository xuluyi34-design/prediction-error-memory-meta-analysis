testthat::test_that("sampling V is diagonal with one effect per sample", {
  data <- data.frame(
    vi_analysis = c(0.01, 0.04),
    sample_id = c("S1", "S2"),
    effect_id_analysis = c("E1", "E2")
  )

  V <- pem_build_sampling_v(data, rho = 0.50)
  V_numeric <- matrix(as.numeric(V), nrow = nrow(data))
  testthat::expect_equal(V_numeric, diag(c(0.01, 0.04)))
})

testthat::test_that("one effect per sample selects rma.uni plus Hartung-Knapp", {
  data <- data.frame(
    stream = "logOR",
    pooling_block = "LOGOR_BINARY_CAT",
    report_id = c("R1", "R1", "R2", "R3"),
    sample_id = paste0("S", 1:4),
    effect_id_analysis = paste0("E", 1:4),
    yi_analysis = c(0.10, 0.15, 0.05, 0.20),
    vi_analysis = rep(0.04, 4),
    stringsAsFactors = FALSE
  )

  fit <- pem_fit_block(
    data,
    rho = 0.50,
    min_samples = 3L,
    min_cr2_clusters = 4L
  )
  summary <- pem_model_summary_row(fit)

  testthat::expect_identical(fit$status, "model_fitted")
  testthat::expect_identical(fit$model_type, "rma.uni")
  testthat::expect_identical(fit$inference_method, "Hartung-Knapp")
  testthat::expect_s3_class(fit$model, "rma.uni")
  testthat::expect_null(fit$cr2)
  testthat::expect_true(is.na(fit$rho))
  testthat::expect_identical(
    fit$random_structure,
    "single_between_effect_tau2"
  )
  testthat::expect_identical(fit$evidence_status, "preliminary")
  testthat::expect_true(is.finite(summary$primary_estimate))
  testthat::expect_equal(summary$primary_df, 3)
  testthat::expect_equal(nrow(pem_variance_components(fit)), 1L)
})

testthat::test_that("a one-report k=3 synthesis is explicitly limited", {
  data <- data.frame(
    stream = "logOR",
    pooling_block = "LOGOR_ORDERED_ENDPOINT",
    report_id = rep("R1", 3),
    sample_id = paste0("S", 1:3),
    effect_id_analysis = paste0("E", 1:3),
    yi_analysis = c(0.10, 0.15, 0.05),
    vi_analysis = rep(0.04, 3),
    stringsAsFactors = FALSE
  )

  fit <- pem_fit_block(data, min_samples = 3L, min_cr2_clusters = 4L)

  testthat::expect_identical(fit$status, "model_fitted")
  testthat::expect_identical(fit$synthesis_scope, "within_report")
  testthat::expect_identical(fit$evidence_status, "within_report_only")
  testthat::expect_true(any(grepl("cross-study conclusion", fit$notes)))
})

testthat::test_that("dependent effects require four Sample_ID clusters for CR2", {
  too_few_clusters <- data.frame(
    stream = "logOR",
    pooling_block = "SYNTHETIC_DEPENDENT",
    report_id = c("R1", "R1", "R2", "R3"),
    sample_id = c("S1", "S1", "S2", "S3"),
    effect_id_analysis = paste0("E", 1:4),
    yi_analysis = c(0.10, 0.12, 0.05, 0.20),
    vi_analysis = rep(0.04, 4),
    stringsAsFactors = FALSE
  )

  decision <- pem_model_decision(
    too_few_clusters,
    min_samples = 3L,
    min_cr2_clusters = 4L
  )
  fit <- pem_fit_block(
    too_few_clusters,
    min_samples = 3L,
    min_cr2_clusters = 4L
  )

  testthat::expect_identical(decision$status, "descriptive_only")
  testthat::expect_identical(decision$n_clusters, 3L)
  testthat::expect_identical(fit$status, "descriptive_only")
  testthat::expect_null(fit$model)

  enough_clusters <- rbind(
    too_few_clusters,
    transform(
      too_few_clusters[1, , drop = FALSE],
      report_id = "R4",
      sample_id = "S4",
      effect_id_analysis = "E5"
    )
  )
  decision_four <- pem_model_decision(
    enough_clusters,
    min_samples = 3L,
    min_cr2_clusters = 4L
  )

  testthat::expect_identical(decision_four$model_type, "rma.mv")
  testthat::expect_identical(
    decision_four$inference_method,
    "CR2/Satterthwaite"
  )
  testthat::expect_identical(decision_four$n_clusters, 4L)
})

testthat::test_that("k below three remains descriptive only", {
  data <- data.frame(
    stream = "SMD",
    pooling_block = "SYNTHETIC_SMALL",
    report_id = c("R1", "R2"),
    sample_id = c("S1", "S2"),
    effect_id_analysis = c("E1", "E2"),
    yi_analysis = c(0.10, 0.20),
    vi_analysis = c(0.04, 0.04),
    stringsAsFactors = FALSE
  )

  fit <- pem_fit_block(data, min_samples = 3L, min_cr2_clusters = 4L)
  testthat::expect_identical(fit$status, "descriptive_only")
  testthat::expect_identical(fit$synthesis_scope, "individual_effects_only")
  testthat::expect_null(fit$model)
})

testthat::test_that("P1v2 block interpretation labels are explicit", {
  raw_urpe <- data.frame(
    pooling_block = rep("LOGOR_RAW_URPE_POINT", 3),
    report_id = rep("R1", 3),
    sample_id = paste0("S", 1:3)
  )
  paired_gz <- data.frame(
    pooling_block = rep("PAIRED_GZ", 3),
    report_id = paste0("R", 1:3),
    sample_id = paste0("S", 1:3)
  )

  raw_label <- pem_block_interpretation(raw_urpe, "model_fitted")
  paired_label <- pem_block_interpretation(paired_gz, "model_fitted")

  testthat::expect_identical(raw_label$synthesis_scope, "within_report")
  testthat::expect_identical(raw_label$evidence_status, "within_report_only")
  testthat::expect_true(grepl("experiments", raw_label$note))
  testthat::expect_identical(paired_label$evidence_status, "exploratory")
})

testthat::test_that("nonlinear k=3 remains descriptive under the k=5 rule", {
  data <- data.frame(
    stream = "nonlinear",
    pooling_block = "NONLINEAR_QUADRATIC",
    report_id = paste0("R", 1:3),
    sample_id = paste0("S", 1:3),
    effect_id_analysis = paste0("E", 1:3),
    yi_analysis = c(-0.10, 0.04, 0.08),
    vi_analysis = c(0.01, 0.01, 0.02),
    stringsAsFactors = FALSE
  )

  fit <- pem_fit_block(data, min_samples = 5L, min_cr2_clusters = 4L)
  testthat::expect_identical(fit$status, "descriptive_only")
  testthat::expect_null(fit$model)
})
