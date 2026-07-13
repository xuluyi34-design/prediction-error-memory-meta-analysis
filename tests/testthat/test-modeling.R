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

testthat::test_that("one-effect-per-sample structure is collapsed transparently", {
  data <- data.frame(
    report_id = c("R1", "R2"),
    sample_id = c("S1", "S2"),
    effect_id_analysis = c("E1", "E2")
  )

  structure <- pem_random_structure(data)
  testthat::expect_true("sample_id" %in% structure$labels)
  testthat::expect_false("sample_id/effect_id_analysis" %in% structure$labels)
  testthat::expect_true(any(grepl("algebraically redundant", structure$notes)))
})

testthat::test_that("an eligible synthetic block returns a model result", {
  data <- data.frame(
    stream = "logOR",
    pooling_block = "SYNTHETIC_BLOCK",
    report_id = rep(paste0("R", 1:4), each = 2),
    sample_id = paste0("S", 1:8),
    effect_id_analysis = paste0("E", 1:8),
    yi_analysis = c(0.10, 0.15, 0.05, 0.20, 0.12, 0.18, 0.08, 0.14),
    vi_analysis = rep(0.04, 8),
    stringsAsFactors = FALSE
  )

  fit <- pem_fit_block(data, rho = 0.50, min_samples = 3L, min_cr2_samples = 4L)
  summary <- pem_model_summary_row(fit)

  testthat::expect_identical(fit$status, "model_fitted")
  testthat::expect_true(is.finite(summary$estimate))
})
