testthat::test_that("descriptive forest plots do not pass model-only arguments", {
  fit <- list(
    stream = "SMD",
    block = "SYNTHETIC_DESCRIPTIVE",
    model = NULL,
    data = data.frame(
      first_author = c("Alpha", "Beta"),
      year = c(2020, 2021),
      sample_id = c("S1", "S2"),
      yi_analysis = c(0.10, 0.20),
      vi_analysis = c(0.01, 0.02),
      stringsAsFactors = FALSE
    )
  )
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path), add = TRUE)

  testthat::expect_no_warning(pem_plot_forest(fit, path))
  testthat::expect_true(file.exists(path))
  testthat::expect_gt(file.info(path)$size, 0)
})

testthat::test_that("descriptive blocks export individual confidence intervals", {
  fit <- list(
    stream = "logOR",
    block = "SYNTHETIC_DESCRIPTIVE",
    status = "descriptive_only",
    synthesis_scope = "individual_effects_only",
    evidence_status = "descriptive",
    notes = "No pooling.",
    data = data.frame(
      analysis_id = c("A1", "A2"),
      study_id = c("ST1", "ST2"),
      sample_id = c("S1", "S2"),
      report_id = c("R1", "R2"),
      effect_id_analysis = c("E1", "E2"),
      yi_analysis = c(0.10, 0.20),
      vi_analysis = c(0.01, 0.04),
      stringsAsFactors = FALSE
    )
  )

  result <- pem_descriptive_effects(list(fit))

  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_true(all(is.finite(result$ci_lb)))
  testthat::expect_true(all(result$estimate_display > 1))
})
