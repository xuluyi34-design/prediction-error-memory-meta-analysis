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
