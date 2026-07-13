testthat::test_that("P1v2 frozen counts are explicit", {
  config <- pem_analysis_config()

  testthat::expect_identical(config$analysis_data_version, "P1v2")
  testthat::expect_identical(config$analysis_method_version, "runP1v2.1")
  testthat::expect_identical(
    config$expected_workbook_filename,
    "P1v2数据.xlsx"
  )
  testthat::expect_identical(
    config$expected_counts,
    c(logor = 14L, smd = 5L, nonlinear = 3L)
  )
  testthat::expect_identical(config$expected_total, 22L)
  testthat::expect_identical(config$min_cr2_clusters, 4L)
})

testthat::test_that("moderator analysis cannot run before thresholds are frozen", {
  testthat::expect_error(
    pem_fit_moderator(),
    "intentionally disabled"
  )
})

testthat::test_that("Hedges conversion refuses missing effective df", {
  testthat::expect_error(
    pem_logor_to_hedges_g(0.20, 0.01, NA_real_),
    "effective df"
  )
})

testthat::test_that("equivalence is evaluated only on a defensible g scale", {
  result <- pem_equivalence_classification(-0.05, 0.06, "logOR")
  testthat::expect_identical(result, "not_evaluated_outside_defensible_g_scale")
})

testthat::test_that("nonfinal risk labels do not trigger a restricted model", {
  fixture <- pem_test_fixture()
  fixture$prepared$logor$overall_risk <- "Unclear"
  result <- pem_risk_of_bias_sensitivity(
    fixture$prepared$logor,
    fixture$config
  )

  testthat::expect_identical(
    result$status[[1]],
    "not_run_incomplete_or_nonfinal_risk_ratings"
  )
})
