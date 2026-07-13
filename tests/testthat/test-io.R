testthat::test_that("endpoint-standardized logOR blocks use endpoint yi and vi", {
  fixture <- pem_test_fixture()
  logor <- fixture$prepared$logor

  testthat::expect_true(logor$endpoint_used[[1]])
  testthat::expect_equal(logor$yi_analysis[[1]], 0.40)
  testthat::expect_equal(logor$vi_analysis[[1]], 0.04)
  testthat::expect_equal(logor$report_id[[1]], "RPT001")
})

testthat::test_that("analysis streams retain distinct labels and pooling blocks", {
  fixture <- pem_test_fixture()

  testthat::expect_identical(fixture$prepared$logor$stream[[1]], "logOR")
  testthat::expect_identical(fixture$prepared$smd$stream[[1]], "SMD")
  testthat::expect_identical(
    fixture$prepared$nonlinear$pooling_block[[1]],
    "NONLINEAR_QUADRATIC"
  )
})

