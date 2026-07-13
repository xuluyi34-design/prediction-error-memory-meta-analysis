testthat::test_that("a valid frozen fixture passes fatal audit checks", {
  fixture <- pem_test_fixture()
  audit <- pem_audit_inputs(
    fixture$raw,
    fixture$prepared,
    fixture$config,
    strict_freeze = TRUE
  )

  testthat::expect_true(audit$passed)
  testthat::expect_equal(
    audit$summary$value[audit$summary$item == "total_rows"],
    3
  )
})

testthat::test_that("duplicate frozen Sample_IDs are rejected", {
  fixture <- pem_test_fixture()
  fixture$raw$smd$sample_id <- "SMP001"
  fixture$raw$smd$study_id <- "ST001"
  fixture$prepared <- pem_prepare_inputs(fixture$raw, fixture$config)

  audit <- pem_audit_inputs(
    fixture$raw,
    fixture$prepared,
    fixture$config,
    strict_freeze = TRUE
  )

  testthat::expect_false(audit$passed)
  testthat::expect_true(any(audit$issues$check == "one_primary_effect_per_sample"))
})

testthat::test_that("variance values inconsistent with SE squared are rejected", {
  fixture <- pem_test_fixture()
  fixture$raw$smd$vi <- 0.40
  fixture$prepared <- pem_prepare_inputs(fixture$raw, fixture$config)

  audit <- pem_audit_inputs(
    fixture$raw,
    fixture$prepared,
    fixture$config,
    strict_freeze = TRUE
  )

  testthat::expect_false(audit$passed)
  testthat::expect_true(any(audit$issues$check == "vi_equals_se_squared"))
})

testthat::test_that("strict run requires the P1v2 workbook filename", {
  fixture <- pem_test_fixture()
  audit <- pem_audit_inputs(
    fixture$raw,
    fixture$prepared,
    fixture$config,
    strict_freeze = TRUE,
    workbook = "P1数据.xlsx"
  )

  testthat::expect_false(audit$passed)
  testthat::expect_true(
    any(audit$issues$check == "frozen_workbook_filename")
  )
})
