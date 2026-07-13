extract_p2_function <- function(name) {
  expressions <- parse(file = file.path("analysis", "P2_analysis_v1.R"))
  matches <- vapply(
    expressions,
    function(expr) {
      is.call(expr) &&
        identical(expr[[1]], as.name("<-")) &&
        identical(as.character(expr[[2]]), name)
    },
    logical(1)
  )
  testthat::expect_equal(sum(matches), 1L)
  environment <- new.env(parent = baseenv())
  eval(expressions[[which(matches)]], envir = environment)
  environment[[name]]
}

testthat::test_that("P2 CR2 is limited to dependent blocks with four clusters", {
  fit_cr2 <- extract_p2_function("fit_mv_cr2_if_eligible")

  independent <- data.frame(
    effect_id = paste0("E", 1:4),
    dependency_cluster = paste0("S", 1:4),
    yi = c(0.10, 0.20, -0.05, 0.15),
    vi = c(0.04, 0.05, 0.03, 0.04),
    stringsAsFactors = FALSE
  )
  independent_result <- fit_cr2(independent, "INDEPENDENT")
  testthat::expect_identical(
    independent_result$decision$decision,
    "NOT_APPLICABLE"
  )
  testthat::expect_null(independent_result$fit)

  too_few <- rbind(
    transform(independent[1, , drop = FALSE], effect_id = "E1b"),
    independent[1:3, , drop = FALSE]
  )
  too_few_result <- fit_cr2(too_few, "TOO_FEW")
  testthat::expect_identical(too_few_result$decision$decision, "SKIPPED")
  testthat::expect_identical(
    too_few_result$decision$independent_clusters,
    3L
  )
  testthat::expect_null(too_few_result$fit)

  eligible <- data.frame(
    effect_id = paste0("E", 1:8),
    dependency_cluster = rep(paste0("S", 1:4), each = 2),
    yi = c(0.10, 0.18, -0.02, 0.07, 0.22, 0.14, -0.08, 0.04),
    vi = c(0.04, 0.05, 0.03, 0.04, 0.05, 0.03, 0.04, 0.05),
    stringsAsFactors = FALSE
  )
  eligible_result <- fit_cr2(eligible, "ELIGIBLE")
  testthat::expect_identical(eligible_result$decision$decision, "RUN")
  testthat::expect_identical(
    eligible_result$decision$independent_clusters,
    4L
  )
  testthat::expect_s3_class(eligible_result$fit, "rma.mv")
  testthat::expect_equal(nrow(eligible_result$results), 1L)
  testthat::expect_true(all(is.finite(eligible_result$results$estimate)))
  testthat::expect_true(all(is.finite(eligible_result$results$se)))
  testthat::expect_true(all(is.finite(eligible_result$results$df_satterthwaite)))
})
