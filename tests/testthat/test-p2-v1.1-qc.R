locate_p2_script <- function(filename) {
  current <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  repeat {
    script_path <- file.path(current, "analysis", filename)
    if (file.exists(script_path)) return(script_path)
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not locate analysis/", filename, " from the test directory.")
    }
    current <- parent
  }
}

extract_assignment_expression <- function(script_path, name) {
  expressions <- parse(file = script_path)
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
  expressions[[which(matches)]]
}

testthat::test_that("P2 v1.1 retains the locked model implementations", {
  baseline_path <- locate_p2_script("P2_analysis_v1.R")
  qc_path <- locate_p2_script("P2_analysis_v1.1.R")
  locked_functions <- c(
    "single_effect_summary",
    "fit_hk_block",
    "fit_known_v_block",
    "fit_mv_cr2_if_eligible"
  )

  for (function_name in locked_functions) {
    baseline_expression <- extract_assignment_expression(baseline_path, function_name)
    qc_expression <- extract_assignment_expression(qc_path, function_name)
    testthat::expect_identical(
      deparse(qc_expression, width.cutoff = 500L),
      deparse(baseline_expression, width.cutoff = 500L),
      info = function_name
    )
  }
})

testthat::test_that("P2 v1.1 locks the A020 metadata-only correction", {
  qc_path <- locate_p2_script("P2_analysis_v1.1.R")
  script_text <- paste(readLines(qc_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  required_literals <- c(
    'input_version <- "Meta_Analysis_Input_v2.1"',
    'analysis_version <- "P2_analysis_v1.1"',
    'paste0("runP2v1.1_", timestamp)',
    'LOGOR_006 = "A020_CHILD"',
    'LOGOR_007 = "A020_YOUNG"',
    'LOGOR_008 = "A020_OLDER"',
    'numeric_tolerance <- 1e-12',
    '"estimate", "se", "ci_lb", "ci_ub", "p_value", "tau2", "I2", "Q", "Q_p"',
    '"This is an interim QC checkpoint, not the final meta-analysis freeze."',
    '"The report universe remains 51 pending the decision on a second literature-inclusion wave."'
  )
  for (literal in required_literals) {
    testthat::expect_true(grepl(literal, script_text, fixed = TRUE), info = literal)
  }

  testthat::expect_true(grepl(
    "yi, sei, and vi are read directly and remain byte-for-byte identical",
    script_text,
    fixed = TRUE
  ))
  testthat::expect_true(grepl(
    "每个 cluster 仅一条效应，标准 Hartung–Knapp 推断已适用",
    script_text,
    fixed = TRUE
  ))
})

testthat::test_that("three independent A020 clusters make CR2 not applicable", {
  qc_path <- locate_p2_script("P2_analysis_v1.1.R")
  expression <- extract_assignment_expression(qc_path, "fit_mv_cr2_if_eligible")
  environment <- new.env(parent = baseenv())
  eval(expression, envir = environment)

  d <- data.frame(
    effect_id = c("LOGOR_006", "LOGOR_007", "LOGOR_008"),
    dependency_cluster = c("A020_CHILD", "A020_YOUNG", "A020_OLDER"),
    yi = c(0.10, 0.20, 0.30),
    vi = c(0.04, 0.05, 0.06),
    stringsAsFactors = FALSE
  )
  result <- environment$fit_mv_cr2_if_eligible(d, "MAIN_LOGOR_ORDERED_ENDPOINT")

  testthat::expect_identical(result$decision$k, 3L)
  testthat::expect_identical(result$decision$independent_clusters, 3L)
  testthat::expect_identical(result$decision$repeated_effect_clusters, 0L)
  testthat::expect_identical(result$decision$decision, "NOT_APPLICABLE")
  testthat::expect_null(result$fit)
})
