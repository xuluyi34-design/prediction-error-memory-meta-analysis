p3_test_root <- function() {
  current <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(current, "analysis", "P3_analysis_v1.0.R"))) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) stop("Could not locate P3 repository root.")
    current <- parent
  }
}

testthat::test_that("P3 paths honor explicit, environment, private, and compatibility precedence", {
  root <- tempfile("p3-paths-")
  dir.create(file.path(root, "analysis"), recursive = TRUE)
  dir.create(file.path(root, "data", "private"), recursive = TRUE)
  cli <- file.path(root, "cli.xlsx")
  env <- file.path(root, "env.xlsx")
  private <- file.path(root, "data", "private", P3_INPUT_FILENAME)
  compatibility <- file.path(root, "analysis", P3_INPUT_FILENAME)
  invisible(vapply(c(cli, env, private, compatibility), file.create, logical(1)))

  old_input <- Sys.getenv("META_ANALYSIS_INPUT", unset = NA_character_)
  old_output <- Sys.getenv("META_ANALYSIS_OUTPUT_DIR", unset = NA_character_)
  on.exit({
    if (is.na(old_input)) Sys.unsetenv("META_ANALYSIS_INPUT") else Sys.setenv(META_ANALYSIS_INPUT = old_input)
    if (is.na(old_output)) Sys.unsetenv("META_ANALYSIS_OUTPUT_DIR") else Sys.setenv(META_ANALYSIS_OUTPUT_DIR = old_output)
  }, add = TRUE)

  Sys.setenv(META_ANALYSIS_INPUT = env)
  exact_output <- file.path(root, "runP3v1_test")
  cli_paths <- p3_resolve_paths(
    c(paste0("--input=", cli), paste0("--output-dir=", exact_output)),
    project_root = root,
    script_dir = file.path(root, "analysis"),
    timestamp = "20260714_000000"
  )
  testthat::expect_identical(cli_paths$input_path, normalizePath(cli, winslash = "/"))
  testthat::expect_identical(cli_paths$input_origin, "--input")
  testthat::expect_identical(cli_paths$run_dir, normalizePath(exact_output, winslash = "/", mustWork = FALSE))

  env_paths <- p3_resolve_paths(
    character(),
    project_root = root,
    script_dir = file.path(root, "analysis"),
    timestamp = "20260714_000000"
  )
  testthat::expect_identical(env_paths$input_path, normalizePath(env, winslash = "/"))
  testthat::expect_identical(env_paths$input_origin, "META_ANALYSIS_INPUT")

  Sys.unsetenv("META_ANALYSIS_INPUT")
  private_paths <- p3_resolve_paths(
    character(),
    project_root = root,
    script_dir = file.path(root, "analysis"),
    timestamp = "20260714_000000"
  )
  testthat::expect_identical(private_paths$input_path, normalizePath(private, winslash = "/"))
  testthat::expect_identical(private_paths$input_origin, "data/private fallback")
})

testthat::test_that("P3 workbook reader locks the two metadata rows", {
  reader_text <- paste(deparse(body(p3_read_workbook), width.cutoff = 500L), collapse = "\n")
  testthat::expect_true(grepl("skip = 2", reader_text, fixed = TRUE))
})

testthat::test_that("P3 synthetic smoke test covers HK, known V, CR2, A008, and replacements", {
  testthat::expect_true(p3_smoke_test())
})

testthat::test_that("P3 never computes missing precision or installs packages", {
  root <- p3_test_root()
  source_text <- paste(
    readLines(file.path(root, "R", "09_p3_analysis.R"), warn = FALSE, encoding = "UTF-8"),
    readLines(file.path(root, "analysis", "P3_analysis_v1.0.R"), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  testthat::expect_false(grepl("install.packages", source_text, fixed = TRUE))
  testthat::expect_true(grepl("No zero-covariance substitution was made", source_text, fixed = TRUE))
  testthat::expect_true(grepl("no imputation was used", source_text, fixed = TRUE))
})

testthat::test_that("P3 replacement sensitivity cannot append an independent sample", {
  primary <- p3_synthetic_effects(
    c("E1", "E2"), c("C1", "C2"), "PRIMARY", c(0.1, 0.2), c(0.04, 0.04)
  )
  replacement <- primary[1, , drop = FALSE]
  replacement$effect_id <- "E1_ALT"
  replacement$replacement_for <- "E1"
  replacement$dependency_cluster <- "C3"
  testthat::expect_error(
    p3_build_replacement_dataset(primary, replacement),
    "must not append a new k"
  )
})

testthat::test_that("P3 compatibility gate rejects mixed scales and encodings", {
  d <- p3_synthetic_effects(
    c("E1", "E2"), c("C1", "C2"), "MIXED", c(0.1, 0.2), c(0.04, 0.04)
  )
  spec <- p3_synthetic_spec("MIXED")
  d$effect_scale[[2]] <- "SMD"
  testthat::expect_false(p3_check_compatibility(d, spec)$ok)
  d$effect_scale[[2]] <- "logOR"
  d$pe_encoding[[2]] <- "different predictor scaling"
  testthat::expect_false(p3_check_compatibility(d, spec)$ok)
})

testthat::test_that("P3 A008 model requires five effects and preserves order", {
  d <- p3_synthetic_effects(
    paste0("A", 1:4), c("C1", "C1", "C2", "C2"), "A008_BAD",
    c(0.1, 0.2, 0.0, 0.1), rep(0.04, 4)
  )
  V <- diag(d$vi)
  covariance <- p3_full_covariance_long(d$effect_id, V, "A008_BAD")
  testthat::expect_error(
    p3_build_sampling_v(d, covariance, "A008_BAD", require_complete = TRUE, require_five = TRUE),
    "exactly five"
  )
})

p3_qc_fixture <- function() {
  effects <- do.call(rbind, lapply(seq_along(P3_ANALYTIC_SHEETS), function(i) {
    d <- p3_synthetic_effects(
      paste0("QC_E", i), paste0("QC_C", i), paste0("QC_M", i),
      yi = 0.01 * i, vi = 0.04
    )
    d$source_sheet <- P3_ANALYTIC_SHEETS[[i]]
    d$row_text <- paste("Synthetic QC", P3_ANALYTIC_SHEETS[[i]])
    d
  }))
  s041_row <- which(effects$source_sheet == "Main_Nonlinear_v3_1")[[1]]
  effects$study_id[[s041_row]] <- "S041"
  effects$dependency_cluster[[s041_row]] <- "S041_RECRUITMENT"
  effects$row_text[[s041_row]] <- "S041 Paradigm 1 primary representative"

  s040_row <- which(effects$source_sheet == "Main_LogOR_v3_1")[[1]]
  effects$study_id[[s040_row]] <- "S040"
  effects$row_text[[s040_row]] <- "S040 raw paired behavioral primary; adjusted model not reproducible limitation"

  s023_row <- which(effects$source_sheet == "Main_SMD_v3_1")[[1]]
  effects$study_id[[s023_row]] <- "S023"
  effects$row_text[[s023_row]] <- "S023 Experiment 1 recollection primary outcome"

  s016 <- p3_synthetic_effects(
    paste0("QC_S016_E", 1:4), paste0("QC_S016_C", 1:4), paste0("QC_S016_M", 1:4),
    yi = c(0.10, 0.12, 0.08, 0.11), vi = rep(0.04, 4)
  )
  s016$study_id <- "S016"
  s016$source_sheet <- "Main_Nonlinear_v3_1"
  s016$effect_scale <- "nonlinear coefficient"
  s016$estimand <- "quadratic term"
  s016$row_text <- paste("S016 Experiment", 1:4, "quadratic primary")
  effects <- rbind(effects, s016)

  manifest <- do.call(rbind, lapply(seq_len(nrow(effects)), function(i) {
    spec <- p3_synthetic_spec(
      effects$model_id[[i]],
      scale = effects$effect_scale[[i]],
      estimand = effects$estimand[[i]]
    )
    spec$source_sheet <- effects$source_sheet[[i]]
    spec
  }))

  rescue <- data.frame(
    record_id = paste0("RES", 1:21),
    record_type = c(rep("NEW_ATOMIC", 17), rep("EXISTING_COMPONENT", 4)),
    analysis_role = c(rep("PRIMARY", 9), rep("SENSITIVITY", 4), rep("COMPONENT_ONLY", 8)),
    analysis_include = c(rep("Yes", 13), rep("No", 8)),
    dependency_cluster = c(paste0("RESCUE_C", 1:13), rep("", 8)),
    stringsAsFactors = FALSE
  )
  quarantine <- data.frame(
    effect_id = c("S023_Q1", "S023_Q2"),
    study_id = "S023",
    experiment = "Experiment 3",
    stringsAsFactors = FALSE
  )
  tables <- list(
    QC_Summary_v3_1 = data.frame(check = c("a", "b"), status = "PASS"),
    Raw_Effects_v3_1 = data.frame(effect_id = paste0("RAW", 1:56)),
    Repository_Rescue_Lock_v3_1 = rescue,
    Quarantined_Effects_v3_1 = quarantine,
    Direction_Audit_v3_1 = data.frame(effect_id = effects$effect_id, status = "LOCKED"),
    Effect_Decision_Lock_v3_1 = data.frame(effect_id = effects$effect_id, analysis_include = "Yes")
  )
  attr(tables, "rescue_sheet") <- "Repository_Rescue_Lock_v3_1"
  list(tables = tables, effects = effects, manifest = manifest)
}

testthat::test_that("P3 hard QC accepts the synthetic contract and rejects S023 Experiment 3 leakage", {
  fixture <- p3_qc_fixture()
  qc <- p3_validate_input_qc(fixture$tables, fixture$effects, fixture$manifest)
  testthat::expect_true(all(qc$status == "PASS"), info = paste(qc$check[qc$status != "PASS"], collapse = ", "))

  leaked <- fixture$effects
  leaked$row_text[[1]] <- "S023 Experiment 3 quarantined effect"
  leaked$study_id[[1]] <- "S023"
  qc_leaked <- p3_validate_input_qc(fixture$tables, leaked, fixture$manifest)
  target <- qc_leaked[qc_leaked$check == "S023 Experiment 3 quarantine", , drop = FALSE]
  testthat::expect_identical(target$status[[1]], "FAIL")
})

testthat::test_that("S016 joint data retains four experiments and recorded L-Q covariance", {
  s016 <- data.frame(
    effect_id = paste0("S016_E", 1:4),
    study_id = "S016",
    dependency_cluster = paste0("S016_C", 1:4),
    analysis_include = "Yes",
    beta_linear = c(0.10, 0.08, -0.02, 0.04),
    se_linear = rep(0.20, 4),
    vi_linear = rep(0.04, 4),
    beta_quadratic = c(0.20, 0.15, 0.11, 0.18),
    se_quadratic = rep(0.30, 4),
    vi_quadratic = rep(0.09, 4),
    cov_linear_quadratic = c(0.01, 0.012, 0.008, 0.011),
    notes = "S016 synthetic",
    stringsAsFactors = FALSE
  )
  joint <- p3_build_s016_joint_data(s016)
  testthat::expect_equal(nrow(joint$data), 8L)
  testthat::expect_equal(length(unique(joint$data$dependency_cluster)), 4L)
  testthat::expect_equal(joint$V[1, 2], 0.01)
  testthat::expect_equal(joint$V[3, 4], 0.012)

  s016$cov_linear_quadratic[[2]] <- NA_real_
  testthat::expect_error(p3_build_s016_joint_data(s016), "no value was imputed")
})
