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
  testthat::expect_identical(
    p3_resolve_rescue_sheet(c("README", "Rescue_Resolution_v3_1")),
    "Rescue_Resolution_v3_1"
  )
})

testthat::test_that("P3 expands sheet-level manifests into unique model specifications", {
  policy <- data.frame(
    evidence_block = "Published native logOR",
    input_sheet = "Main_LogOR_v3_1",
    effect_scale = "logOR",
    pooling_block = "model_id specific",
    inference_plan = "REML + Hartung-Knapp",
    guardrail = "Keep predictor units separate",
    stringsAsFactors = FALSE
  )
  inherited <- data.frame(
    evidence_block = "Event/temporal",
    input_sheet = "Event_Temporal_v3",
    effect_scale = "native beta/Hedges g/descriptive",
    pooling_block = "outcome-specific",
    inference_plan = "Existing module rules retained",
    guardrail = "Copied from baseline",
    stringsAsFactors = FALSE
  )
  effects <- rbind(
    p3_synthetic_effects("E1", "C1", "MODEL_A", 0.1, 0.04),
    p3_synthetic_effects("E2", "C2", "MODEL_B", 0.2, 0.04),
    p3_synthetic_effects("E3", "C3", "MODEL_EXCLUDED", 0.3, 0.04)
  )
  effects$source_sheet <- "Main_LogOR_v3_1"
  effects$analysis_include[[3]] <- "No"
  tables <- list(Analysis_Manifest_v3 = inherited, Analysis_Manifest_v3_1 = policy)
  expanded <- p3_combine_manifests(tables, effects)
  testthat::expect_setequal(expanded$model_id, c("MODEL_A", "MODEL_B", "MODEL_EXCLUDED"))
  testthat::expect_true(all(expanded$source_sheet == "Main_LogOR_v3_1"))
  testthat::expect_true(all(expanded$manifest_version == "v3.1"))
  testthat::expect_identical(
    expanded$run_model[expanded$model_id == "MODEL_EXCLUDED"],
    "No"
  )

  empty_policy <- p3_standardize_manifest(policy[0, , drop = FALSE], "v3.1")
  testthat::expect_equal(nrow(empty_policy), 0L)
})

testthat::test_that("MPT records remain descriptive and retain reported credible intervals", {
  mpt <- data.frame(
    mpt_id = "MPT001",
    parameter = "recollection",
    posterior_mean = 0.42,
    CrI_lb = 0.20,
    CrI_ub = 0.61,
    analysis_role = "SEPARATE",
    analysis_include = "Yes",
    stringsAsFactors = FALSE
  )
  d <- p3_standardize_effect_sheet(mpt, "MPT_Separate_v3")
  testthat::expect_identical(d$model_id[[1]], "MPT_DESC__MPT001")
  testthat::expect_true(d$descriptive_only[[1]])
  spec <- p3_synthetic_spec(
    d$model_id[[1]],
    role = "DESCRIPTIVE_MODULE",
    scale = d$effect_scale[[1]],
    estimand = d$estimand[[1]]
  )
  spec$pe_encoding <- d$pe_encoding[[1]]
  spec$outcome <- d$outcome[[1]]
  result <- p3_fit_block(list(spec = spec, data = d), list())$result
  testthat::expect_equal(result$ci_lb[[1]], 0.20)
  testthat::expect_equal(result$ci_ub[[1]], 0.61)
  testthat::expect_true(is.na(result$se[[1]]))
})

testthat::test_that("special inverted-S rows use the recorded linear coefficient slots", {
  nonlinear <- data.frame(
    effect_id = "V3N001",
    model_id = "NONLINEAR_SPECIAL",
    pe_coding = "Best-fitting inverted-S signed aversive PE function",
    beta_linear = 0.27,
    se_linear = 0.10074626865671642,
    vi_linear = 0.010149810648251281,
    beta_quadratic = NA_real_,
    se_quadratic = NA_real_,
    vi_quadratic = NA_real_,
    analysis_include = "Yes",
    stringsAsFactors = FALSE
  )
  d <- p3_standardize_effect_sheet(nonlinear, "Main_Nonlinear_v3_1")
  testthat::expect_equal(d$yi[[1]], nonlinear$beta_linear[[1]])
  testthat::expect_equal(d$sei[[1]], nonlinear$se_linear[[1]])
  testthat::expect_equal(d$vi[[1]], nonlinear$vi_linear[[1]])
  testthat::expect_identical(d$estimand[[1]], "special inverted-S function")
})

testthat::test_that("cumulative lock metadata defines roles and same-sample replacements", {
  effects <- rbind(
    p3_synthetic_effects("PRIMARY_ID", "PRIMARY_CLUSTER", "PRIMARY_MODEL", 0.1, 0.04),
    p3_synthetic_effects("ALT_ID", "SOURCE_ALT_CLUSTER", "ALT_MODEL", 0.2, 0.04)
  )
  effects$source_sheet <- "Module_SMD_v3_1"
  effects$study_id[[1]] <- "ST123"
  effects$row_text[[1]] <- "Primary record for ST123"
  lock <- data.frame(
    source_analysis_id = c("PRIMARY_ID", "ALT_ID"),
    analysis_include = "Yes",
    include_primary = c("Yes", "No"),
    include_sensitivity = c("No", "Yes"),
    include_robustness = "No",
    include_descriptive = "No",
    decision_status = c("LOCKED_PRIMARY", "LOCKED_SENSITIVITY"),
    dependency_cluster = c("PRIMARY_CLUSTER", "SOURCE_ALT_CLUSTER"),
    duplicate_or_alternative_of = c("", "Preferred effect in ST123"),
    direction_rule = "Higher values favor the locked PE direction",
    source_version = "v3.1",
    stringsAsFactors = FALSE
  )
  tables <- list(
    Effect_Decision_Lock_v3_1 = lock,
    Raw_Effects_v3_1 = data.frame(
      effect_id = c("PRIMARY_ID", "ALT_ID"),
      candidate_id = c("S023", "S023"),
      stringsAsFactors = FALSE
    ),
    Direction_Audit_v3_1 = data.frame(
      effect_id = c("PRIMARY_ID", "ALT_ID"),
      candidate_id = c("S023", "S023"),
      audit_status = c("CHECKED", "CHECKED_V3_1"),
      stringsAsFactors = FALSE
    )
  )
  enriched <- p3_enrich_effect_metadata(effects, tables)
  alt <- enriched[enriched$effect_id == "ALT_ID", , drop = FALSE]
  testthat::expect_identical(alt$analysis_role[[1]], "SENSITIVITY")
  testthat::expect_identical(alt$replacement_for[[1]], "PRIMARY_ID")
  testthat::expect_identical(alt$source_replacement_for[[1]], "Preferred effect in ST123")
  testthat::expect_identical(alt$base_model_id[[1]], "PRIMARY_MODEL")
  testthat::expect_identical(alt$dependency_cluster[[1]], "PRIMARY_CLUSTER")
  testthat::expect_identical(alt$source_dependency_cluster[[1]], "SOURCE_ALT_CLUSTER")
  testthat::expect_identical(alt$candidate_id[[1]], "S023")
  testthat::expect_identical(alt$direction_status[[1]], "CHECKED_V3_1")
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

testthat::test_that("frozen P2 compatibility exceptions are model-ID scoped", {
  policy <- data.frame(
    evidence_block = "Frozen P2",
    input_sheet = "Main_LogOR_v3_1",
    effect_scale = "logOR",
    pooling_block = "locked model_id",
    inference_plan = "REML + Hartung-Knapp",
    guardrail = "Frozen grouping",
    stringsAsFactors = FALSE
  )
  inherited <- data.frame(
    evidence_block = "Inherited",
    input_sheet = "Event_Temporal_v3",
    effect_scale = "native",
    pooling_block = "model_id",
    inference_plan = "Existing rules",
    guardrail = "Inherited",
    stringsAsFactors = FALSE
  )
  d <- p3_synthetic_effects(
    c("F1", "F2"), c("FC1", "FC2"), "MAIN_LOGOR_BINARY_CAT",
    c(0.1, 0.2), c(0.04, 0.04)
  )
  d$source_sheet <- "Main_LogOR_v3_1"
  d$outcome <- c("Locked binary memory outcome A", "Locked binary memory outcome B")
  manifest <- p3_combine_manifests(
    list(Analysis_Manifest_v3 = inherited, Analysis_Manifest_v3_1 = policy),
    d
  )
  testthat::expect_identical(manifest$allow_mixed_outcome[[1]], "Yes")
  testthat::expect_true(p3_check_compatibility(d, manifest)$ok)

  d$model_id <- "UNLOCKED_NEW_MODEL"
  d$source_model_id <- "UNLOCKED_NEW_MODEL"
  manifest_new <- p3_combine_manifests(
    list(Analysis_Manifest_v3 = inherited, Analysis_Manifest_v3_1 = policy),
    d
  )
  testthat::expect_identical(manifest_new$allow_mixed_outcome[[1]], "No")
  testthat::expect_false(p3_check_compatibility(d, manifest_new)$ok)
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
  effects$candidate_id[[s041_row]] <- "S041"
  effects$study_id[[s041_row]] <- "ST142"
  effects$dependency_cluster[[s041_row]] <- "S041_RECRUITMENT"
  effects$row_text[[s041_row]] <- "S041 Paradigm 1 primary representative"

  s041_sensitivity <- effects[s041_row, , drop = FALSE]
  s041_sensitivity$effect_id <- "QC_S041_P2"
  s041_sensitivity$source_sheet <- "Module_Nonlinear_v3_1"
  s041_sensitivity$source_model_id <- "QC_S041_SENS"
  s041_sensitivity$model_id <- "QC_S041_SENS"
  s041_sensitivity$base_model_id <- effects$model_id[[s041_row]]
  s041_sensitivity$analysis_role <- "SENSITIVITY"
  s041_sensitivity$replacement_for <- effects$effect_id[[s041_row]]
  s041_sensitivity$row_text <- "S041 Paradigm 2 same-recruitment replacement sensitivity"

  s040_row <- which(effects$source_sheet == "Main_LogOR_v3_1")[[1]]
  effects$candidate_id[[s040_row]] <- "S040"
  effects$study_id[[s040_row]] <- "ST141"
  effects$row_text[[s040_row]] <- "S040 raw paired behavioral primary; adjusted model not reproducible limitation"

  s023_row <- which(effects$source_sheet == "Main_SMD_v3_1")[[1]]
  effects$candidate_id[[s023_row]] <- "S023"
  effects$study_id[[s023_row]] <- "ST111"
  effects$outcome[[s023_row]] <- "recollection"
  effects$dependency_cluster[[s023_row]] <- "S023_Experiment_1_R_F"
  effects$row_text[[s023_row]] <- "S023 Experiment 1 recollection primary outcome"

  s023_exp2 <- effects[s023_row, , drop = FALSE]
  s023_exp2$effect_id <- "QC_S023_EXP2"
  s023_exp2$source_sheet <- "Module_SMD_v3_1"
  s023_exp2$source_model_id <- "QC_S023_EXP2_SENS"
  s023_exp2$model_id <- "QC_S023_EXP2_SENS"
  s023_exp2$base_model_id <- ""
  s023_exp2$analysis_role <- "SENSITIVITY"
  s023_exp2$dependency_cluster <- "S023_Experiment_2_R_F"
  s023_exp2$source_dependency_cluster <- "S023_Experiment_2_R_F"
  s023_exp2$study_id <- "ST112"
  s023_exp2$row_text <- "S023 Experiment 2 recollection sensitivity-only source clarification"

  s010 <- p3_synthetic_effects("QC_S010_N71", "S010_N71", "QC_S010_SENS", 0.1, 0.04)
  s010$candidate_id <- "S010"
  s010$source_sheet <- "Sensitivity_LogOR_v3_1"
  s010$analysis_role <- "SENSITIVITY"
  s010$row_text <- "S010 N=71 public subset sensitivity only; does not replace N=76"

  s021 <- p3_synthetic_effects("QC_S021_N35", "S021_N35", "QC_S021_SENS", 0.1, 0.04)
  s021$candidate_id <- "S021"
  s021$source_sheet <- "Module_SMD_v3_1"
  s021$analysis_role <- "SENSITIVITY"
  s021$row_text <- "S021 young adult N=35 subgroup sensitivity only; not a replacement"

  s016 <- p3_synthetic_effects(
    paste0("QC_S016_E", 1:4), paste0("QC_S016_C", 1:4), paste0("QC_S016_M", 1:4),
    yi = c(0.10, 0.12, 0.08, 0.11), vi = rep(0.04, 4)
  )
  s016$candidate_id <- "S016"
  s016$study_id <- paste0("ST", 94:97)
  s016$source_sheet <- "Main_Nonlinear_v3_1"
  s016$effect_scale <- "nonlinear coefficient"
  s016$estimand <- "quadratic term"
  s016$row_text <- paste("S016 Experiment", 1:4, "quadratic primary")
  effects <- rbind(effects, s041_sensitivity, s023_exp2, s010, s021, s016)

  manifest <- do.call(rbind, lapply(seq_len(nrow(effects)), function(i) {
    spec <- p3_synthetic_spec(
      effects$model_id[[i]],
      scale = effects$effect_scale[[i]],
      estimand = effects$estimand[[i]]
    )
    spec$source_sheet <- effects$source_sheet[[i]]
    spec$analysis_role <- effects$analysis_role[[i]]
    spec$base_model_id <- effects$base_model_id[[i]]
    spec
  }))

  rescue_resolution <- data.frame(
    candidate_id = paste0("S", sprintf("%03d", 1:7)),
    article_id = paste0("A", sprintf("%03d", 1:7)),
    resolution = "RESOLVED",
    stringsAsFactors = FALSE
  )
  raw_v3 <- data.frame(effect_id = paste0("RAW", 1:39), stringsAsFactors = FALSE)
  raw_v31 <- data.frame(
    effect_id = paste0("RAW", 1:56),
    candidate_id = c(rep("", 39), rep(rescue_resolution$candidate_id, length.out = 17)),
    stringsAsFactors = FALSE
  )
  rescue_lock <- data.frame(
    lock_id = paste0("LOCK", 1:21),
    source_version = "v3.1",
    source_analysis_id = c(effects$effect_id, paste0("RES_EXTRA", seq_len(21 - nrow(effects)))),
    article_id = rep(rescue_resolution$article_id, length.out = 21),
    analysis_include = "Yes",
    include_primary = c(rep("Yes", 13), rep("No", 8)),
    include_sensitivity = c(rep("No", 13), rep("Yes", 5), rep("No", 3)),
    dependency_cluster = c(
      paste0("PRIMARY_C", c(1:9, 1:4)),
      paste0("SENS_C", c(1:4, 1)),
      paste0("OTHER_C", 1:3)
    ),
    stringsAsFactors = FALSE
  )
  quarantine <- data.frame(
    effect_id = c("S023_Q1", "S023_Q2"),
    candidate_id = "S023",
    study_id = "ST113",
    experiment = "Experiment 3",
    stringsAsFactors = FALSE
  )
  tables <- list(
    QC_Summary_v3_1 = data.frame(check = c("a", "b"), status = "PASS"),
    Rescue_Resolution_v3_1 = rescue_resolution,
    Raw_Effects_v3 = raw_v3,
    Raw_Effects_v3_1 = raw_v31,
    Quarantined_Effects_v3_1 = quarantine,
    Direction_Audit_v3_1 = data.frame(effect_id = effects$effect_id, audit_status = "CHECKED_V3_1"),
    Effect_Decision_Lock_v3_1 = rescue_lock
  )
  attr(tables, "rescue_sheet") <- "Rescue_Resolution_v3_1"
  list(tables = tables, effects = effects, manifest = manifest)
}

testthat::test_that("P3 hard QC accepts the synthetic contract and rejects S023 Experiment 3 leakage", {
  fixture <- p3_qc_fixture()
  qc <- p3_validate_input_qc(fixture$tables, fixture$effects, fixture$manifest)
  testthat::expect_true(all(qc$status == "PASS"), info = paste(qc$check[qc$status != "PASS"], collapse = ", "))

  leaked <- fixture$effects
  leaked$row_text[[1]] <- "S023 Experiment 3 quarantined effect"
  leaked$candidate_id[[1]] <- "S023"
  leaked$dependency_cluster[[1]] <- "S023_Experiment_3"
  qc_leaked <- p3_validate_input_qc(fixture$tables, leaked, fixture$manifest)
  target <- qc_leaked[qc_leaked$check == "S023 Experiment 3 quarantine", , drop = FALSE]
  testthat::expect_identical(target$status[[1]], "FAIL")

  bad_delta <- fixture
  bad_delta$tables$Raw_Effects_v3$effect_id[[39]] <- "RAW40"
  qc_bad_delta <- p3_validate_input_qc(bad_delta$tables, bad_delta$effects, bad_delta$manifest)
  target_delta <- qc_bad_delta[qc_bad_delta$check == "Repository-rescue atomic count", , drop = FALSE]
  testthat::expect_identical(target_delta$status[[1]], "FAIL")
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
