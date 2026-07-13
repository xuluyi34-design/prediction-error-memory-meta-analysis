pem_test_fixture <- function() {
  sample_map <- data.frame(
    Study_ID = c("ST001", "ST002", "ST003"),
    Sample_ID_Provisional = c("SMP001", "SMP002", "SMP003"),
    Report_ID = c("RPT001", "RPT002", "RPT003"),
    Module = rep("A_Main_Encoding", 3),
    Memory_Test_Delay = c("24 h", "Immediate", "Immediate"),
    Age_Group = rep("Adults", 3),
    Design = rep("Within-subject", 3),
    Preregistration = rep("Not reported", 3),
    Open_Data_Code = rep("No", 3),
    stringsAsFactors = FALSE
  )
  names(sample_map) <- pem_normalise_names(names(sample_map))

  logor <- data.frame(
    analysis_id = "LOGOR_T01",
    freeze_id = "FZ001",
    study_id = "ST001",
    sample_id = "SMP001",
    first_author = "Alpha",
    year = 2020,
    effect_id = "E001",
    yi = 0.20,
    sei = 0.10,
    vi = 0.01,
    yi_endpoint = 0.40,
    vi_endpoint = 0.04,
    pooling_block = "LOGOR_ORDERED_ENDPOINT",
    pe_sign_type = "unsigned_schema",
    memory_class = "item_recognition",
    stringsAsFactors = FALSE
  )

  smd <- data.frame(
    analysis_id = "SMD_T01",
    freeze_id = "FZ002",
    study_id = "ST002",
    sample_id = "SMP002",
    first_author = "Beta",
    year = 2021,
    effect_id = "E002",
    yi = 0.30,
    sei = 0.20,
    vi = 0.04,
    smd_type = "Hedges g_z",
    pooling_block = "PAIRED_GZ",
    pe_sign_type = "unsigned_magnitude",
    memory_class = "item_recognition",
    stringsAsFactors = FALSE
  )

  nonlinear <- data.frame(
    analysis_id = "NONLIN_T01",
    freeze_id = "FZ003",
    study_id = "ST003",
    sample_id = "SMP003",
    first_author = "Gamma",
    year = 2022,
    memory_outcome = "Recognition",
    effect_id_quadratic = "E003Q",
    beta_quadratic = -0.20,
    sei_quadratic = 0.05,
    vi_quadratic = 0.0025,
    joint_model_status = "Covariance unavailable",
    stringsAsFactors = FALSE
  )

  config <- pem_analysis_config()
  config$expected_counts <- c(logor = 1L, smd = 1L, nonlinear = 1L)
  config$expected_total <- 3L

  raw <- list(
    sample_map = sample_map,
    logor = logor,
    smd = smd,
    nonlinear = nonlinear
  )

  list(
    config = config,
    raw = raw,
    prepared = pem_prepare_inputs(raw, config)
  )
}

