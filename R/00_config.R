pem_analysis_config <- function() {
  list(
    project_title = paste(
      "Prediction Error and Human Episodic Memory:",
      "A Systematic Review and Multilevel Meta-analysis Across Memory Outcomes"
    ),
    protocol_version = "1.0",
    protocol_freeze_date = as.Date("2026-07-13"),
    sheets = list(
      sample_map = "Study_Sample_Map",
      risk_of_bias = "Risk_of_Bias",
      logor = "MAI_LogOR_v1",
      smd = "MAI_SMD_v1",
      nonlinear = "MAI_Nonlinear_v1"
    ),
    expected_counts = c(logor = 14L, smd = 5L, nonlinear = 2L),
    expected_total = 21L,
    primary_rho = 0.50,
    rho_grid = c(0.00, 0.30, 0.50, 0.70, 0.90),
    min_quantitative_samples = 3L,
    min_cr2_samples = 4L,
    min_nonlinear_samples = 5L,
    min_publication_bias_samples = 10L,
    equivalence_bounds_g = c(-0.10, 0.10),
    logor_endpoint_blocks = c(
      "LOGOR_ORDERED_ENDPOINT",
      "LOGOR_SIGNED_FULLRANGE"
    ),
    moderator_thresholds_frozen = FALSE,
    required_packages = c(
      "clubSandwich",
      "dplyr",
      "metafor",
      "readxl"
    )
  )
}
