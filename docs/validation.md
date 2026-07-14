# Validation

This branch is used to run the repository's automated R checks after the initial public-repository setup.

The workflow installs the declared R dependencies, parses both repository P2
entry points, and runs all tests under `tests/testthat/`. The tests cover:

- frozen-stream and one-primary-effect-per-sample guards;
- endpoint selection for prespecified logOR blocks;
- separation of logOR, SMD, and nonlinear streams;
- sampling covariance construction;
- estimability-aware random-effects structure;
- moderator, equivalence, and risk-of-bias safeguards.
- preservation of the v1 model-fitting functions in `P2_analysis_v1.1.R`;
- the three locked A020 v2.1 dependency clusters and CR2 applicability rule;
- the required nine-field, `1e-12` numeric-comparison contract.

No research workbook or pooled result is included in the CI job. Parsing
Parsing `analysis/P2_analysis_v1.R` and `analysis/P2_analysis_v1.1.R` checks
syntax only; the private P2 workbook and retained v1 run are required for input
validation, model execution, and numeric comparison. Private inputs and run
artifacts are never added to CI or Git.
