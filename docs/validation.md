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
- P3 input/output path precedence and two-row workbook-header handling;
- P3 REML/Hartung-Knapp, known-V, CR2, A008, replacement, and compatibility
  rules using synthetic data only.

No research workbook or pooled result is included in the CI job. Parsing
Parsing `analysis/P2_analysis_v1.R` and `analysis/P2_analysis_v1.1.R` checks
syntax only; the private P2 workbook and retained v1 run are required for input
validation, model execution, and numeric comparison. Private inputs and run
artifacts are never added to CI or Git.

CI also parses `analysis/P3_analysis_v1.0.R` and runs its explicit synthetic
smoke test. The private v3.1 workbook is not available to CI, so these checks do
not constitute a real P3 run or a result freeze.
