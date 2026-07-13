# Validation

This branch is used to run the repository's automated R checks after the initial public-repository setup.

The workflow installs the declared R dependencies, parses the repository P2
entry point, and runs all tests under `tests/testthat/`. The tests cover:

- frozen-stream and one-primary-effect-per-sample guards;
- endpoint selection for prespecified logOR blocks;
- separation of logOR, SMD, and nonlinear streams;
- sampling covariance construction;
- estimability-aware random-effects structure;
- moderator, equivalence, and risk-of-bias safeguards.

No research workbook or pooled result is included in the CI job. Parsing
`analysis/P2_analysis_v1.R` checks syntax only; the private P2 workbook is
required for input validation and model execution.
