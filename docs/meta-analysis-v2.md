# Meta-analysis v2/v2.1: locked P1v2 + interim P2 QC workflow

This repository workflow analyzes the locked quantitative inputs for the prediction-error/surprise and memory review.

## Interim P2 v1.1 QC checkpoint

`analysis/P2_analysis_v1.1.R` is a metadata-only QC rerun derived from the
unchanged `analysis/P2_analysis_v1.R` baseline. It reads the private
[`Meta_Analysis_Input_v2.1`](https://docs.google.com/spreadsheets/d/1BaWXC1wWLC_6kteg272okrTI1ZHAF0CTB76RXYWj8KE/edit)
workbook and locks the following A020 dependency clusters:

- `LOGOR_006` → `A020_CHILD`
- `LOGOR_007` → `A020_YOUNG`
- `LOGOR_008` → `A020_OLDER`

The script asserts that the three clusters are distinct and that their `yi`,
`sei`, and `vi` values pass through unchanged. It retains all 26 model
definitions and compares nine numeric fields for every model against
`runP2v1_20260714_093232` at an absolute tolerance of `1e-12`.

This is an interim QC checkpoint, not the final meta-analysis freeze. The
report universe remains 51 pending the decision on a second
literature-inclusion wave.

## Data location

The baseline v2 and interim v2.1 inputs are maintained as private native Google
Sheets and are not stored in Git:

[Meta_Analysis_Input_v2](https://docs.google.com/spreadsheets/d/150k4bvDpfSBX48eoUpLgDxIAZi0N3ZuGwTLxoZOo52Y/edit)

[Meta_Analysis_Input_v2.1](https://docs.google.com/spreadsheets/d/1BaWXC1wWLC_6kteg272okrTI1ZHAF0CTB76RXYWj8KE/edit)

To run the analysis, export the sheet as `Meta_Analysis_Input_v2.xlsx` and place it in a local ignored directory such as:

```text
data/private/Meta_Analysis_Input_v2.xlsx
```

Do not commit the exported workbook, source PDFs, credentials, or generated model output.

## Locked evidence structure

The report universe is fixed at 51 reports. The workbook freezes 64 quantitative candidate-effect decisions:

| Decision status | Count |
| --- | ---: |
| `LOCKED_PRIMARY` | 21 |
| `LOCKED_SENSITIVITY` | 21 |
| `LOCKED_ROBUSTNESS` | 5 |
| `LOCKED_DESCRIPTIVE` | 12 |
| `LOCKED_EXCLUDE_DUPLICATE` | 1 |
| `PENDING_PRECISION` | 4 |

Important dependence decisions:

- A026/P2L010 reuses A011 samples and is not a new independent effect.
- A021 and A042 share N=42; A042 is the behavioral anchor and A021 provides mechanistic evidence.
- A008 contains five shared-control logOR effects and uses the explicit 5×5 sampling covariance matrix in `V_Matrix_A008`.
- A008 has only two independent shared-control blocks, so CR2 is not estimable under the locked four-cluster rule.

## Analysis rules

- Compatible independent-effect blocks use REML random effects with Hartung–Knapp inference.
- Single-effect blocks are descriptive and are not labeled pooled estimates.
- CR2 is evaluated only for blocks where a `dependency_cluster` contributes
  multiple effects. It is run with `rma.mv` REML and Satterthwaite inference
  only when at least four independent clusters are available; otherwise the
  exact not-applicable or skipped reason is recorded.
- Influence and leave-one-out diagnostics require a compatible `rma.uni` model with k ≥ 4.
- Publication-bias or Egger testing requires a compatible block with k ≥ 10.
- logOR, Hedges g_z, g_av, Gaussian coefficients, nonlinear coefficients, and Bayesian MPT parameters remain separate.
- Linear and quadratic coefficients from one polynomial model are not treated as independent effects.
- The input workbook is read-only.

## Required R packages

```r
install.packages(c(
  "readxl", "dplyr", "tibble", "purrr",
  "stringr", "metafor", "clubSandwich"
))
```

The script does not install packages automatically. It stops with an exact installation command if dependencies are missing.

## Running the analysis

For the v2.1 QC rerun, export the private sheet as
`data/private/Meta_Analysis_Input_v2.1.xlsx`, retain the unmodified baseline run
directory locally, and run:

```bash
Rscript analysis/P2_analysis_v1.1.R \
  --input=data/private/Meta_Analysis_Input_v2.1.xlsx \
  --baseline-run=results/runP2v1_20260714_093232 \
  --output-dir=results
```

The v1.1 runner creates `runP2v1.1_YYYYMMDD_HHMMSS` and includes a
machine-readable `P2_v1_vs_v1.1_numeric_comparison.csv`, a final validation
log, `README_run.txt`, and top-level copies of the required handoff artifacts.
The workbook and complete run directory remain ignored and must not be
committed.

For the retained v2 baseline runner:

The repository-integrated script supports an explicit private input path. For example:

```bash
Rscript analysis/P2_analysis_v1.R \
  --input=data/private/Meta_Analysis_Input_v2.xlsx \
  --output-dir=results
```

Input resolution follows this order:

1. `--input=`;
2. the `META_ANALYSIS_INPUT` environment variable;
3. `data/private/Meta_Analysis_Input_v2.xlsx`;
4. a compatibility copy beside `P2_analysis_v1.R`.

The output root can be set with `--output-dir=` or
`META_ANALYSIS_OUTPUT_DIR`. Without either, repository runs are written under
`results/`. The script only reads the input workbook and never downloads it
from Google Drive or writes back to it.

The script creates a timestamped `runP2v1_YYYYMMDD_HHMMSS` directory containing:

```text
tables/
figures/
models/
logs/
RUN_NOTE.txt
```

Core outputs include:

- `analysis_validation.csv`
- `effect_selection_audit.csv`
- `manifest_k_check.csv`
- `included_effects_long.csv`
- `model_summary.csv`
- `CR2_decision_log.csv`
- `CR2_results.csv`
- `diagnostic_decisions.csv`
- `publication_bias_decision_log.csv`
- model-specific forest plots

If validation fails, the script stops before fitting models and reports the affected sheet, effect ID, and field.
`CR2_results.csv` always has a stable header. It is empty when no model block is
eligible, while `CR2_decision_log.csv` preserves the decision and rationale for
every requested model.

## Current verification status

Before repository handoff, the workbook passed structural checks:

- 64 unique locked candidate effects;
- expected decision-status counts;
- no excluded duplicate marked as included;
- finite positive `yi`, `sei`, and `vi` for every included model row;
- `vi` agrees with `sei²` within tolerance;
- all 26 model blocks match the expected k in `Analysis_Manifest`;
- the A008 covariance matrix is complete and symmetric.

Private v2.1 execution and PASS/FAIL status are reported with the external run
handoff, not committed here. The private workbook, generated run directory,
figures, model objects, PDFs, numeric comparison, and result ZIP are not stored
in Git.
