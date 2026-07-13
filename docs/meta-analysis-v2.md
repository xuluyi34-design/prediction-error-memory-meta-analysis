# Meta-analysis v2: locked P1v2 + P2 workflow

This repository workflow analyzes the locked quantitative inputs for the prediction-error/surprise and memory review.

## Data location

The input data are maintained as a private native Google Sheet and are not stored in Git:

[Meta_Analysis_Input_v2](https://docs.google.com/spreadsheets/d/150k4bvDpfSBX48eoUpLgDxIAZi0N3ZuGwTLxoZOo52Y/edit)

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
- CR2 is allowed only with at least four independent clusters.
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
- `diagnostic_decisions.csv`
- `publication_bias_decision_log.csv`
- model-specific forest plots

If validation fails, the script stops before fitting models and reports the affected sheet, effect ID, and field.

## Current verification status

Before repository handoff, the workbook passed structural checks:

- 64 unique locked candidate effects;
- expected decision-status counts;
- no excluded duplicate marked as included;
- finite positive `yi`, `sei`, and `vi` for every included model row;
- `vi` agrees with `sei²` within tolerance;
- all 26 model blocks match the expected k in `Analysis_Manifest`;
- the A008 covariance matrix is complete and symmetric.

The source workspace did not contain an R runtime, so the statistical models were not executed there. The GitHub workspace must report syntax checks and full-run status separately and must not describe an unrun model as completed.
