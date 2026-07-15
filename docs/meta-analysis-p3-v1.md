# P3 v3.1 meta-analysis workflow

`analysis/P3_analysis_v1.0.R` is the read-only entry point for the private
`Meta_Analysis_Input_v3.1.xlsx` workbook. P3 extends the frozen P2 evidence
structure without editing `P2_analysis_v1.R`, `P2_analysis_v1.1.R`, or any
v2/v2.1/v3/v3.1 workbook.

The P3 code is split into a thin entry script and testable functions in
`R/09_p3_analysis.R`. It never downloads a Google Sheet, stores credentials,
installs packages, or writes to the input workbook.

## Private input

Obtain the workbook manually and keep it outside version control. The simplest
local location is:

```text
data/private/Meta_Analysis_Input_v3.1.xlsx
```

Manual retrieval locations:

- [Meta_Analysis_Input_v3.1](https://docs.google.com/spreadsheets/d/168qCwWoH38DBJIMZyY1X42SWLsJxIZoAo4zgW_Gd88Q/edit?usp=drivesdk)
- [Effect lock v3.1](https://docs.google.com/spreadsheets/d/1Y7632FIHBUcEdJcZRZFgaWMTL01OMHEPeQGCgM9LMM0/edit?usp=drivesdk)

The runner does not access either URL. Export and place the workbook manually;
do not embed Drive credentials, cookies, or tokens in R code.

The input path is resolved in this order:

1. `--input=/absolute/path/Meta_Analysis_Input_v3.1.xlsx`;
2. `META_ANALYSIS_INPUT`;
3. the ignored repository path above;
4. a compatibility copy beside `P3_analysis_v1.0.R`.

Every workbook sheet is read with `skip = 2`, because the first two rows are
titles or notes and the third row contains the field names.

## Required sheets and precedence

The v3.1 manifest overrides a same-ID rule in the v3 manifest. Unchanged v3
modules remain available through the inherited manifest.

P3 requires the v3.1 QC, raw-effect, manifest, main, sensitivity, covariance,
quarantine, direction-audit, and decision-lock sheets specified by the v3.1
handoff. It also requires:

- `Analysis_Manifest_v3`;
- `Raw_Effects_v3`;
- `V_Matrix_A008`;
- `Event_Temporal_v3`;
- `Updating_v3`;
- `MPT_Separate_v3`;
- `Grey_LogOR_v3`;
- `Module_Gaussian_v3`.

`Rescue_Resolution_v3_1` identifies the seven rescue candidates and their
article IDs. The runner links those IDs to `Effect_Decision_Lock_v3_1`; it does
not treat the seven-row resolution sheet as an effect-level lock. Legacy rescue
sheet names remain accepted for compatibility, but they must expose the same
candidate/article mapping.

## Hard QC stop

Before fitting any model, the runner verifies the following conditions:

- every populated `QC_Summary_v3_1` status is `PASS`;
- `Raw_Effects_v3_1` contains 56 valid raw-effect records;
- the seven rescue article IDs select 21 v3.1 effect/component rows from
  `Effect_Decision_Lock_v3_1`;
- the set difference between `Raw_Effects_v3_1` and `Raw_Effects_v3` contains
  exactly 17 new atomic effect IDs, all mapped to a rescue candidate;
- the rescue layer contributes independent primary `k = 9` and sensitivity
  `k = 4`;
- every included record has finite `yi`, positive `sei` and `vi`, and
  `vi` consistent with `sei^2`;
- effect IDs are unique within every analytic sheet;
- every included effect maps to the combined manifest;
- S041 Paradigm 2 is not a second primary sample;
- S023 Experiment 2 has zero included rows in `Main_SMD_v3_1`;
- exactly two S023 Experiment 3 records remain quarantined and none enters an
  analytic sheet;
- S023 familiarity does not add an independent cluster;
- S041 Paradigm 2, the S010 N=71 subset, the S021 N=35 subgroup, boundary
  exclusions, and alternative outcomes/models are replacement sensitivities;
- every included direction is covered by an approved or locked
  `Direction_Audit_v3_1` record.

Any critical failure writes `qc_report.csv` and stops before model fitting. The
runner never changes a row to make a check pass.

## Statistical rules

- A compatible independent block with `k >= 2` uses
  `metafor::rma.uni(method = "REML", test = "knha")`.
- `k = 1` is descriptive only.
- Dependent effects use `metafor::rma.mv()` and an explicit sampling covariance
  matrix. A missing within-cluster covariance stops that block; zero is not
  substituted.
- CR2/Satterthwaite runs only when a dependent block has at least four
  independent clusters.
- A008 retains its five locked effects and complete 5 by 5 `V_Matrix_A008`.
  Its two shared-control clusters leave CR2 below threshold.
- Influence and leave-one-out diagnostics require a compatible `rma.uni`
  block with `k >= 4`.
- Egger regression requires a compatible `rma.uni` block with `k >= 10`.
- No inferential funnel plot is generated for an ineligible block.

The compatibility gate keeps logOR, SMD denominator families, Gaussian beta,
nonlinear coefficients, inverted-S functions, Bayesian MPT parameters,
incompatible PE encodings, and incompatible memory estimands separate. P3 does
not fit a single omnibus PE effect.

For S016, the four quadratic terms remain the primary estimand. When all four
recorded linear terms and their linear-quadratic covariances are available, P3
also fits an explicitly labeled joint L/Q ancillary model. It does not combine
those native coefficients with S041's one-SD orthogonal-polynomial coefficient.

## Replacement sensitivities

A replacement row must identify `replacement_for`. The builder removes that
primary effect before adding its alternative and stops if the number of
independent clusters changes. Primary and sensitivity estimates are written to
separate files.

Risk of bias remains `PROVISIONAL_NOT_LOCKED`. Without a separate final-locked
file supplied through `--rob-input=` or `META_ANALYSIS_ROB_INPUT`, P3 records:

```text
ROB_SENSITIVITY_SKIPPED_PROVISIONAL
```

Only a file explicitly marked `FINAL_LOCKED` can trigger the High-risk
exclusion sensitivity.

## Running P3

From the repository root:

```bash
Rscript analysis/P3_analysis_v1.0.R \
  --input=/absolute/path/Meta_Analysis_Input_v3.1.xlsx \
  --output-dir=/absolute/path/runP3v1_YYYYMMDD_HHMMSS
```

In RStudio, after opening the project and placing the workbook in
`data/private/`, run:

```r
source("analysis/P3_analysis_v1.0.R")
```

Without an explicit output path, the run is written to the ignored
`results/runP3v1_<timestamp>/` directory.

## Outputs

Each complete run writes at least:

```text
qc_report.csv
input_hashes.csv
model_manifest.csv
primary_estimates.csv
sensitivity_estimates.csv
descriptive_singletons.csv
skipped_models.csv
dependency_checks.csv
diagnostic_checks.csv
analysis_summary.md
sessionInfo.txt
figures/forest_*.pdf
figures/forest_*.png
models/model_objects.rds
```

The input SHA-256 is recorded before reading, after reading, and after analysis.
Any change causes a nonzero exit.

These outputs, the workbook, PDFs, raw repository data, and ZIP handoff files
remain local and must not be committed.

## Verification without private data

The public repository can parse the P3 entry point, run the normal `testthat`
suite, and run:

```bash
Rscript analysis/P3_analysis_v1.0.R --smoke-test
```

The smoke test uses invented values only. It exercises independent REML/HK,
known-V `rma.mv`, the four-cluster CR2 threshold, A008's two-cluster CR2 skip,
replacement sensitivity, and incompatibility rejection. It is not a real-data
analysis.

At initial implementation, the private v3.1 workbook was not present in the
GitHub workspace. Therefore no real P3 model result is claimed or frozen by
this repository change.
