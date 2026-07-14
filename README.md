# Prediction Error and Human Episodic Memory

Reproducible R code for **Prediction Error and Human Episodic Memory: A Systematic Review and Multilevel Meta-analysis Across Memory Outcomes**.

This repository contains the analysis pipeline only. It does not contain copyrighted PDFs or the extraction workbook. The code is aligned with OSF protocol v1.0, frozen on 13 July 2026, the unchanged P1v2 analysis-data freeze, and the dependence-aware `runP1v2.1` analysis specification.

## Analysis entry points

- `analysis/run_analysis.R` retains the frozen 22-effect P1v2 baseline and the
  `runP1v2.1` dependence-aware workflow described below.
- `analysis/P2_analysis_v1.R` is the separate P2 runner for the locked
  `Meta_Analysis_Input_v2` workbook. Its data structure, model rules, and run
  instructions are documented in [docs/meta-analysis-v2.md](docs/meta-analysis-v2.md).
- `analysis/P2_analysis_v1.1.R` retains the v1 model definitions for an interim
  metadata-only QC rerun of `Meta_Analysis_Input_v2.1`. It verifies the three
  independent A020 age-sample clusters and performs a 26-model numeric
  comparison against the successful v1 run at tolerance `1e-12`.
- `analysis/P3_analysis_v1.0.R` is the read-only entry point for the private
  `Meta_Analysis_Input_v3.1` workbook. It applies the v3.1 manifest over the
  inherited v3 modules, enforces the second-wave QC and replacement rules, and
  keeps primary, sensitivity, dependent-effect, and descriptive outputs
  separate. See [docs/meta-analysis-p3-v1.md](docs/meta-analysis-p3-v1.md).
- [docs/NEXT_CHAT_PROMPT.md](docs/NEXT_CHAT_PROMPT.md) is the handoff prompt for
  reviewing a completed private P2 run; it is not a replacement repository
  README.

The P2 and P3 workbooks remain private in Google Drive and are never downloaded
by the code. A local export belongs in the ignored `data/private/` directory or
may be supplied through an explicit path. The v2.1 rerun remains an interim QC
checkpoint and P3 does not alter the frozen P1/P2 files.

## 当前分析边界

The P1v2 frozen baseline contains 22 strict primary effects, with one primary effect per independent sample:

| Analysis stream | Sheet | Current k | Rule |
|---|---|---:|---|
| Native log odds | `MAI_LogOR_v1` | 14 | Analyze only within compatible `pooling_block` values; endpoint-standardized columns are used only for prespecified endpoint blocks. |
| Standardized effects | `MAI_SMD_v1` | 5 | Keep denominator families separated; `SMD_005` is confirmed as `g_z` in P1v2. |
| Nonlinear effects | `MAI_Nonlinear_v1` | 3 | Analyze quadratic coefficients separately; with k < 5, retain as descriptive/narrative evidence. |

The three streams are never concatenated into one unqualified model. Positive effects mean that stronger, more unexpected, or more positive PE predicts better target memory after the prespecified direction harmonization.

P1v2 adds the verified A011 Experiment 2A nonlinear coefficient (N = 22,
1,696 trials, quadratic beta = .04, SE = .10, logit link), confirms A031
Experiment 1 `SMD_005` as `g_z`, and standardizes all 22 included RoB labels to
`Some concerns`. A011 Experiment 2B, A013 Experiment 2, and A031 Experiment 2
remain in `Conditional_Effect_Queue_v2`; they are not assigned inferred point
estimates and are not read into the primary models.

P1v2 effect sizes are frozen. `runP1v2.1` changes the analysis method only:

| Data structure within a compatible block | Model | Primary inference |
|---|---|---|
| One effect per independent sample, k >= 3 | `metafor::rma.uni(method = "REML", test = "knha")` | Hartung-Knapp |
| Multiple correlated effects from a sample and at least 4 `Sample_ID` clusters | `metafor::rma.mv(method = "REML")` | CR2/Satterthwaite, clustered by `sample_id` |
| Multiple correlated effects but fewer than 4 `Sample_ID` clusters | No pooled primary model | Individual effects only |
| Fewer than 3 independent samples | No pooling | Individual effects only |

The CR2 threshold is based on the number of independent `Sample_ID` clusters,
not the total number of effects or reports. Because all 22 current P1v2 records
are one-effect-per-sample, the current pooled blocks use `rma.uni()` plus
Hartung-Knapp and do not use CR2.

Current interpretation labels are explicit: `LOGOR_BINARY_CAT` is a small,
preliminary cross-report synthesis; `LOGOR_ORDERED_ENDPOINT` and
`LOGOR_RAW_URPE_POINT` are single-report syntheses that cannot support a
cross-study conclusion; `PAIRED_GZ` is exploratory. All other k = 1 or k = 2
blocks and the nonlinear k = 3 block remain descriptive only.

## 1. 准备数据

Download the current Google Sheet as an Excel workbook and save it locally as:

```text
data/raw/P1v2数据.xlsx
```

The workbook must contain these tabs:

- `Study_Sample_Map`
- `Risk_of_Bias`
- `MAI_LogOR_v1`
- `MAI_SMD_v1`
- `MAI_Nonlinear_v1`

Do not upload the workbook to this public repository unless its public-release status has been checked separately.

## 2. 安装 R 包

Open the repository as an RStudio project or set the working directory to the repository root, then run:

```r
source("analysis/install_packages.R")
```

On Windows, the installer uses the official CRAN CDN and binary packages to
avoid stale or blocked mirrors. Set `PEM_CRAN_REPO` only when an explicit CRAN
mirror override is needed.

## 3. 运行全部分析

The simplest route is:

```r
source("analysis/run_analysis.R")
```

Or provide an explicit workbook and results directory from a terminal:

```bash
Rscript analysis/run_analysis.R \
  "data/raw/P1v2数据.xlsx" \
  results
```

You may also set the workbook path before running:

```r
Sys.setenv(PEM_WORKBOOK = "D:/your-folder/workbook.xlsx")
source("analysis/run_analysis.R")
```

The default `PEM_STRICT_FREEZE=true` enforces the filename `P1v2数据.xlsx`, the P1v2 frozen 14 + 5 + 3 counts, and the one-sample/one-primary-effect rule. Do not disable it for the confirmatory analysis unless the change is covered by a dated protocol amendment.

Each successful run is written to `results/runP1v2.1_YYYYMMDD_HHMMSS/`.

## 4. 主要输出

Each run creates a timestamped folder under `results/` containing:

- input audit and freeze checks;
- block-readiness table;
- the dependence-based model decision for every compatible block;
- `rma.uni` REML estimates with Hartung-Knapp inference for the current pooled blocks;
- `rma.mv` plus CR2/Satterthwaite only for future blocks with dependent effects and at least four `Sample_ID` clusters;
- 95% confidence and prediction intervals;
- heterogeneity estimates and I-squared-like decomposition;
- a separate table of individual effects for every `descriptive_only` block;
- rho-dependence sensitivity results when a sample contributes multiple compatible effects;
- leave-one-sample-out and leave-one-report-out diagnostics;
- low/some-concerns risk-of-bias sensitivity, once ratings are final and complete;
- publication-bias eligibility checks;
- forest plots, model objects, and `sessionInfo()`.

Blocks with too few compatible independent samples are explicitly labeled `descriptive_only`; the code does not force a pooled estimate.

## Important safeguards

- One-effect-per-sample blocks fit one between-effect variance (`tau2`) with `rma.uni`; they do not simultaneously estimate `report_id` and `sample_id` random variances.
- A k = 3 synthesis drawn entirely from one report is labeled `within_report` and cannot be reported as a cross-study conclusion.
- `rho = .50` is the primary sampling-covariance assumption only when a sample contributes multiple compatible outcomes. Sensitivity values are `.00`, `.30`, `.70`, and `.90`.
- Nonlinear synthesis requires at least five compatible independent samples.
- Publication-bias analyses require at least ten compatible independent samples.
- Moderator models remain disabled until their minimum-sample rules are explicitly frozen; the pipeline does not invent a threshold after results are visible.
- No automatic fallback to a simpler substantive model occurs after a registered model fails. Failures and warnings are written to the audit output.

More detail is recorded in [docs/analysis_contract.md](docs/analysis_contract.md).
