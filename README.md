# Prediction Error and Human Episodic Memory

Reproducible R code for **Prediction Error and Human Episodic Memory: A Systematic Review and Multilevel Meta-analysis Across Memory Outcomes**.

This repository contains the analysis pipeline only. It does not contain copyrighted PDFs or the extraction workbook. The code is aligned with OSF protocol v1.0, frozen on 13 July 2026, and with the current `Meta_Analysis_Input_v1` workbook structure.

## 当前分析边界

The frozen baseline contains 21 strict primary effects, with one primary effect per independent sample:

| Analysis stream | Sheet | Current k | Rule |
|---|---|---:|---|
| Native log odds | `MAI_LogOR_v1` | 14 | Analyze only within compatible `pooling_block` values; endpoint-standardized columns are used only for prespecified endpoint blocks. |
| Standardized effects | `MAI_SMD_v1` | 5 | Keep `g_z`, `g_av`, provisional effects, and participant-slope SMDs separated unless denominator equivalence is documented. |
| Nonlinear effects | `MAI_Nonlinear_v1` | 2 | Analyze quadratic coefficients separately; with k < 5, retain as descriptive/narrative evidence. |

The three streams are never concatenated into one unqualified model. Positive effects mean that stronger, more unexpected, or more positive PE predicts better target memory after the prespecified direction harmonization.

## 1. 准备数据

Download the current Google Sheet as an Excel workbook and save it locally as:

```text
data/raw/prediction_error_memory_calculation_ready.xlsx
```

The workbook must contain these tabs:

- `Study_Sample_Map`
- `MAI_LogOR_v1`
- `MAI_SMD_v1`
- `MAI_Nonlinear_v1`

Do not upload the workbook to this public repository unless its public-release status has been checked separately.

## 2. 安装 R 包

Open the repository as an RStudio project or set the working directory to the repository root, then run:

```r
source("analysis/install_packages.R")
```

## 3. 运行全部分析

The simplest route is:

```r
source("analysis/run_analysis.R")
```

Or provide an explicit workbook and results directory from a terminal:

```bash
Rscript analysis/run_analysis.R \
  data/raw/prediction_error_memory_calculation_ready.xlsx \
  results
```

You may also set the workbook path before running:

```r
Sys.setenv(PEM_WORKBOOK = "D:/your-folder/workbook.xlsx")
source("analysis/run_analysis.R")
```

The default `PEM_STRICT_FREEZE=true` enforces the frozen 14 + 5 + 2 counts and the one-sample/one-primary-effect rule. Do not disable it for the confirmatory analysis unless the change is covered by a dated protocol amendment.

## 4. 主要输出

Each run creates a timestamped folder under `results/` containing:

- input audit and freeze checks;
- block-readiness table;
- conventional multilevel estimates;
- CR2/Satterthwaite estimates when feasible;
- 95% confidence and prediction intervals;
- variance components and I-squared-like decomposition;
- rho-dependence sensitivity results when a sample contributes multiple compatible effects;
- leave-one-sample-out and leave-one-report-out diagnostics;
- low/some-concerns risk-of-bias sensitivity, once ratings are final and complete;
- publication-bias eligibility checks;
- forest plots, model objects, and `sessionInfo()`.

Blocks with too few compatible independent samples are explicitly labeled `descriptive_only`; the code does not force a pooled estimate.

## Important safeguards

- The registered random-effects structure is used when estimable. If the frozen input has one effect per sample, the algebraically redundant nested effect component is collapsed and documented rather than pretending that two heterogeneity components are identifiable.
- `rho = .50` is the primary sampling-covariance assumption only when a sample contributes multiple compatible outcomes. Sensitivity values are `.00`, `.30`, `.70`, and `.90`.
- Nonlinear synthesis requires at least five compatible independent samples.
- Publication-bias analyses require at least ten compatible independent samples.
- Moderator models remain disabled until their minimum-sample rules are explicitly frozen; the pipeline does not invent a threshold after results are visible.
- No automatic fallback to a simpler substantive model occurs after a registered model fails. Failures and warnings are written to the audit output.

More detail is recorded in [docs/analysis_contract.md](docs/analysis_contract.md).
