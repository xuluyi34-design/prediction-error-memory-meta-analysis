# Frozen analysis contract

## Source alignment

- Project: *Prediction Error and Human Episodic Memory: A Systematic Review and Multilevel Meta-analysis Across Memory Outcomes*
- OSF protocol: version 1.0, finalized 13 July 2026
- Frozen primary-effect source: `Primary_Effect_Set_v1`
- Current analysis data freeze: `P1v2`
- Current analysis method/output version: `runP1v2.1`
- Required strict-run workbook filename: `P1v2数据.xlsx`
- Current analysis sheets: `MAI_LogOR_v1`, `MAI_SMD_v1`, and `MAI_Nonlinear_v1`
- Frozen baseline: 14 native log-odds effects, 5 standardized effects, and 3 nonlinear records (22 independent primary samples)

## P1v2 corrections and boundaries

- A011 Experiment 2A is included as a nonlinear primary record: N = 22,
  1,696 trials, quadratic beta = .04, SE = .10, with a logit link.
- `SMD_005` from A031 Experiment 1 is confirmed as `g_z` and is no longer
  provisional.
- All 22 included primary samples use the finalized `Some concerns` risk-of-bias
  label.
- A011 Experiment 2B, A013 Experiment 2, and A031 Experiment 2 remain in
  `Conditional_Effect_Queue_v2` because reliable exact estimates are not yet
  available. Queue records are not read into the primary analysis, and no
  inferred values are substituted as point estimates.

## Estimand separation

The pipeline preserves three nonexchangeable analysis streams. Within the log-odds stream it also preserves `pooling_block`, because coefficients with different predictor units do not become exchangeable merely by sharing a logit link. Within the SMD stream, denominator definitions remain visible and provisional harmonization flags are honored. Quadratic coefficients are never combined with linear coefficients without their covariance.

## runP1v2.1 dependence-aware model selection

P1v2 effect estimates and variances remain frozen. `runP1v2.1` changes only the
model and small-sample inference implementation:

- If every independent sample contributes exactly one compatible effect and
  there are at least three samples, fit `metafor::rma.uni()` by REML with
  `test = "knha"`. This estimates one between-effect variance and uses
  Hartung-Knapp inference. It does not attempt separate `Report_ID` and
  `Sample_ID` random variances.
- If at least one sample contributes multiple correlated effects, use a
  sampling covariance matrix and `metafor::rma.mv()`. Primary inference then
  uses `clubSandwich::coef_test(..., vcov = "CR2", cluster = sample_id)`.
- The CR2 eligibility threshold is at least four distinct `Sample_ID` clusters.
  It is not based on total effect count. `Report_ID` is not substituted as the
  cluster variable.
- If a dependent-effect block has fewer than four independent sample clusters,
  no CR2 primary inference or pooled primary conclusion is produced.
- If a compatible block has fewer than three independent samples, it is not
  pooled.

All current P1v2 blocks contain one effect per sample, so any eligible current
block uses `rma.uni()` plus Hartung-Knapp; CR2 is not part of the P1v2.1 primary
results.

## Current block rules

- `LOGOR_BINARY_CAT` uses native `yi` and `vi`.
- `LOGOR_ZSCORED_UPE` uses native one-SD PE coefficients and is separate from categorical contrasts.
- `LOGOR_ORDERED_ENDPOINT` uses `yi_endpoint` and `vi_endpoint` to represent the prespecified expected-to-unexpected endpoint contrast.
- `LOGOR_NATIVE_SRPE` retains the native centered signed-RPE unit.
- `LOGOR_SIGNED_FULLRANGE` uses endpoint columns for the full -1 to +1 signed-PE range.
- `LOGOR_RAW_URPE_POINT` retains one raw absolute-RPE point as its unit.
- SMD pooling follows the exact `pooling_block`; `SMD_005` is treated as confirmed
  `g_z`, while any genuinely provisional blocks remain isolated.
- `NONLINEAR_QUADRATIC` uses `beta_quadratic` and `vi_quadratic`; P1v2 contains
  three records, and k must be at least 5 for synthesis.

For the current P1v2 block composition:

- `LOGOR_BINARY_CAT` is pooled by single-level REML plus Hartung-Knapp and is
  labeled preliminary cross-report evidence (four samples, three reports).
- `LOGOR_ORDERED_ENDPOINT` is pooled by single-level REML plus Hartung-Knapp
  and labeled a single-report synthesis across three age samples, not a
  cross-study conclusion.
- `LOGOR_RAW_URPE_POINT` is pooled by single-level REML plus Hartung-Knapp and
  labeled a single-report synthesis across three experiments, not a
  cross-study conclusion.
- `PAIRED_GZ` is pooled by single-level REML plus Hartung-Knapp and labeled
  exploratory.
- Other k = 1 or k = 2 blocks remain `descriptive_only`.
- The nonlinear k = 3 block remains `descriptive_only` under its k >= 5 rule.

## Dependence and sensitivity

Observed covariance is preferred. When compatible effects from one sample lack covariance information, the primary working value is rho = .50, with .00, .30, .70, and .90 sensitivity analyses. If there is only one effect per sample, rho is inapplicable and the pipeline reports that fact rather than producing five cosmetically identical analyses.

The pipeline implements leave-one-independent-sample-out and leave-one-report-out checks when the remaining data still meet the quantitative minimum. It does not treat robust variance estimation as a substitute for correct overlap coding.

The risk-of-bias sensitivity retains only finalized `Low`/`Low risk` and `Some concerns` judgments. If a compatible block still contains `Unclear`, mixed, missing, or otherwise nonfinal labels, the restricted model is not run and the unresolved labels are reported.

## Thresholds and disabled analyses

- General block synthesis requires at least 3 compatible independent samples.
- CR2 applies only to dependent-effect multilevel models with at least 4
  distinct `Sample_ID` clusters and is reported with Satterthwaite degrees of
  freedom.
- Nonlinear synthesis requires at least 5 independent samples.
- Publication-bias analysis requires at least 10 independent samples.
- Moderator thresholds were not numerically specified in the finalized text available to the code generator. Moderator fitting is therefore disabled until those rules are frozen or amended explicitly.

## Interpretation

Conclusions must integrate effect magnitude, uncertainty, prediction intervals, heterogeneity, independent-sample count, risk of bias, and robustness. Nonsignificance is not evidence of no effect. The prespecified practically trivial interval is Hedges' g from -0.10 to +0.10 and is applied only on a defensible g scale.
