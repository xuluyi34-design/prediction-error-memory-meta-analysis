# Frozen analysis contract

## Source alignment

- Project: *Prediction Error and Human Episodic Memory: A Systematic Review and Multilevel Meta-analysis Across Memory Outcomes*
- OSF protocol: version 1.0, finalized 13 July 2026
- Frozen primary-effect source: `Primary_Effect_Set_v1`
- Current analysis index: `Meta_Analysis_Input_v1`
- Frozen baseline: 14 native log-odds effects, 5 standardized effects, and 2 nonlinear records

## Estimand separation

The pipeline preserves three nonexchangeable analysis streams. Within the log-odds stream it also preserves `pooling_block`, because coefficients with different predictor units do not become exchangeable merely by sharing a logit link. Within the SMD stream, denominator definitions remain visible and provisional harmonization flags are honored. Quadratic coefficients are never combined with linear coefficients without their covariance.

## Registered model

For each compatible effect family, the protocol specifies a REML multilevel random-effects model. Sampling dependence is represented in `V`. `Report_ID` and `Sample_ID` are crossed random-intercept components, with `Effect_ID` nested within `Sample_ID` when that component is identifiable. Primary robust inference uses CR2 standard errors and Satterthwaite small-sample tests clustered at `Sample_ID` when feasible.

If every sample contributes exactly one effect, `Sample_ID` and the nested `Effect_ID` heterogeneity components are algebraically indistinguishable. The implementation therefore fits a single sample-level component in that case and records the reason. This is an estimability adjustment, not an unreported change in the substantive estimand.

## Current block rules

- `LOGOR_BINARY_CAT` uses native `yi` and `vi`.
- `LOGOR_ZSCORED_UPE` uses native one-SD PE coefficients and is separate from categorical contrasts.
- `LOGOR_ORDERED_ENDPOINT` uses `yi_endpoint` and `vi_endpoint` to represent the prespecified expected-to-unexpected endpoint contrast.
- `LOGOR_NATIVE_SRPE` retains the native centered signed-RPE unit.
- `LOGOR_SIGNED_FULLRANGE` uses endpoint columns for the full -1 to +1 signed-PE range.
- `LOGOR_RAW_URPE_POINT` retains one raw absolute-RPE point as its unit.
- SMD pooling follows the exact `pooling_block`; provisional blocks are not silently merged.
- `NONLINEAR_QUADRATIC` uses `beta_quadratic` and `vi_quadratic`; k must be at least 5 for synthesis.

## Dependence and sensitivity

Observed covariance is preferred. When compatible effects from one sample lack covariance information, the primary working value is rho = .50, with .00, .30, .70, and .90 sensitivity analyses. If there is only one effect per sample, rho is inapplicable and the pipeline reports that fact rather than producing five cosmetically identical analyses.

The pipeline implements leave-one-independent-sample-out and leave-one-report-out checks when the remaining data still meet the quantitative minimum. It does not treat robust variance estimation as a substitute for correct overlap coding.

The risk-of-bias sensitivity retains only finalized `Low`/`Low risk` and `Some concerns` judgments. If a compatible block still contains `Unclear`, mixed, missing, or otherwise nonfinal labels, the restricted model is not run and the unresolved labels are reported.

## Thresholds and disabled analyses

- General block synthesis requires at least 3 compatible independent samples in the current code skeleton.
- CR2 is attempted with at least 4 independent samples and is still reported with its Satterthwaite degrees of freedom.
- Nonlinear synthesis requires at least 5 independent samples.
- Publication-bias analysis requires at least 10 independent samples.
- Moderator thresholds were not numerically specified in the finalized text available to the code generator. Moderator fitting is therefore disabled until those rules are frozen or amended explicitly.

## Interpretation

Conclusions must integrate effect magnitude, uncertainty, prediction intervals, heterogeneity, independent-sample count, risk of bias, and robustness. Nonsignificance is not evidence of no effect. The prespecified practically trivial interval is Hedges' g from -0.10 to +0.10 and is applied only on a defensible g scale.
