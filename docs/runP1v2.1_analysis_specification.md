# runP1v2.1 analysis specification

Date: 13 July 2026

## Scope

This specification changes the R analysis method, not the P1v2 data freeze.
The 14 logOR, 5 SMD, and 3 nonlinear records (22 total) remain unchanged. No
effect estimate is re-extracted, recalculated, imputed, or replaced.

The strict input is `P1v2数据.xlsx`. Successful output folders use the prefix
`runP1v2.1_`.

## Model decision rule

1. Split records by compatible analysis stream and `pooling_block`.
2. Count distinct `Sample_ID` values and effects contributed by each sample.
3. With fewer than three independent samples, do not pool.
4. With one effect per sample and at least three samples, fit:

   ```r
   metafor::rma.uni(
     yi = yi_analysis,
     vi = vi_analysis,
     method = "REML",
     test = "knha"
   )
   ```

5. Only when a sample contributes multiple correlated effects, construct the
   sampling covariance matrix and fit a multilevel `rma.mv()` model. Run CR2
   only with at least four distinct `Sample_ID` clusters:

   ```r
   clubSandwich::coef_test(
     multilevel_model,
     vcov = "CR2",
     cluster = sample_id,
     test = "Satterthwaite"
   )
   ```

6. If a dependent-effect block has fewer than four sample clusters, retain
   individual descriptive results and do not force CR2 inference.
7. A synthesis based on samples from one report is labeled `within_report` and
   cannot be interpreted as a cross-study conclusion.

## Current P1v2 consequences

All 22 records are one-effect-per-sample. Thus eligible blocks use single-level
REML plus Hartung-Knapp, while CR2 is not used. `LOGOR_BINARY_CAT` is
preliminary; `LOGOR_ORDERED_ENDPOINT` and `LOGOR_RAW_URPE_POINT` are
within-report syntheses; `PAIRED_GZ` is exploratory. k = 1 and k = 2 blocks are
descriptive only, as is the nonlinear k = 3 block under the retained k >= 5
nonlinear threshold.
