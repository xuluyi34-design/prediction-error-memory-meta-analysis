# 给另一个对话的完整提示词

请将本文件下方“可直接复制的提示词”整段复制到新的对话，并同时上传：

1. `Meta_Analysis_Input_v2.xlsx`
2. `P2_analysis_v1.R`

---

## 可直接复制的提示词

我正在进行“预测误差/惊讶与记忆”的元分析。请使用我本次上传的两个文件继续工作：

- `Meta_Analysis_Input_v2.xlsx`
- `P2_analysis_v1.R`

项目状态与不可更改的前提：

1. 文献报告总数固定为 **51 篇**；不要改变这个分母。
2. P1v2 与 P2 的第二轮全文数值提取已经完成。
3. 工作簿已经锁定 64 条可量化候选效应的状态：
   - `LOCKED_PRIMARY`：21 条
   - `LOCKED_SENSITIVITY`：21 条
   - `LOCKED_ROBUSTNESS`：5 条
   - `LOCKED_DESCRIPTIVE`：12 条
   - `LOCKED_EXCLUDE_DUPLICATE`：1 条
   - `PENDING_PRECISION`：4 条
4. `A026/P2L010` 是 A011 的重复样本重分析，不得作为新增独立效应。
5. A021 与 A042 共用 N=42；A042 是行为结果锚点，A021 仅作神经机制证据。
6. A008 的 5 条效应必须使用工作簿中的 `V_Matrix_A008` 已知抽样协方差矩阵；它只有两个共享对照簇，因此不要强行运行 CR2。
7. 不同量尺和不同结果不能强行合并：logOR、Hedges g_z、g_av、Gaussian β、非线性 β 和 Bayesian MPT 参数必须维持独立分析流。
8. 非线性模型中的线性项与二次项来自同一模型，不能当作独立效应；仅按工作簿和脚本中已经指定的二次项分析处理。
9. 不要重新读取或重新编码 PDF，也不要静默修改效应方向、量尺、纳入标记、重复样本规则、`model_id` 或目标 contrast。
10. 不得覆盖或回写输入工作簿和 R 脚本。

请完成以下工作：

1. 将 `Meta_Analysis_Input_v2.xlsx` 与 `P2_analysis_v1.R` 放在同一文件夹。
2. 检查本机是否安装脚本要求的 R 包；如缺失，按脚本报出的精确命令安装：
   `readxl`, `dplyr`, `tibble`, `purrr`, `stringr`, `metafor`, `clubSandwich`。
3. 运行：

   ```r
   source("P2_analysis_v1.R")
   ```

   或在终端运行：

   ```bash
   Rscript P2_analysis_v1.R
   ```

4. 脚本应生成时间戳文件夹 `runP2v1_YYYYMMDD_HHMMSS`。请完整检查其中的验证表、模型结果、诊断记录、森林图和运行日志。
5. 若发生路径、包版本或 R 运行环境错误，可以修复运行方式；但不得修改锁定的统计决策。
6. 若输入校验失败，请立即停止，不要猜测或自动修补数据；请准确报告：
   - 出错工作表；
   - 效应 ID；
   - 字段名；
   - 期望值与实际值。
7. 若某模型因 k 太小而不能做某项诊断，必须明确记录为“跳过”，不能把未运行写成阴性结果。

统计规则：

- 相互独立且量尺兼容的随机效应块：REML + Hartung–Knapp。
- A008：使用显式已知 V 的 `rma.mv`；报告 t-based 推断，并说明 CR2 因仅两个独立共享对照簇而跳过。
- CR2 仅在至少 4 个独立簇时才允许运行。
- k=1 的块只报告描述性单效应估计，不称为合并效应。
- influence 与 leave-one-out 仅在兼容的 `rma.uni` 模型且 k≥4 时运行。
- Egger/发表偏倚检验仅在同一兼容量尺块 k≥10 时运行；否则明确记录跳过。
- 小 k 的 Hartung–Knapp 区间应谨慎解释。

请用中文向我汇报，但保留原始变量名、工作表名和 `model_id`。汇报内容至少包括：

1. 输入验证是否全部通过，以及最终进入模型的效应条数；
2. 每个 `model_id` 的 k、估计值、SE、95% CI、p、τ²、I²、Q 与 Q p 值；
3. 哪些模型使用 Hartung–Knapp，哪个模型使用已知 V 的多变量模型；
4. CR2 的逐模型判断与 A008 的跳过理由；
5. influence/leave-one-out 的运行或跳过情况，以及有无明显影响点；
6. 发表偏倚检验的运行或跳过情况；
7. 森林图文件清单；
8. 对主分析、敏感性分析、稳健性分析与事件/更新模块分别作简洁解释；
9. 任何异常、警告或仍需作者查询的数据。

最后请把完整的 `runP2v1_YYYYMMDD_HHMMSS` 文件夹压缩为 `runP2v1.zip` 并提供给我，不要遗漏 `tables`、`figures`、`models`、`logs` 和 `RUN_NOTE.txt`。
