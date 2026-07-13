options(stringsAsFactors = FALSE)

full_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", full_args, value = TRUE)

if (length(file_arg) > 0L) {
  script_path <- sub("^--file=", "", file_arg[[1]])
  project_root <- dirname(dirname(normalizePath(script_path, mustWork = TRUE)))
} else {
  project_root <- normalizePath(getwd(), mustWork = TRUE)
}

if (!file.exists(file.path(project_root, "R", "00_config.R"))) {
  stop(
    "Run this script from the repository root, or use Rscript analysis/run_analysis.R.",
    call. = FALSE
  )
}

r_files <- sort(list.files(
  file.path(project_root, "R"),
  pattern = "\\.R$",
  full.names = TRUE
))
invisible(lapply(r_files, source, local = .GlobalEnv))

args <- commandArgs(trailingOnly = TRUE)
config <- pem_analysis_config()
default_workbook <- file.path(
  project_root,
  "data",
  "raw",
  config$expected_workbook_filename
)
workbook <- if (length(args) >= 1L) {
  args[[1]]
} else {
  Sys.getenv("PEM_WORKBOOK", unset = default_workbook)
}
output_root <- if (length(args) >= 2L) {
  args[[2]]
} else {
  file.path(project_root, "results")
}

strict_value <- tolower(Sys.getenv("PEM_STRICT_FREEZE", unset = "true"))
strict_freeze <- !strict_value %in% c("false", "0", "no")

pem_run_analysis(
  workbook = workbook,
  output_root = output_root,
  strict_freeze = strict_freeze,
  config = config
)
