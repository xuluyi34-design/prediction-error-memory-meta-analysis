#!/usr/bin/env Rscript

# P3_analysis_v1.0.R
# Read-only entry point for Meta_Analysis_Input_v3.1.xlsx.

p3_entry_path <- function() {
  command_line <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command_line, value = TRUE)
  if (length(file_arg) > 0L) return(sub("^--file=", "", file_arg[[1]]))
  f_index <- which(command_line == "-f")
  if (length(f_index) > 0L && f_index[[1]] < length(command_line)) {
    return(command_line[[f_index[[1]] + 1L]])
  }
  source_files <- vapply(
    sys.frames(),
    function(frame) if (!is.null(frame$ofile)) as.character(frame$ofile) else NA_character_,
    character(1)
  )
  source_files <- source_files[!is.na(source_files) & nzchar(source_files)]
  if (length(source_files) > 0L) return(tail(source_files, 1L))
  NA_character_
}

p3_entry <- p3_entry_path()
p3_entry_dir <- if (!is.na(p3_entry)) {
  dirname(normalizePath(p3_entry, winslash = "/", mustWork = FALSE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
p3_project_root <- normalizePath(file.path(p3_entry_dir, ".."), winslash = "/", mustWork = TRUE)
p3_module <- file.path(p3_project_root, "R", "09_p3_analysis.R")
if (!file.exists(p3_module)) {
  stop("P3 function module not found: ", p3_module, call. = FALSE)
}
source(p3_module, local = .GlobalEnv)

p3_args <- commandArgs(trailingOnly = TRUE)
if (p3_has_flag("smoke-test", p3_args)) {
  p3_smoke_test()
} else {
  p3_run_analysis(
    args = p3_args,
    project_root = p3_project_root,
    script_dir = p3_entry_dir
  )
}
