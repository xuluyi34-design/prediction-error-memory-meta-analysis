options(stringsAsFactors = FALSE)

project_root <- normalizePath(getwd(), mustWork = TRUE)
r_files <- sort(list.files(
  file.path(project_root, "R"),
  pattern = "\\.R$",
  full.names = TRUE
))
invisible(lapply(r_files, source, local = .GlobalEnv))

testthat::test_dir(
  file.path(project_root, "tests", "testthat"),
  reporter = "summary",
  stop_on_failure = TRUE,
  stop_on_warning = FALSE
)

