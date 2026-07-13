required <- c(
  "clubSandwich",
  "dplyr",
  "metafor",
  "readxl",
  "testthat"
)

# GitHub-hosted runners may build `fs` from source. Its bundled libuv fallback
# keeps dependency installation independent of system-level libuv packages.
Sys.setenv(USE_BUNDLED_LIBUV = "1")

repos <- getOption("repos")
if (
  length(repos) == 0L ||
    is.null(repos[["CRAN"]]) ||
    identical(unname(repos[["CRAN"]]), "@CRAN@")
) {
  repos <- c(CRAN = "https://cloud.r-project.org")
}

installed <- rownames(utils::installed.packages())
missing <- setdiff(required, installed)

if (length(missing) == 0L) {
  message("All required packages are already installed.")
} else {
  message("Installing: ", paste(missing, collapse = ", "))
  utils::install.packages(missing, repos = repos)

  still_missing <- setdiff(required, rownames(utils::installed.packages()))
  if (length(still_missing) > 0L) {
    stop(
      "Required packages could not be installed: ",
      paste(still_missing, collapse = ", "),
      call. = FALSE
    )
  }
}
