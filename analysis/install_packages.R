required <- c(
  "clubSandwich",
  "dplyr",
  "metafor",
  "purrr",
  "readxl",
  "stringr",
  "testthat",
  "tibble"
)

# GitHub-hosted runners may build `fs` from source. Its bundled libuv fallback
# keeps dependency installation independent of system-level libuv packages.
Sys.setenv(USE_BUNDLED_LIBUV = "1")

default_cran <- "https://cloud.r-project.org"
repo_override <- trimws(Sys.getenv("PEM_CRAN_REPO", unset = ""))
repos <- getOption("repos")

if (nzchar(repo_override)) {
  repos <- c(CRAN = repo_override)
} else if (.Platform$OS.type == "windows") {
  # Use the CRAN CDN on Windows instead of inheriting a stale or blocked mirror.
  repos <- c(CRAN = default_cran)
} else if (
  length(repos) == 0L ||
    is.null(repos[["CRAN"]]) ||
    identical(unname(repos[["CRAN"]]), "@CRAN@")
) {
  repos <- c(CRAN = default_cran)
}

message("Using CRAN repository: ", unname(repos[["CRAN"]]))

installed <- rownames(utils::installed.packages())
missing <- setdiff(required, installed)

if (length(missing) == 0L) {
  message("All required packages are already installed.")
} else {
  message("Installing: ", paste(missing, collapse = ", "))
  install_args <- list(pkgs = missing, repos = repos)
  if (.Platform$OS.type == "windows") {
    install_args$type <- "binary"
    message("Windows detected: installing binary packages.")
  }
  do.call(utils::install.packages, install_args)

  still_missing <- setdiff(required, rownames(utils::installed.packages()))
  if (length(still_missing) > 0L) {
    stop(
      "Required packages could not be installed: ",
      paste(still_missing, collapse = ", "),
      call. = FALSE
    )
  }
}
