required <- c(
  "clubSandwich",
  "dplyr",
  "metafor",
  "readxl",
  "testthat"
)

installed <- rownames(utils::installed.packages())
missing <- setdiff(required, installed)

if (length(missing) == 0L) {
  message("All required packages are already installed.")
} else {
  message("Installing: ", paste(missing, collapse = ", "))
  utils::install.packages(missing, repos = "https://cloud.r-project.org")
}

