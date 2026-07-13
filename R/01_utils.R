`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

pem_abort <- function(message) {
  stop(message, call. = FALSE)
}

pem_check_dependencies <- function(config = pem_analysis_config()) {
  missing <- config$required_packages[
    !vapply(config$required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing) > 0L) {
    pem_abort(paste0(
      "Missing R packages: ", paste(missing, collapse = ", "),
      ". Run source('analysis/install_packages.R') first."
    ))
  }

  invisible(TRUE)
}

pem_normalise_names <- function(x) {
  x <- trimws(x)
  x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", x)
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

pem_drop_empty_rows <- function(data) {
  if (nrow(data) == 0L) return(data)

  populated <- vapply(
    seq_len(nrow(data)),
    function(i) {
      values <- unlist(data[i, , drop = FALSE], use.names = FALSE)
      any(!is.na(values) & nzchar(trimws(as.character(values))))
    },
    logical(1)
  )

  data[populated, , drop = FALSE]
}

pem_require_columns <- function(data, columns, object_name = deparse(substitute(data))) {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    pem_abort(paste0(
      object_name, " is missing required columns: ",
      paste(missing, collapse = ", ")
    ))
  }
  invisible(TRUE)
}

pem_as_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

pem_make_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

pem_run_id <- function(time = Sys.time()) {
  format(time, "%Y%m%d_%H%M%S")
}

pem_safe_filename <- function(x) {
  x <- gsub("[^A-Za-z0-9_-]+", "_", x)
  gsub("^_+|_+$", "", x)
}

pem_capture_conditions <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(
    tryCatch(expr, error = function(e) e),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  list(
    value = value,
    error = if (inherits(value, "error")) conditionMessage(value) else NULL,
    warnings = unique(warnings)
  )
}

pem_write_csv <- function(data, path) {
  utils::write.csv(
    data,
    file = path,
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
  invisible(path)
}

pem_bind_rows <- function(x) {
  if (length(x) == 0L) return(data.frame())
  dplyr::bind_rows(x)
}

