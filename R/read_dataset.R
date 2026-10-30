#' Read an uploaded dataset
#'
#' Internal helper used by the Shiny app to read a CSV or Excel file. Character
#' columns are converted to factors so they behave as grouping variables
#' (e.g. in [check_normality()], which relies on `levels()`).
#'
#' @param path Path to the file on disk (for Shiny uploads, the temporary
#'   `datapath`).
#' @param name Original file name, used to determine the file type from its
#'   extension. Defaults to `path`.
#'
#' @return A data frame.
#'
#' @keywords internal
#' @noRd
read_dataset <- function(path, name = path) {
  ext <- tolower(tools::file_ext(name))

  data <- switch(ext,
    csv = utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    xlsx = ,
    xls = as.data.frame(readxl::read_excel(path)),
    stop("Unsupported file type '.", ext, "'. Please upload a .csv, .xlsx or .xls file.",
         call. = FALSE)
  )

  is_char <- vapply(data, is.character, logical(1))
  data[is_char] <- lapply(data[is_char], as.factor)
  data
}
