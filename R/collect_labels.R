#' Collect non-blank variable labels
#'
#' Internal helper used by the Shiny app to turn the text typed into the
#' per-variable label boxes into the named vector `summary_table()` expects
#' for `ylab`. Blank (or whitespace-only) entries are dropped so those
#' variables keep their original names.
#'
#' @param vars Character vector of variable names.
#' @param values Character vector of typed labels, same length and order as
#'   `vars`.
#'
#' @return A named character vector (names are variable names), or `NULL` if
#'   no non-blank label was entered.
#'
#' @keywords internal
#' @noRd
collect_labels <- function(vars, values) {
  values <- trimws(values)
  keep <- nzchar(values)
  if (!any(keep)) {
    return(NULL)
  }
  stats::setNames(values[keep], vars[keep])
}
