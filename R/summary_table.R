#' Build a "Table 1"-style summary table
#'
#' Produces a descriptive (and, optionally, comparison) summary table for one
#' or more outcome variables, optionally stratified by a grouping variable.
#' This is the successor to the original `tbl_s()` function.
#'
#' @param x Character scalar naming the grouping/independent variable in
#'   `data`, or `NULL` (default) for an unstratified overall table.
#' @param y Character vector naming the outcome/dependent variable(s) in
#'   `data`.
#' @param data A data frame containing `x` (if supplied) and all of `y`.
#' @param p Logical. If `TRUE`, add a p-value column comparing `y` across the
#'   levels of `x`. Default `FALSE`. Ignored if `x` is `NULL`.
#' @param ylab Optional named character vector mapping variable names in
#'   `data` to display labels, e.g. `c(Sepal.Length = "Sepal length")`.
#' @param caption Optional caption string. If non-empty, `", N = {N}"` is
#'   appended automatically.
#' @param span_header Optional string used as a spanning header over the
#'   per-group statistic columns.
#' @param mean Logical. If `TRUE`, continuous variables are summarized as
#'   mean \eqn{\pm} SD instead of the `gtsummary` default (median \[IQR\]).
#'   Default `FALSE`.
#' @param flex Logical. If `TRUE` (default), the result is converted to a
#'   `flextable` object (via [flextable::as_flex_table()] with
#'   [flextable::theme_box()]) suitable for direct export to Word/PowerPoint.
#'   If `FALSE`, the raw `gtsummary` table object is returned instead.
#'
#' @return A `flextable` object (if `flex = TRUE`) or a `gtsummary` table
#'   object (if `flex = FALSE`).
#'
#' @examples
#' lab_y <- c(Sepal.Width = "Sepal width", Sepal.Length = "Sepal length")
#' summary_table(y = c("Sepal.Width", "Sepal.Length"), data = iris)
#' summary_table(
#'   x = "Species", y = c("Sepal.Width", "Sepal.Length"), data = iris,
#'   p = TRUE, ylab = lab_y, caption = "**Comparison**"
#' )
#'
#' @export
summary_table <- function(x = NULL,
                           y,
                           data,
                           p = FALSE,
                           ylab = NULL,
                           caption = "",
                           span_header = NULL,
                           mean = FALSE,
                           flex = TRUE) {
  if (length(ylab) >= 1) {
    Hmisc::label(data) <- as.list(ylab[match(names(data), names(ylab))])
  }

  table <- data |>
    dplyr::select(dplyr::all_of(c(x, y)))

  if (mean == FALSE) {
    table <- table |>
      gtsummary::tbl_summary(
        by = dplyr::all_of(x),
        type = gtsummary::all_dichotomous() ~ "categorical"
      )
  } else {
    table <- table |>
      gtsummary::tbl_summary(
        by = dplyr::all_of(x),
        type = gtsummary::all_dichotomous() ~ "categorical",
        statistic = gtsummary::all_continuous() ~ "{mean} ± {sd}"
      )
  }

  if (length(span_header) > 0) {
    table <- table |>
      gtsummary::modify_spanning_header(
        c("stat_1":paste0("stat_", length(table[["table_body"]]) - 5)) ~ span_header
      )
  }

  if (length(x) > 0) {
    table <- table |>
      gtsummary::modify_header(gtsummary::all_stat_cols() ~ "**{level}**, N = {n} ({n*100/N}%)") |>
      gtsummary::add_overall() |>
      gtsummary::modify_header(stat_0 ~ "**Total**, N = {N}") |>
      gtsummary::modify_table_body(~ .x |> dplyr::relocate(stat_0, .after = dplyr::last_col()))
  }

  if (caption != "") {
    table <- table |>
      gtsummary::modify_caption(paste(caption, "N = {N}"))
  }

  if (p == TRUE) {
    table <- table |>
      gtsummary::add_p()
  }

  if (length(y) == 1) {
    table <- table |>
      gtsummary::modify_header(label ~ y)
  } else {
    table <- table |>
      gtsummary::modify_header(label ~ "**Variable**")
  }

  table <- table |>
    gtsummary::bold_labels()

  if (flex == TRUE) {
    table <- table |>
      gtsummary::as_flex_table() |>
      flextable::theme_box()
  }

  table
}
