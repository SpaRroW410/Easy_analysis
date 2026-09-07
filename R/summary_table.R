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
#' @param parametric Optional character vector of continuous variable names
#'   (drawn from `y`) that should always use a parametric test
#'   (`t.test`/`aov`), overriding the automatic normality check below.
#'   Default `NULL`.
#' @param flex Logical. If `TRUE` (default), the result is converted to a
#'   `flextable` object (via [flextable::as_flex_table()] with
#'   [flextable::theme_box()]) suitable for direct export to Word/PowerPoint.
#'   If `FALSE`, the raw `gtsummary` table object is returned instead.
#'
#' @details
#' When `p = TRUE` and `x` is supplied, the test applied to each *continuous*
#' variable in `y` (numeric with more than two distinct values) is chosen
#' automatically based on normality, using the same Shapiro-Wilk logic as
#' [check_normality()] but checked across *every* level of `x`: a variable
#' counts as normal only if it passes (p > 0.05) in every group. Normal
#' variables use `t.test` (two groups) or `aov` (three or more groups);
#' non-normal variables use `wilcox.test` or `kruskal.test` respectively. Any
#' variable named in `parametric` skips the normality check and always uses
#' the parametric test — useful when Shapiro-Wilk is overly sensitive (e.g.
#' large samples) and a parametric test is preferred anyway. Categorical
#' variables in `y` are unaffected and keep using `gtsummary`'s own default
#' test.
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
#' # Force Sepal.Width to a parametric test regardless of normality
#' summary_table(
#'   x = "Species", y = c("Sepal.Width", "Sepal.Length"), data = iris,
#'   p = TRUE, parametric = "Sepal.Width"
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
                           parametric = NULL,
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
    continuous_vars <- character(0)
    if (length(x) > 0) {
      is_continuous <- vapply(
        y,
        function(v) is.numeric(data[[v]]) && dplyr::n_distinct(data[[v]], na.rm = TRUE) > 2,
        logical(1)
      )
      continuous_vars <- y[is_continuous]
    }

    if (length(continuous_vars) > 0) {
      n_groups <- dplyr::n_distinct(data[[x]], na.rm = TRUE)

      normal <- stats::setNames(rep(TRUE, length(continuous_vars)), continuous_vars)
      vars_to_check <- setdiff(continuous_vars, parametric)
      if (length(vars_to_check) > 0) {
        normal[vars_to_check] <- all_groups_normal(x, vars_to_check, data)
      }

      test_name <- ifelse(normal,
        if (n_groups > 2) "aov" else "t.test",
        if (n_groups > 2) "kruskal.test" else "wilcox.test"
      )

      test_list <- Map(
        function(v, t) stats::as.formula(sprintf("`%s` ~ \"%s\"", v, t)),
        continuous_vars, test_name
      )

      table <- table |>
        gtsummary::add_p(test = test_list)
    } else {
      table <- table |>
        gtsummary::add_p()
    }
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
