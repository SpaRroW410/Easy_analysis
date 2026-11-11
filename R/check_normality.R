#' Shapiro-Wilk p-values for one group
#'
#' Internal helper shared by [check_normality()] and the automatic
#' normality-based test selection in [summary_table()]. Runs a Shapiro-Wilk
#' test on each variable in `y`, restricted to rows where `x` equals
#' `group_value`.
#'
#' @param x Character scalar naming the grouping variable in `data`.
#' @param y Character vector naming the continuous variable(s) to test.
#' @param data A data frame containing `x` and all of `y`.
#' @param group_value The single value of `x` to filter rows to before
#'   testing.
#'
#' @return A data frame with one row per variable in `y`, with columns
#'   `Variable` and `p.value` (the raw Shapiro-Wilk p-value).
#'
#' @keywords internal
#' @noRd
shapiro_p_one_group <- function(x, y, data, group_value) {
  in_group <- data[[x]] == group_value
  p_values <- vapply(
    y,
    function(v) stats::shapiro.test(as.numeric(data[[v]][in_group]))$p.value,
    numeric(1)
  )
  data.frame(Variable = y, p.value = unname(p_values), stringsAsFactors = FALSE)
}

#' Check whether variables are normal across every group
#'
#' Internal helper used by [summary_table()] to decide, for each variable in
#' `y`, whether a parametric test is appropriate: a variable only counts as
#' normal if its Shapiro-Wilk p-value exceeds `alpha` in *every* level of
#' `x`, not just one.
#'
#' @inheritParams shapiro_p_one_group
#' @param alpha Significance threshold. Default `0.05`.
#'
#' @return A named logical vector, one entry per variable in `y` (in the
#'   same order), `TRUE` if that variable is normal in every group.
#'
#' @keywords internal
#' @noRd
all_groups_normal <- function(x, y, data, alpha = 0.05) {
  group_levels <- levels(as.factor(data[[x]]))

  p_by_group <- lapply(group_levels, function(group_value) {
    shapiro_p_one_group(x, y, data, group_value)
  })

  result <- dplyr::bind_rows(p_by_group) |>
    dplyr::group_by(Variable) |>
    dplyr::summarise(all_normal = all(p.value > alpha), .groups = "drop")

  stats::setNames(result$all_normal, result$Variable)[y]
}

#' Check normality of continuous variables within one group
#'
#' Runs a Shapiro-Wilk normality test on each variable in `y`, restricted to
#' rows where the grouping variable `x` equals its first level. This is the
#' successor to the original `s.t()` helper, which was written alongside the
#' first iteration of [summary_table()] but dropped from later versions.
#'
#' @param x Character scalar naming a factor column in `data` used to select
#'   the subgroup to test (its first level, via `levels(data[[x]])[[1]]`).
#' @param y Character vector naming the continuous variable(s) to test.
#' @param data A data frame containing `x` and all of `y`.
#'
#' @return A data frame with one row per variable in `y`, giving the rounded
#'   Shapiro-Wilk p-value (`p.value_dec`) and whether it falls below 0.05
#'   (`p.value_r`).
#'
#' @examples
#' check_normality(x = "Species", y = c("Sepal.Width", "Sepal.Length"), data = iris)
#'
#' @export
check_normality <- function(x, y, data) {
  shapiro_p_one_group(x, y, data, levels(data[[x]])[[1]]) |>
    dplyr::mutate(p.value_dec = formattable::formattable(p.value, digits = 3, format = "f")) |>
    dplyr::mutate(p.value_r = dplyr::case_when(
      p.value < 0.05 ~ TRUE,
      p.value > 0.05 ~ FALSE
    )) |>
    dplyr::select(-p.value)
}
