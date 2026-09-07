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
  data |>
    dplyr::filter(!!as.symbol(x) == levels(data[[x]])[[1]]) |>
    tidyr::gather(key = "Variable", value = "value", dplyr::all_of(y)) |>
    dplyr::group_by(Variable) |>
    dplyr::do(broom::tidy(shapiro.test(.$value))) |>
    dplyr::ungroup() |>
    dplyr::mutate(p.value_dec = formattable::formattable(p.value, digits = 3, format = "f")) |>
    dplyr::mutate(p.value_r = dplyr::case_when(
      p.value < 0.05 ~ TRUE,
      p.value > 0.05 ~ FALSE
    )) |>
    dplyr::select(-c(method, p.value, statistic))
}
