#' Choose a test for each continuous variable
#'
#' Internal helper used by [summary_table()] when `p = TRUE`. For every
#' continuous variable in `y` (numeric with more than two distinct values),
#' picks `t.test`/`aov` if the variable is normal in every group of `x` (or is
#' listed in `parametric`), otherwise `wilcox.test`/`kruskal.test`, using the
#' two-group or three-or-more-group variant according to the number of levels
#' of `x`.
#'
#' @inheritParams summary_table
#'
#' @return A list of formulas in the form `gtsummary::add_p(test = )` expects
#'   (one per continuous variable), or `NULL` if there is no grouping variable
#'   or no continuous variable, in which case `gtsummary`'s defaults apply.
#'
#' @keywords internal
#' @noRd
choose_tests <- function(x, y, data, parametric = NULL) {
  if (length(x) == 0) {
    return(NULL)
  }

  is_continuous <- vapply(
    y,
    function(v) is.numeric(data[[v]]) && dplyr::n_distinct(data[[v]], na.rm = TRUE) > 2,
    logical(1)
  )
  continuous_vars <- y[is_continuous]
  if (length(continuous_vars) == 0) {
    return(NULL)
  }

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

  # Unnamed on purpose: gtsummary reads a named list as list(var = value) and
  # would then reject the formulas.
  unname(Map(
    function(v, t) stats::as.formula(sprintf("`%s` ~ \"%s\"", v, t)),
    continuous_vars, test_name
  ))
}
