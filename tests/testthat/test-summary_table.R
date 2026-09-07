test_that("summary_table returns a flextable for an unstratified table", {
  result <- summary_table(y = c("Sepal.Width", "Sepal.Length"), data = iris)
  expect_s3_class(result, "flextable")
})

test_that("summary_table returns a flextable when stratified with a p-value", {
  lab_y <- c(Sepal.Width = "Sepal width", Sepal.Length = "Sepal length")
  result <- summary_table(
    x = "Species", y = c("Sepal.Width", "Sepal.Length"), data = iris,
    p = TRUE, ylab = lab_y, caption = "**Comparison**"
  )
  expect_s3_class(result, "flextable")
})

test_that("summary_table can return the raw gtsummary object with mean/SD", {
  result <- summary_table(
    x = "Species", y = "Sepal.Length", data = iris,
    mean = TRUE, flex = FALSE
  )
  expect_s3_class(result, "tbl_summary")
})

test_that("summary_table runs the normality-based test-selection path (3+ groups)", {
  result <- summary_table(
    x = "Species", y = c("Sepal.Width", "Sepal.Length"), data = iris,
    p = TRUE, flex = FALSE
  )
  expect_s3_class(result, "tbl_summary")
})

test_that("summary_table runs the normality-based test-selection path (2 groups)", {
  iris2 <- droplevels(iris[iris$Species != "setosa", ])
  result <- summary_table(
    x = "Species", y = c("Sepal.Width", "Sepal.Length"), data = iris2,
    p = TRUE, flex = FALSE
  )
  expect_s3_class(result, "tbl_summary")
})

test_that("summary_table accepts a parametric override without error", {
  result <- summary_table(
    x = "Species", y = c("Sepal.Width", "Sepal.Length"), data = iris,
    p = TRUE, parametric = "Sepal.Width", flex = FALSE
  )
  expect_s3_class(result, "tbl_summary")
})

test_that("all_groups_normal requires every group to pass to call a variable normal", {
  testthat::local_mocked_bindings(
    shapiro_p_one_group = function(x, y, data, group_value) {
      p <- if (group_value == "setosa") c(a = 0.9, b = 0.01) else c(a = 0.8, b = 0.02)
      data.frame(Variable = names(p), p.value = unname(p))
    }
  )

  result <- all_groups_normal(x = "Species", y = c("a", "b"), data = iris)

  expect_true(result[["a"]])
  expect_false(result[["b"]])
})
