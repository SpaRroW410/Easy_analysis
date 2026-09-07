test_that("check_normality returns one row per variable with expected columns", {
  result <- check_normality(x = "Species", y = c("Sepal.Width", "Sepal.Length"), data = iris)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_setequal(result$Variable, c("Sepal.Width", "Sepal.Length"))
  expect_true(all(c("p.value_dec", "p.value_r") %in% names(result)))
  expect_type(result$p.value_r, "logical")
})
