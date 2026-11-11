test_that("check_normality returns one row per variable with expected columns", {
  result <- check_normality(x = "Species", y = c("Sepal.Width", "Sepal.Length"), data = iris)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_setequal(result$Variable, c("Sepal.Width", "Sepal.Length"))
  expect_true(all(c("p.value_dec", "p.value_r") %in% names(result)))
  expect_type(result$p.value_r, "logical")
})

test_that("check_normality ignores missing values", {
  df <- iris
  df$Sepal.Length[c(1, 2)] <- NA
  result <- check_normality(x = "Species", y = "Sepal.Length", data = df)

  expect_equal(nrow(result), 1)
  expect_false(is.na(result$p.value_r))
})

test_that("normality checks do not warn when columns carry different labels", {
  df <- iris
  Hmisc::label(df$Sepal.Width) <- "Sepal width"
  Hmisc::label(df$Sepal.Length) <- "Sepal length"

  expect_no_warning(check_normality("Species", c("Sepal.Width", "Sepal.Length"), df))
  expect_no_warning(all_groups_normal("Species", c("Sepal.Width", "Sepal.Length"), df))
})
