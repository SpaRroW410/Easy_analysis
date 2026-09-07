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
