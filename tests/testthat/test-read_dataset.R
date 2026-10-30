test_that("read_dataset reads a CSV and converts character columns to factors", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  utils::write.csv(
    data.frame(group = c("a", "b", "a"), value = c(1.5, 2.5, 3.5)),
    path, row.names = FALSE
  )

  result <- read_dataset(path)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_s3_class(result$group, "factor")
  expect_type(result$value, "double")
})

test_that("read_dataset uses the original file name to pick the reader", {
  path <- tempfile(fileext = ".tmp")
  on.exit(unlink(path))
  utils::write.csv(data.frame(a = 1:2), path, row.names = FALSE)

  result <- read_dataset(path, name = "upload.csv")

  expect_equal(nrow(result), 2)
})

test_that("read_dataset rejects unsupported file types", {
  path <- tempfile(fileext = ".txt")
  on.exit(unlink(path))
  writeLines("a,b", path)

  expect_error(read_dataset(path), "Unsupported file type")
})
