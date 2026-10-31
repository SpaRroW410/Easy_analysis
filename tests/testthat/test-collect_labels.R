test_that("collect_labels keeps non-blank labels with their variable names", {
  result <- collect_labels(c("a", "b", "c"), c("Alpha", "", "Gamma"))

  expect_equal(result, c(a = "Alpha", c = "Gamma"))
})

test_that("collect_labels trims whitespace and drops whitespace-only entries", {
  result <- collect_labels(c("a", "b"), c("  Alpha  ", "   "))

  expect_equal(result, c(a = "Alpha"))
})

test_that("collect_labels returns NULL when nothing was entered", {
  expect_null(collect_labels(c("a", "b"), c("", " ")))
  expect_null(collect_labels(character(0), character(0)))
})
