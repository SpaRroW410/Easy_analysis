grouped <- function(n_groups) {
  data.frame(
    g = factor(rep(letters[seq_len(n_groups)], each = 5)),
    a = seq_len(5 * n_groups),
    b = rev(seq_len(5 * n_groups))
  )
}

test_that("choose_tests returns NULL without a grouping variable", {
  expect_null(choose_tests(NULL, c("a", "b"), grouped(3)))
})

test_that("choose_tests returns NULL when no variable is continuous", {
  df <- grouped(3)
  df$f <- factor(rep(c("u", "v"), length.out = nrow(df)))
  df$bin <- rep(c(0, 1), length.out = nrow(df))

  expect_null(choose_tests("g", c("f", "bin"), df))
})

test_that("choose_tests picks aov/kruskal.test for three or more groups", {
  testthat::local_mocked_bindings(
    all_groups_normal = function(x, y, data, alpha = 0.05) c(a = TRUE, b = FALSE)
  )

  result <- choose_tests("g", c("a", "b"), grouped(3))

  expect_equal(deparse(result[[1]]), "a ~ \"aov\"")
  expect_equal(deparse(result[[2]]), "b ~ \"kruskal.test\"")
})

test_that("choose_tests picks t.test/wilcox.test for two groups", {
  testthat::local_mocked_bindings(
    all_groups_normal = function(x, y, data, alpha = 0.05) c(a = TRUE, b = FALSE)
  )

  result <- choose_tests("g", c("a", "b"), grouped(2))

  expect_equal(deparse(result[[1]]), "a ~ \"t.test\"")
  expect_equal(deparse(result[[2]]), "b ~ \"wilcox.test\"")
})

test_that("choose_tests skips the normality check for parametric variables", {
  testthat::local_mocked_bindings(
    all_groups_normal = function(x, y, data, alpha = 0.05) {
      stopifnot(identical(y, "a"))
      c(a = FALSE)
    }
  )

  result <- choose_tests("g", c("a", "b"), grouped(3), parametric = "b")

  expect_equal(deparse(result[[1]]), "a ~ \"kruskal.test\"")
  expect_equal(deparse(result[[2]]), "b ~ \"aov\"")
})
