test_that("run_app is exported", {
  expect_true("run_app" %in% getNamespaceExports("easyanalysis"))
})

test_that("the app bundled with the package exists", {
  expect_true(file.exists(system.file("app", "app.R", package = "easyanalysis")))
})
