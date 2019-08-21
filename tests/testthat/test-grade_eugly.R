library(iglu)

data_test = c(40,50,100,150,200)
test_that("grade_eugly returns true value for vector", {
  expect_equal((grade_eugly(data_test)), 0.5823323, tolerance = 1e-04)
})
