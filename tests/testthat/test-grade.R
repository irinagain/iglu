library(iglu)

data_test = c(101,121,141,151,161,171,191,201,231,251)
test_that("grade returns true value for vector", {
  expect_equal((grade(data_test)), 9.713811, tolerance = 1e-04)
})
