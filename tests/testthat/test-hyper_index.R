library(iglu)

data_test = c(101,121,141,151,161,171,191,201,231,251)
test_that("hyper_index returns true value for vector", {
  expect_equal((hyper_index(data_test)),8.443412, tolerance = 1e-04)
  expect_equal((hyper_index(c(100, 120, 130, 135, 140))), 0, tolerance = 1e-04)
})
