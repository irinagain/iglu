library(iglu)

data_test = c(101,121,141,151,161,171,191,201,231,251)
test_that("hbgi returns true value for vector", {
  expect_equal((hbgi(data_test)), 7.806938, tolerance = 1e-04)
})
