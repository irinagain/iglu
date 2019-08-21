library(iglu)

data_test = c(101,121,141,151,161,171,191,201,231,251)
test_that("lbgi returns true value for vector", {
  expect_equal((lbgi(data_test)),0.04042143, tolerance = 1e-04)
})
