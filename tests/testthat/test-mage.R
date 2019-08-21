library(iglu)

data_test = c(101,121,141,151,161,171,191,201,231,251)
test_that("mage returns true value for vector", {
  expect_equal((mage(data_test)), 65, tolerance = 1e-04)
})
