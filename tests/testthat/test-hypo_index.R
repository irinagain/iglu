library(iglu)

data_test = c(101,121,141,151,161,171,191,201,231,251)
test_that("hypo_index returns true value for vector", {
  expect_equal((hypo_index(data_test)),0, tolerance = 1e-04)
  expect_equal((hypo_index(c(40,60, data_test[3:10]))), 17.33333, tolerance = 1e-04)
})
