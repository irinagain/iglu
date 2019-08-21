library(iglu)

data_test = c(101,121,141,151,161,171,191,201,231,251)
test_that("median_glu returns true value for vector", {
  expect_equal(median_glu(data_test),166, tolerance = 1e-04)
})

num_vec_test = rnorm(10)
test_that('median_glu equivalent to median for numeric vectors', {
  expect_equal(median_glu(num_vec_test), median(num_vec_test), tolerance = 1e-04)
})
