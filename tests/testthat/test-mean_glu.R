library(iglu)

data_test = c(101,121,141,151,161,171,191,201,231,251)
test_that("mean_glu returns true value for vector", {
  expect_equal(mean_glu(data_test),172, tolerance = 1e-04)
})

num_vec_test = rnorm(10)
test_that('mean_glu equivalent to mean for numeric vectors', {
  expect_equal(mean_glu(num_vec_test), mean(num_vec_test), tolerance = 1e-04)
})
