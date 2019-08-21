library(iglu)

data_test = c(101,121,141,151,161,171,191,201,231,251)
test_that("sd_glu returns true value for vector", {
  expect_equal(sd_glu(data_test),47.2464, tolerance = 1e-04)
})

num_vec_test = rnorm(10)
test_that('sd_glu equivalent to sd for numeric vectors', {
  expect_equal(sd_glu(num_vec_test), sd(num_vec_test), tolerance = 1e-04)
})
