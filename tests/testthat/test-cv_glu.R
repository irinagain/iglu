library(iglu)

data_test = c(101,121,141,151,161,171,191,201,231,251)
test_that("cv_glu returns true value for vector", {
  expect_equal((cv_glu(data_test)),27.46884, tolerance = 1e-04)
})

num_vec_test = rnorm(10)
test_that('cv_glu equivalent to 100*sd/mean for numeric vectors', {
  expect_equal(cv_glu(num_vec_test), 100*sd(num_vec_test)/mean(num_vec_test), tolerance = 1e-04)
})
