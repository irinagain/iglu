library(iglu)

# NOTE- testing only works for mg/dl

data_test = c(101,121,141,151,161,171,191,201,231,251)
test_that("j_index returns true value for vector", {
  expect_equal((j_index(data_test)), 48.06898, tolerance = 1e-04)
})

num_vec_test = rnorm(10)
test_that('j_index equivalent to .001 * (mean+sd)^2 for mg/dl', {
  expect_equal(j_index(num_vec_test), .001* (mean(num_vec_test) + sd(num_vec_test))^2, tolerance = 1e-04)
})
