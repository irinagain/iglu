library(iglu)

data_test = c(101,121,141,151,161,171,191,201,231,251)
test_that("igc returns true value for vector", {
  expect_equal((igc(data_test)),8.443412, tolerance = 1e-04)
  expect_equal((igc(c(40,60, data_test[3:10]))), 25.77675, tolerance = 1e-04)

})
num_vec_test = rnorm(10)
test_that("igc returns same value as hypo + hyper", {
  expect_equal((igc(num_vec_test)), (hypo_index(num_vec_test) + hyper_index(num_vec_test)), tolerance = 1e-04)
})
