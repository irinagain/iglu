library(iglu)

data_test = c(101,121,141,151,161,171,191,201,231,251)
test_that("range_glu equivalent to max - min", {
  expect_equal((range_glu(data_test)), max(data_test) - min(data_test), tolerance = 1e-04)
  expect_equal((range_glu(example_data_1_subject)), max(example_data_1_subject$gl) - min(example_data_1_subject$gl), tolerance = 1e-04)
})
