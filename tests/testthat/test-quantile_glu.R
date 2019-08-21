library(iglu)

data_test = c(101,121,141,151,161,171,191,201,231,251)
test_that("quantile_glu equivalent to quantile()", {
  expect_equal((quantile_glu(data_test)), quantile(data_test), tolerance = 1e-04)
  expect_equal((quantile_glu(example_data_1_subject)), quantile(example_data_1_subject$gl), tolerance = 1e-04)
})
