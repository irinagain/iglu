library(iglu)

data_test = c(101,121,141,151,161,171,191,201,231,251)
test_that("iqr_glu equivalent to IQR()", {
  expect_equal((iqr_glu(data_test)), IQR(data_test), tolerance = 1e-04)
  expect_equal((iqr_glu(example_data_1_subject)), IQR(example_data_1_subject$gl), tolerance = 1e-04)
})
