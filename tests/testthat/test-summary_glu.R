library(iglu)

data_test = c(101,121,141,151,161,171,191,201,231,251)
test_that("summary_glu equivalent to summary for vectors", {
  expect_equal((summary_glu(data_test)), summary(data_test), tolerance = 1e-04)
})

test_that("summary_glu is equivalent to summary(data$gl) for dataframes", {
  expect_equal((summary_glu(example_data_1_subject)), summary(example_data_1_subject$gl), tolerance = 1e-04)
})
