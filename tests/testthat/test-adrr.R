library(iglu)

#data is example 1 subject data
test_that("adrr returns true value for dataframe", {
  expect_equal((adrr(example_data_1_subject)), 15.13737, tolerance = 1e-04)
})
