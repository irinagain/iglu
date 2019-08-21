library(iglu)

test_that("conga returns true value for dataframe", {
  expect_equal((conga(example_data_1_subject)), 36.86259, tolerance = 1e-04)
})
