library(iglu)

test_that("modd returns true value for dataframe", {
  expect_equal((modd(example_data_1_subject)), 27.88716, tolerance = 1e-04)
})
