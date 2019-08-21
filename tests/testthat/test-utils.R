library(iglu)

data_test = c(101,121,141,151,161,171,191,201,231,251)
test_that("read_df_or_vec produces right output class for vector", {
  expect_equal(class(read_df_or_vec(data_test)), 'numeric')
})

test_that("read_df_or_vec produces right output class for dataframe", {
  expect_equal(class(read_df_or_vec(example_data_1_subject)), 'numeric')
})
