library(iglu)

data_test = c(40,50,100,150,200)
test_that("grade_hyper returns true value for vector", {
  expect_equal((grade_hyper(data_test)), 27.08213, tolerance = 1e-04)
})
