library(iglu)

data_test = c(40,50,100,150,200)
test_that("grade_hypo returns true value for vector", {
  expect_equal((grade_hypo(data_test)), 72.33554, tolerance = 1e-04)
})
