library(iglu)

data_test = c(101,121,141,151,161,171,191,201,231,251)
test_that("in_range_percent returns true percent for default", {
  expect_equal(as.numeric(in_range_percent(data_test)),c(70,60,20))
})

test_that("in_range_percent returns true for alternate targets", {
  expect_equal(as.numeric(in_range_percent(data_test, targets = list(c(200, 100), c(250,150), c(0,1000)))),
               c(70,60,100))
})

