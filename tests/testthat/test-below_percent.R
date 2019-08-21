library(iglu)

data_test = c(101,121,141,151,161,171,191,201,231,251)
test_that("below_percent returns true percent for default", {
  expect_equal(as.numeric(below_percent(data_test)),c(0,0))
  expect_equal(as.numeric(below_percent(c(40,60,75,80,100))),c(20,60))
})

test_that("below_percent returns true for alternate targets", {
  expect_equal(as.numeric(below_percent(data_test, targets = c(0,100,102,150,151,1000))),
               c(0,0,10,30,30,100))
})

