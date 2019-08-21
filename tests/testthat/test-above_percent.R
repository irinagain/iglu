library(iglu)

data_test = c(101,121,141,151,161,171,191,201,231,251)
test_that("above_percent returns true percent for default", {
  expect_equal(as.numeric(above_percent(data_test)),c(80,40,30,10))
})

test_that("above_percent returns true for alternate targets", {
  expect_equal(as.numeric(above_percent(data_test, targets = c(0,100,102,150,151,1000))),
               c(100,100,90,70,60,0))
})

