# Simple test
test_data <- data.frame(
  id = c(1, 1, 1, 2, 2, 2),
  gl = c(100, 120, NA, 140, 160, 180),
  time = c("08:00", "12:00", "16:00")
)


test_that("range_glu calculates correct range for each id", {
  result <- range_glu(test_data)
  expect_equal(result$range[result$id == 1], 20)
  expect_equal(result$range[result$id == 2], 40)
})

# All values are the same
test_data_same <- data.frame(
  id = c(1, 1, 1),
  gl = c(100, 100, 100),
  time = c("08:00", "12:00", "16:00")
)

test_that("range_glu handles all values the same correctly", {
  result <- range_glu(test_data_same)
  expect_equal(result$range, c(0))
})
