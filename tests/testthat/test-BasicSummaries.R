
# Test on toy example
test_data = c(100, 120, 150)
test_that("above percent works", {
  expect_equal(above_percent(test_data)$above_180, 0)
  expect_equal(above_percent(test_data)$above_250, 0)
  expect_equal(above_percent(test_data)$above_140, 100/3)
})

test_that("below percent works", {
  expect_equal(below_percent(test_data)$below_70, 0)
  expect_equal(below_percent(test_data)$below_54, 0)
  expect_equal(below_percent(test_data, targets_below = c(100, 150))$below_150, 200/3)
})

test_that("in range percent works", {
  expect_equal(in_range_percent(test_data)$in_range_70_180, 100)
  expect_equal(in_range_percent(test_data)$in_range_63_140, 200/3)
})

# Test on one subject data
out = in_range_percent(example_data_1_subject)
test_that("no changes on 1 subject data for ranges", {
  expect_equal(out$in_range_63_140, 73.9, tolerance = 0.2)
  expect_equal(out$in_range_70_180, 91.7, tolerance = 0.2)
})
