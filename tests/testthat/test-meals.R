# Test on one subject data
test_hall = example_data_hall[example_data_hall$id == '2133-010', ]
test_meals = example_meals_hall[example_meals_hall$id == '2133-010', ]
out = meal_metrics(test_hall, test_meals, interpolate = TRUE, adjust_mealtimes = TRUE)


# delta G
test_that("no changes on Hall 2133-010 for delta G", {
  expect_equal(out$deltag[1], 42.1, tolerance = 0.2)
  expect_equal(out$deltag[2], 25.0, tolerance = 0.2)
  expect_equal(out$deltag[3], 37.2, tolerance = 0.2)
})

# delta T
test_that("no changes on Hall 2133-010 for delta T", {
  expect_equal(out$deltat[1], 60, tolerance = 0.2)
  expect_equal(out$deltat[2], 45, tolerance = 0.2)
  expect_equal(out$deltat[3], 155, tolerance = 0.2)
})

# delta G
test_that("no changes on Hall 2133-010 for baseline recovery", {
  expect_equal(out$basereco[1], 0.741, tolerance = 0.2)
  expect_equal(out$basereco[2], 1.88, tolerance = 0.2)
  expect_equal(out$basereco[3], 0.918, tolerance = 0.2)
})
