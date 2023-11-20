# Test on one subject data
test_hall = example_data_hall[example_data_hall$id == '2133-018', ]
test_meals = example_meals_hall[example_meals_hall$id == '2133-018', ]
out = meal_metrics(test_hall, test_meals, interpolate = TRUE, adjust_mealtimes = TRUE)


# delta G
test_that("no changes on Hall 2133-010 for delta G", {
  expect_equal(out$deltag[1], 97.8, tolerance = 0.2)
  expect_equal(out$deltag[2], 171, tolerance = 0.2)
  expect_equal(out$deltag[3], 77.6, tolerance = 0.2)
})

# delta T
test_that("no changes on Hall 2133-010 for delta T", {
  expect_equal(out$deltat[1], 80, tolerance = 0.2)
  expect_equal(out$deltat[2], 75, tolerance = 0.2)
  expect_equal(out$deltat[3], 75, tolerance = 0.2)
})

# delta G
test_that("no changes on Hall 2133-010 for baseline recovery", {
  expect_equal(out$basereco[1], 0.378, tolerance = 0.2)
  expect_equal(out$basereco[2], 0.429, tolerance = 0.2)
  expect_equal(out$basereco[3], 0.670, tolerance = 0.2)
})
