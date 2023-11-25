# Test on one subject data
test1 = iglu::example_data_1_subject
test2 = iglu::example_data_1_subject[1:300, ]
test3 = iglu::example_data_5_subject

out = iglu::median_glu(iglu::example_data_5_subject)$median

test_that("iglu::mean_glu == base::mean", {
  expect_equal(iglu::median_glu(test1)$median[1], stats::median(test1$gl, na.rm = TRUE), tolerance = 0.0001)
  expect_equal(iglu::median_glu(test2)$median[1], stats::median(test2$gl, na.rm = TRUE), tolerance = 0.0001)
  expect_equal(out[1], stats::median(test3[test3$id=="Subject 1", ]$gl, na.rm = TRUE), tolerance = 0.0001)
  expect_equal(out[2], stats::median(test3[test3$id=="Subject 2", ]$gl, na.rm = TRUE), tolerance = 0.0001)
  expect_equal(out[3], stats::median(test3[test3$id=="Subject 3", ]$gl, na.rm = TRUE), tolerance = 0.0001)
  expect_equal(out[4], stats::median(test3[test3$id=="Subject 4", ]$gl, na.rm = TRUE), tolerance = 0.0001)
  expect_equal(out[5], stats::median(test3[test3$id=="Subject 5", ]$gl, na.rm = TRUE), tolerance = 0.0001)
})
