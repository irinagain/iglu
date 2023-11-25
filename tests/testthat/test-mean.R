# Test on one subject data
test1 = iglu::example_data_1_subject
test2 = iglu::example_data_1_subject[1:300, ]
test3 = iglu::example_data_5_subject

out = iglu::mean_glu(iglu::example_data_5_subject)$mean

test_that("iglu::mean_glu == base::mean", {
  expect_equal(iglu::mean_glu(test1)$mean[1], base::mean(test1$gl, na.rm = TRUE), tolerance = 0.0001)
  expect_equal(iglu::mean_glu(test2)$mean[1], base::mean(test2$gl, na.rm = TRUE), tolerance = 0.0001)
  expect_equal(out[1], base::mean(test3[test3$id=="Subject 1", ]$gl, na.rm = TRUE), tolerance = 0.0001)
  expect_equal(out[2], base::mean(test3[test3$id=="Subject 2", ]$gl, na.rm = TRUE), tolerance = 0.0001)
  expect_equal(out[3], base::mean(test3[test3$id=="Subject 3", ]$gl, na.rm = TRUE), tolerance = 0.0001)
  expect_equal(out[4], base::mean(test3[test3$id=="Subject 4", ]$gl, na.rm = TRUE), tolerance = 0.0001)
  expect_equal(out[5], base::mean(test3[test3$id=="Subject 5", ]$gl, na.rm = TRUE), tolerance = 0.0001)
})
