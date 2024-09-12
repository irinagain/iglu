# Test on one subject data
test1 = iglu::example_data_1_subject
test2 = iglu::example_data_1_subject[1:300, ]
test3 = iglu::example_data_5_subject

out = iglu::gmi(iglu::example_data_5_subject)$GMI

testthat::test_that("gmi works", {
  testthat::expect_equal(iglu::gmi(test1)$GMI[1], 3.31 + 0.02392 * mean(test1$gl, na.rm = T), tolerance = 0.0001)
  testthat::expect_equal(iglu::gmi(test2)$GMI[1], 3.31 + 0.02392 * mean(test2$gl, na.rm = T), tolerance = 0.0001)
  testthat::expect_equal(out[1], 3.31 + 0.02392 * mean(test3[test3$id=="Subject 1", ]$gl, na.rm = TRUE), tolerance = 0.0001)
  testthat::expect_equal(out[2], 3.31 + 0.02392 * mean(test3[test3$id=="Subject 2", ]$gl, na.rm = TRUE), tolerance = 0.0001)
  testthat::expect_equal(out[3], 3.31 + 0.02392 * mean(test3[test3$id=="Subject 3", ]$gl, na.rm = TRUE), tolerance = 0.0001)
  testthat::expect_equal(out[4], 3.31 + 0.02392 * mean(test3[test3$id=="Subject 4", ]$gl, na.rm = TRUE), tolerance = 0.0001)
  testthat::expect_equal(out[5], 3.31 + 0.02392 * mean(test3[test3$id=="Subject 5", ]$gl, na.rm = TRUE), tolerance = 0.0001)
})
