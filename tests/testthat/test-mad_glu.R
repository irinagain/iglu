# Test on one subject data
test1 = iglu::example_data_1_subject
test2 = iglu::example_data_1_subject[1:300, ]
test3 = iglu::example_data_5_subject

out = iglu::mad_glu(iglu::example_data_5_subject)$MAD

testthat::test_that("mad_glu works", {
  testthat::expect_equal(iglu::mad_glu(test1)$MAD[1], 1.4826 * median(abs(test1$gl - median(test1$gl))), tolerance = 0.0001)
  testthat::expect_equal(iglu::mad_glu(test2)$MAD[1], 1.4826 * median(abs(test2$gl - median(test2$gl))), tolerance = 0.0001)
  testthat::expect_equal(out[1], 1.4826 * median(abs(test3[test3$id=="Subject 1", ]$gl - median(test3[test3$id=="Subject 1", ]$gl))), tolerance = 0.0001)
  testthat::expect_equal(out[2], 1.4826 * median(abs(test3[test3$id=="Subject 2", ]$gl - median(test3[test3$id=="Subject 2", ]$gl))), tolerance = 0.0001)
  testthat::expect_equal(out[3], 1.4826 * median(abs(test3[test3$id=="Subject 3", ]$gl - median(test3[test3$id=="Subject 3", ]$gl))), tolerance = 0.0001)
  testthat::expect_equal(out[4], 1.4826 * median(abs(test3[test3$id=="Subject 4", ]$gl - median(test3[test3$id=="Subject 4", ]$gl))), tolerance = 0.0001)
  testthat::expect_equal(out[5], 1.4826 * median(abs(test3[test3$id=="Subject 5", ]$gl - median(test3[test3$id=="Subject 5", ]$gl))), tolerance = 0.0001)
})
