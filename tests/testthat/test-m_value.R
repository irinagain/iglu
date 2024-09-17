# Test on one subject data
test1 = iglu::example_data_1_subject
test2 = iglu::example_data_1_subject[1:300, ]
test3 = iglu::example_data_5_subject

out = iglu::m_value(iglu::example_data_5_subject)$M_value

testthat::test_that("m_value works", {
  testthat::expect_equal(iglu::m_value(test1)$M_value[1], mean(abs(10 * log10((test1$gl)/90))^3), tolerance = 0.0001)
  testthat::expect_equal(iglu::m_value(test2)$M_value[1], mean(abs(10 * log10((test2$gl)/90))^3), tolerance = 0.0001)
  testthat::expect_equal(out[1], mean(abs(10 * log10((test3[test3$id=="Subject 1", ]$gl)/90))^3), tolerance = 0.0001)
  testthat::expect_equal(out[2], mean(abs(10 * log10((test3[test3$id=="Subject 2", ]$gl)/90))^3), tolerance = 0.0001)
  testthat::expect_equal(out[3], mean(abs(10 * log10((test3[test3$id=="Subject 3", ]$gl)/90))^3), tolerance = 0.0001)
  testthat::expect_equal(out[4], mean(abs(10 * log10((test3[test3$id=="Subject 4", ]$gl)/90))^3), tolerance = 0.0001)
  testthat::expect_equal(out[5], mean(abs(10 * log10((test3[test3$id=="Subject 5", ]$gl)/90))^3), tolerance = 0.0001)
})
