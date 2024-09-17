# Test on one subject data
test1 = iglu::example_data_1_subject
test2 = iglu::example_data_1_subject[1:300, ]
test3 = iglu::example_data_5_subject

out = iglu::hyper_index(iglu::example_data_5_subject)$hyper_index

testthat::test_that("hyper_index works", {
  testthat::expect_equal(iglu::hyper_index(test1)$hyper_index[1], sum((test1$gl[test1$gl>140] - 140)^1.1) / length(test1$gl)/30, tolerance = 0.0001)
  testthat::expect_equal(iglu::hyper_index(test2)$hyper_index[1], sum((test2$gl[test2$gl>140] - 140)^1.1) / length(test2$gl)/30, tolerance = 0.0001)
  testthat::expect_equal(out[1], sum((test3[test3$id=="Subject 1", ]$gl[test3[test3$id=="Subject 1", ]$gl>140] - 140)^1.1) / length(test3[test3$id=="Subject 1", ]$gl)/30, tolerance = 0.0001)
  testthat::expect_equal(out[2], sum((test3[test3$id=="Subject 2", ]$gl[test3[test3$id=="Subject 2", ]$gl>140] - 140)^1.1) / length(test3[test3$id=="Subject 2", ]$gl)/30, tolerance = 0.0001)
  testthat::expect_equal(out[3], sum((test3[test3$id=="Subject 3", ]$gl[test3[test3$id=="Subject 3", ]$gl>140] - 140)^1.1) / length(test3[test3$id=="Subject 3", ]$gl)/30, tolerance = 0.0001)
  testthat::expect_equal(out[4], sum((test3[test3$id=="Subject 4", ]$gl[test3[test3$id=="Subject 4", ]$gl>140] - 140)^1.1) / length(test3[test3$id=="Subject 4", ]$gl)/30, tolerance = 0.0001)
  testthat::expect_equal(out[5], sum((test3[test3$id=="Subject 5", ]$gl[test3[test3$id=="Subject 5", ]$gl>140] - 140)^1.1) / length(test3[test3$id=="Subject 5", ]$gl)/30, tolerance = 0.0001)
})

