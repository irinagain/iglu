# Test on one subject data
test1 = iglu::example_data_1_subject
test2 = iglu::example_data_1_subject[1:300, ]
test3 = iglu::example_data_5_subject

out = iglu::hypo_index(iglu::example_data_5_subject)$hypo_index

testthat::test_that("hypo_index works", {
  testthat::expect_equal(iglu::hypo_index(test1)$hypo_index[1], sum((80 - test1$gl[test1$gl<80])^2) / length(test1$gl)/30, tolerance = 0.0001)
  testthat::expect_equal(iglu::hypo_index(test2)$hypo_index[1], sum((80 - test2$gl[test2$gl<80])^2) / length(test2$gl)/30, tolerance = 0.0001)
  testthat::expect_equal(out[1], sum((80 - test3[test3$id=="Subject 1", ]$gl[test3[test3$id=="Subject 1", ]$gl<80])^2) / length(test3[test3$id=="Subject 1", ]$gl)/30, tolerance = 0.0001)
  testthat::expect_equal(out[2], sum((80 - test3[test3$id=="Subject 2", ]$gl[test3[test3$id=="Subject 2", ]$gl<80])^2) / length(test3[test3$id=="Subject 2", ]$gl)/30, tolerance = 0.0001)
  testthat::expect_equal(out[3], sum((80 - test3[test3$id=="Subject 3", ]$gl[test3[test3$id=="Subject 3", ]$gl<80])^2) / length(test3[test3$id=="Subject 3", ]$gl)/30, tolerance = 0.0001)
  testthat::expect_equal(out[4], sum((80 - test3[test3$id=="Subject 4", ]$gl[test3[test3$id=="Subject 4", ]$gl<80])^2) / length(test3[test3$id=="Subject 4", ]$gl)/30, tolerance = 0.0001)
  testthat::expect_equal(out[5], sum((80 - test3[test3$id=="Subject 5", ]$gl[test3[test3$id=="Subject 5", ]$gl<80])^2) / length(test3[test3$id=="Subject 5", ]$gl)/30, tolerance = 0.0001)
})

