# Test on one subject data
test1 = iglu::example_data_1_subject
test2 = iglu::example_data_1_subject[1:300, ]
test3 = iglu::example_data_5_subject

test5 = iglu::example_data_5_subject[iglu::example_data_5_subject$id == "Subject 5",]
out = iglu::gri(iglu::example_data_5_subject)$GRI

testthat::test_that("gri works", {
  testthat::expect_equal(iglu::gri(test1)$GRI[1],
                           (3 * iglu::agp_metrics(test1)$below_54) + (2.4 * (iglu::agp_metrics(test1)$below_70 - iglu::agp_metrics(test1)$below_54)) +
                             (1.6 * iglu::agp_metrics(test1)$above_250) + (0.8 * (iglu::agp_metrics(test1)$above_180 - iglu::agp_metrics(test1)$above_250)), tolerance = 0.0001)
  testthat::expect_equal(iglu::gri(test2)$GRI[1],
                         (3 * iglu::agp_metrics(test2)$below_54) + (2.4 * (iglu::agp_metrics(test2)$below_70 - iglu::agp_metrics(test1)$below_54)) +
                           (1.6 * iglu::agp_metrics(test2)$above_250) + (0.8 * (iglu::agp_metrics(test2)$above_180 - iglu::agp_metrics(test2)$above_250)), tolerance = 0.0001)
  testthat::expect_equal(out[1],
                         (3 * iglu::agp_metrics(test3[test3$id=="Subject 1", ])$below_54) +
                           (2.4 * (iglu::agp_metrics(test3[test3$id=="Subject 1", ])$below_70 -
                                     iglu::agp_metrics(test3[test3$id=="Subject 1", ])$below_54)) +
                           (1.6 * iglu::agp_metrics(test3[test3$id=="Subject 1", ])$above_250) +
                           (0.8 * (iglu::agp_metrics(test3[test3$id=="Subject 1", ])$above_180 -
                                     iglu::agp_metrics(test3[test3$id=="Subject 1", ])$above_250)), tolerance = 0.0001)
  testthat::expect_equal(out[2],
                         (3 * iglu::agp_metrics(test3[test3$id=="Subject 2", ])$below_54) +
                           (2.4 * (iglu::agp_metrics(test3[test3$id=="Subject 2", ])$below_70 -
                                     iglu::agp_metrics(test3[test3$id=="Subject 2", ])$below_54)) +
                           (1.6 * iglu::agp_metrics(test3[test3$id=="Subject 2", ])$above_250) +
                           (0.8 * (iglu::agp_metrics(test3[test3$id=="Subject 2", ])$above_180 -
                                     iglu::agp_metrics(test3[test3$id=="Subject 2", ])$above_250)), tolerance = 0.0001)
  testthat::expect_equal(out[3],
                         (3 * iglu::agp_metrics(test3[test3$id=="Subject 3", ])$below_54) +
                           (2.4 * (iglu::agp_metrics(test3[test3$id=="Subject 3", ])$below_70 -
                                     iglu::agp_metrics(test3[test3$id=="Subject 3", ])$below_54)) +
                           (1.6 * iglu::agp_metrics(test3[test3$id=="Subject 3", ])$above_250) +
                           (0.8 * (iglu::agp_metrics(test3[test3$id=="Subject 3", ])$above_180 -
                                     iglu::agp_metrics(test3[test3$id=="Subject 3", ])$above_250)), tolerance = 0.0001)
  testthat::expect_equal(out[4],
                         (3 * iglu::agp_metrics(test3[test3$id=="Subject 4", ])$below_54) +
                           (2.4 * (iglu::agp_metrics(test3[test3$id=="Subject 4", ])$below_70 -
                                     iglu::agp_metrics(test3[test3$id=="Subject 4", ])$below_54)) +
                           (1.6 * iglu::agp_metrics(test3[test3$id=="Subject 4", ])$above_250) +
                           (0.8 * (iglu::agp_metrics(test3[test3$id=="Subject 4", ])$above_180 -
                                     iglu::agp_metrics(test3[test3$id=="Subject 4", ])$above_250)), tolerance = 0.0001)
  testthat::expect_equal(out[5],
                         (3 * iglu::agp_metrics(test3[test3$id=="Subject 5", ])$below_54) +
                           (2.4 * (iglu::agp_metrics(test3[test3$id=="Subject 5", ])$below_70 -
                                     iglu::agp_metrics(test3[test3$id=="Subject 5", ])$below_54)) +
                           (1.6 * iglu::agp_metrics(test3[test3$id=="Subject 5", ])$above_250) +
                           (0.8 * (iglu::agp_metrics(test3[test3$id=="Subject 5", ])$above_180 -
                                     iglu::agp_metrics(test3[test3$id=="Subject 5", ])$above_250)), tolerance = 0.0001)
})


