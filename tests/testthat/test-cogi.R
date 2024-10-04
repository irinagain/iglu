# Test on one subject data
test1 = iglu::example_data_1_subject
test2 = iglu::example_data_1_subject[1:300, ]
test3 = iglu::example_data_5_subject

out = iglu::cogi(iglu::example_data_5_subject)$COGI


test_cogi = function(data, targets = c(70, 180), weights = c(.5, .35, .15)){
  ir = in_range_percent(data, list(targets))[[2]] * weights[1]
  br = max(min(((-20/3) * below_percent(data, targets_below = targets[1])[[2]] + 100), 100), 0) * weights[2]
  st = max(min(((-10/9) * sd_glu(data)[[2]] + 120), 100), 0) * weights[3]
  cogi = ir + br + st
  return(cogi)
}

testthat::test_that("cogi works", {
  testthat::expect_equal(iglu::cogi(test1)$COGI[1], test_cogi(test1), tolerance = 0.0001)
  testthat::expect_equal(iglu::cogi(test1, targets = c(80, 150), weights = c(0.2,0.6,0.2))$COGI[1], test_cogi(test1, targets = c(80, 150), weights = c(0.2,0.6,0.2)), tolerance = 0.0001)
  testthat::expect_equal(iglu::cogi(test2)$COGI[1], test_cogi(test2), tolerance = 0.0001)
  testthat::expect_equal(out[1], test_cogi(test3[test3$id=='Subject 1',]), tolerance = 0.0001)
  testthat::expect_equal(out[2], test_cogi(test3[test3$id=='Subject 2',]), tolerance = 0.0001)
  testthat::expect_equal(out[3], test_cogi(test3[test3$id=='Subject 3',]), tolerance = 0.0001)
  testthat::expect_equal(out[4], test_cogi(test3[test3$id=='Subject 4',]), tolerance = 0.0001)
  testthat::expect_equal(out[5], test_cogi(test3[test3$id=='Subject 5',]), tolerance = 0.0001)
})







