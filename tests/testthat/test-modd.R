# Test on one subject data
test1 = iglu::example_data_1_subject
test2 = iglu::example_data_1_subject[1:300, ]
test3 = iglu::example_data_5_subject

out = iglu::modd(iglu::example_data_5_subject)$MODD


test_modd = function(data, lag = 1){
  data_ip <- CGMS2DayByDay(data)
  gl_by_day <- data_ip$gd2d
  modd <- mean(abs(diff(gl_by_day, lag = lag)), na.rm = TRUE)
  return(modd)
}

testthat::test_that("modd works", {
  testthat::expect_equal(iglu::modd(test1)$MODD[1], test_modd(test1), tolerance = 0.0001)
  testthat::expect_equal(iglu::modd(test2)$MODD[1], test_modd(test2), tolerance = 0.0001)
  testthat::expect_equal(out[1], test_modd(test3[test3$id=='Subject 1',]), tolerance = 0.0001)
  testthat::expect_equal(out[2], test_modd(test3[test3$id=='Subject 2',]), tolerance = 0.0001)
  testthat::expect_equal(out[3], test_modd(test3[test3$id=='Subject 3',]), tolerance = 0.0001)
  testthat::expect_equal(out[4], test_modd(test3[test3$id=='Subject 4',]), tolerance = 0.0001)
  testthat::expect_equal(out[5], test_modd(test3[test3$id=='Subject 5',]), tolerance = 0.0001)
})







