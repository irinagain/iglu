# Test on one subject data
test1 = iglu::example_data_1_subject
test2 = iglu::example_data_1_subject[1:300, ]
test3 = iglu::example_data_5_subject

out = iglu::hbgi(iglu::example_data_5_subject)$HBGI


fg = c()
for(g in test1$gl){fg = c(fg, max(0, 1.509 * ((log(g)^1.084) - 5.381)))}
first_test = (1/length(test1$gl)) * sum(10 * fg^2)

fg = c()
for(g in test2$gl){fg = c(fg, max(0, 1.509 * ((log(g)^1.084) - 5.381)))}
second_test = (1/length(test2$gl)) * sum(10 * fg^2)

five_tests = c()

for(sub in unique(test3$id)){
  fg = c()
  for(g in test3[test3$id == sub,]$gl){fg = c(fg, max(0, 1.509 * ((log(g)^1.084) - 5.381)))}
  five_tests = c(five_tests, ((1/length(test3[test3$id == sub,]$gl)) * sum(10 * fg^2)))
}

testthat::test_that("hbgi works", {
  testthat::expect_equal(iglu::hbgi(test1)$HBGI[1], first_test, tolerance = 0.0001)
  testthat::expect_equal(iglu::hbgi(test2)$HBGI[1], second_test, tolerance = 0.0001)
  testthat::expect_equal(out[1], five_tests[1], tolerance = 0.0001)
  testthat::expect_equal(out[2], five_tests[2], tolerance = 0.0001)
  testthat::expect_equal(out[3], five_tests[3], tolerance = 0.0001)
  testthat::expect_equal(out[4], five_tests[4], tolerance = 0.0001)
  testthat::expect_equal(out[5], five_tests[5], tolerance = 0.0001)
})
