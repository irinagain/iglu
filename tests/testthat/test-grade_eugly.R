# Helper Function
grade_eugly_formula_tester <- function(gl, lower = 70, upper = 140){
  grade = grade_formula_tester(gl)
  grade_eugly = sum(grade[gl >= lower & gl <= upper ], na.rm = TRUE) /
    sum(grade, na.rm = TRUE) * 100
  return(grade_eugly)
}

# Test Case:
test_df <- data.frame(id = 'test 1', gl = c(100, 120, 150, 200, NA))
test1 = iglu::example_data_1_subject
test2 = iglu::example_data_1_subject[1:300, ]
test3 = iglu::example_data_5_subject

test_out = data.frame(GRADE_eugly = mean(grade_eugly_formula_tester(test_df$gl)))

out = iglu::grade_eugly(test3)$GRADE_eugly


outresult =
  testthat::test_that("iglu::grade_eugly", {
    expect_equal(iglu::grade_eugly(test_df$gl)$GRADE_eugly, test_out$GRADE_eugly, tolerance = 0.0001)
    expect_equal(iglu::grade_eugly(test1)$GRADE_eugly, mean(grade_eugly_formula_tester(test1$gl)), tolerance = 0.0001)
    expect_equal(iglu::grade_eugly(test2)$GRADE_eugly, mean(grade_eugly_formula_tester(test2$gl)), tolerance = 0.0001)
    expect_equal(out[1], mean(grade_eugly_formula_tester(test3[test3$id=="Subject 1", ]$gl)), tolerance = 0.0001)
    expect_equal(out[2], mean(grade_eugly_formula_tester(test3[test3$id=="Subject 2", ]$gl)), tolerance = 0.0001)
    expect_equal(out[3], mean(grade_eugly_formula_tester(test3[test3$id=="Subject 3", ]$gl)), tolerance = 0.0001)
    expect_equal(out[4], mean(grade_eugly_formula_tester(test3[test3$id=="Subject 4", ]$gl)), tolerance = 0.0001)
    expect_equal(out[5], mean(grade_eugly_formula_tester(test3[test3$id=="Subject 5", ]$gl)), tolerance = 0.0001)
  })

