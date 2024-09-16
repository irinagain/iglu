# Helper Function
grade_hypo_formula_tester <- function(gl, lower = 80){
  grade = grade_formula_tester(gl)
  GRADE_hypo = sum(grade[gl < lower], na.rm = TRUE) /
    sum(grade, na.rm = TRUE) * 100
  return(GRADE_hypo)
}

# Test Case:
test_df <- data.frame(id = 'test 1', gl = c(100, 120, 150, 200, NA))
test1 = iglu::example_data_1_subject
test2 = iglu::example_data_1_subject[1:300, ]
test3 = iglu::example_data_5_subject

test_out = data.frame(GRADE_hyper = mean(grade_hypo_formula_tester(test_df$gl)))

out = iglu::grade_hypo(test3)$GRADE_hypo


outresult =
  testthat::test_that("iglu::grade_hypo", {
    expect_equal(iglu::grade_hypo(test_df$gl)$GRADE_hypo, test_out$GRADE_hyper, tolerance = 0.0001)
    expect_equal(iglu::grade_hypo(test1)$GRADE_hypo, mean(grade_hypo_formula_tester(test1$gl)), tolerance = 0.0001)
    expect_equal(iglu::grade_hypo(test2)$GRADE_hypo, mean(grade_hypo_formula_tester(test2$gl)), tolerance = 0.0001)
    expect_equal(out[1], mean(grade_hypo_formula_tester(test3[test3$id=="Subject 1", ]$gl)), tolerance = 0.0001)
    expect_equal(out[2], mean(grade_hypo_formula_tester(test3[test3$id=="Subject 2", ]$gl)), tolerance = 0.0001)
    expect_equal(out[3], mean(grade_hypo_formula_tester(test3[test3$id=="Subject 3", ]$gl)), tolerance = 0.0001)
    expect_equal(out[4], mean(grade_hypo_formula_tester(test3[test3$id=="Subject 4", ]$gl)), tolerance = 0.0001)
    expect_equal(out[5], mean(grade_hypo_formula_tester(test3[test3$id=="Subject 5", ]$gl)), tolerance = 0.0001)
  })

