# Helper Function
grade_formula_tester <- function(x){
  grade = (425 * (log10(log10(x/18)) + 0.16)^2)
  grade <- pmin(grade, 50)
  return(grade)
}

# Test Case:
test_df <- data.frame(id = 'test 1', gl = c(100, 120, 150, 200, NA))
test1 = iglu::example_data_1_subject
test2 = iglu::example_data_1_subject[1:300, ]
test3 = iglu::example_data_5_subject

test_out = data.frame(GRADE = mean(grade_formula_tester(test_df$gl)))

out = iglu::grade(test3)$GRADE

result =
  testthat::test_that("iglu::grade", {
    expect_equal(iglu::grade(test1)$GRADE, mean(grade_formula_tester(test1$gl)), tolerance = 0.0001)
    expect_equal(iglu::grade(test2)$GRADE, mean(grade_formula_tester(test2$gl)), tolerance = 0.0001)
    expect_equal(out[1], mean(grade_formula_tester(test3[test3$id=="Subject 1", ]$gl)), tolerance = 0.0001)
    expect_equal(out[2], mean(grade_formula_tester(test3[test3$id=="Subject 2", ]$gl)), tolerance = 0.0001)
    expect_equal(out[3], mean(grade_formula_tester(test3[test3$id=="Subject 3", ]$gl)), tolerance = 0.0001)
    expect_equal(out[4], mean(grade_formula_tester(test3[test3$id=="Subject 4", ]$gl)), tolerance = 0.0001)
    expect_equal(out[5], mean(grade_formula_tester(test3[test3$id=="Subject 5", ]$gl)), tolerance = 0.0001)
  })

