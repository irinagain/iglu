# Test on one subject data
test1 = iglu::example_data_1_subject
test2 = iglu::example_data_1_subject[1:300, ]
test3 = iglu::example_data_5_subject

out = iglu::sd_glu(iglu::example_data_5_subject)$SD

test_that("iglu::mean_glu == stats::sd", {
  expect_equal(iglu::sd_glu(test1)$SD[1], stats::sd(test1$gl, na.rm = TRUE), tolerance = 0.0001)
  expect_equal(iglu::sd_glu(test2)$SD[1], stats::sd(test2$gl, na.rm = TRUE), tolerance = 0.0001)
  expect_equal(out[1], stats::sd(test3[test3$id=="Subject 1", ]$gl, na.rm = TRUE), tolerance = 0.0001)
  expect_equal(out[2], stats::sd(test3[test3$id=="Subject 2", ]$gl, na.rm = TRUE), tolerance = 0.0001)
  expect_equal(out[3], stats::sd(test3[test3$id=="Subject 3", ]$gl, na.rm = TRUE), tolerance = 0.0001)
  expect_equal(out[4], stats::sd(test3[test3$id=="Subject 4", ]$gl, na.rm = TRUE), tolerance = 0.0001)
  expect_equal(out[5], stats::sd(test3[test3$id=="Subject 5", ]$gl, na.rm = TRUE), tolerance = 0.0001)
})

out_iqr = iglu::iqr_glu(iglu::example_data_5_subject)$IQR

test_that("iglu::iqr == IQR()", {
  expect_equal(iglu::iqr_glu(test1)$IQR[1], IQR(test1$gl, na.rm=TRUE), tolerance = 0.0001)
  expect_equal(iglu::iqr_glu(test2)$IQR[1], IQR(test2$gl, na.rm=TRUE), tolerance = 0.0001)
  expect_equal(out_iqr[1], IQR(test3[test3$id == "Subject 1", ]$gl, na.rm = TRUE), tolerance = 0.0001)
  expect_equal(out_iqr[2], IQR(test3[test3$id == "Subject 2", ]$gl, na.rm = TRUE), tolerance = 0.0001)
  expect_equal(out_iqr[3], IQR(test3[test3$id == "Subject 3", ]$gl, na.rm = TRUE), tolerance = 0.0001)
  expect_equal(out_iqr[4], IQR(test3[test3$id == "Subject 4", ]$gl, na.rm = TRUE), tolerance = 0.0001)
  expect_equal(out_iqr[5], IQR(test3[test3$id == "Subject 5", ]$gl, na.rm = TRUE), tolerance = 0.0001)
})

out_cv = iglu::cv_glu(test3)$CV

test_that("iglu::cv == sd()/mean()", {
  expect_equal(iglu::cv_glu(test1)$CV[1], 100*stats::sd(test1$gl, na.rm=TRUE)/base::mean(test1$gl, na.rm=TRUE), tolerance = 0.0001)
  expect_equal(iglu::cv_glu(test2)$CV[1], 100*stats::sd(test2$gl, na.rm=TRUE)/base::mean(test2$gl, na.rm=TRUE), tolerance = 0.0001)
  expect_equal(out_cv[1], 100*stats::sd(test3[test3$id == "Subject 1", ]$gl, na.rm=TRUE)/base::mean(test3[test3$id == "Subject 1", ]$gl, na.rm=TRUE), tolerance = 0.0001)
  expect_equal(out_cv[2], 100*stats::sd(test3[test3$id == "Subject 2", ]$gl, na.rm=TRUE)/base::mean(test3[test3$id == "Subject 2", ]$gl, na.rm=TRUE), tolerance = 0.0001)
  expect_equal(out_cv[3], 100*stats::sd(test3[test3$id == "Subject 3", ]$gl, na.rm=TRUE)/base::mean(test3[test3$id == "Subject 3", ]$gl, na.rm=TRUE), tolerance = 0.0001)
  expect_equal(out_cv[4], 100*stats::sd(test3[test3$id == "Subject 4", ]$gl, na.rm=TRUE)/base::mean(test3[test3$id == "Subject 4", ]$gl, na.rm=TRUE), tolerance = 0.0001)
  expect_equal(out_cv[5], 100*stats::sd(test3[test3$id == "Subject 5", ]$gl, na.rm=TRUE)/base::mean(test3[test3$id == "Subject 5", ]$gl, na.rm=TRUE), tolerance = 0.0001)
})
