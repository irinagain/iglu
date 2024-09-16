test_df <- iglu::example_data_1_subject
test3 <- iglu::example_data_5_subject

# Helper function to extract statistics from agp()
extract_agp_stats <- function(data) {
  subject = unique(data$id)[1]
  out_range = active_percent(data)
  avg_glucose = round(mean_glu(data)$mean)
  gmi_value = round(gmi(data)$GMI, 1)
  cv_value = round(cv_glu(data)$CV, 1)

  list(
    subject = subject,
    active_percent = out_range$active_percent,
    avg_glucose = avg_glucose,
    gmi_value = gmi_value,
    cv_value = cv_value
  )
}

expected_stats_test_df <- extract_agp_stats(test_df)
expected_stats_test3_subject1 <- extract_agp_stats(test3[test3$id == "Subject 1", ])

# Test Function
testthat::test_that("iglu::agp", {

  agp_output_test_df <- agp(test_df, daily = FALSE)
  expect_equal(extract_agp_stats(test_df)$subject, expected_stats_test_df$subject)
  expect_equal(extract_agp_stats(test_df)$active_percent, expected_stats_test_df$active_percent, tolerance = 0.0001)
  expect_equal(extract_agp_stats(test_df)$avg_glucose, expected_stats_test_df$avg_glucose, tolerance = 0.0001)
  expect_equal(extract_agp_stats(test_df)$gmi_value, expected_stats_test_df$gmi_value, tolerance = 0.0001)
  expect_equal(extract_agp_stats(test_df)$cv_value, expected_stats_test_df$cv_value, tolerance = 0.0001)

  agp_output_test3 <- agp(test3[test3$id == "Subject 1", ], daily = FALSE)
  expect_equal(extract_agp_stats(test3[test3$id == "Subject 1", ])$subject, expected_stats_test3_subject1$subject)
  expect_equal(extract_agp_stats(test3[test3$id == "Subject 1", ])$active_percent, expected_stats_test3_subject1$active_percent, tolerance = 0.0001)
  expect_equal(extract_agp_stats(test3[test3$id == "Subject 1", ])$avg_glucose, expected_stats_test3_subject1$avg_glucose, tolerance = 0.0001)
  expect_equal(extract_agp_stats(test3[test3$id == "Subject 1", ])$gmi_value, expected_stats_test3_subject1$gmi_value, tolerance = 0.0001)
  expect_equal(extract_agp_stats(test3[test3$id == "Subject 1", ])$cv_value, expected_stats_test3_subject1$cv_value, tolerance = 0.0001)
})
