test_df <- iglu::example_data_1_subject
test3 <- iglu::example_data_5_subject

# Helper function to extract important statistics from agp_metrics()
extract_agp_metrics <- function(data) {
  activity <- active_percent(data)
  avg_glucose <- mean_glu(data)
  gmi_value <- gmi(data)
  cv_value <- cv_glu(data)
  percent_below <- below_percent(data, targets_below = c(54, 70))
  percent_in_range <- in_range_percent(data, target_ranges = list(c(70, 180)))
  percent_above <- above_percent(data, targets_above = c(180, 250))

  list(
    active_percent = activity$active_percent,
    avg_glucose = avg_glucose$mean,
    gmi_value = gmi_value$GMI,
    cv_value = cv_value$CV,
    percent_below_54 = percent_below[, 2]$below_54,
    percent_below_70 = percent_below[, 3]$below_70,
    percent_in_range = percent_in_range[, 2]$in_range_70_180,
    percent_above_180 = percent_above[, 2]$above_180,
    percent_above_250 = percent_above[, 3]$above_250
  )
}

expected_metrics_test_df <- extract_agp_metrics(test_df)
expected_metrics_test3_subject1 <- extract_agp_metrics(test3[test3$id == "Subject 1", ])

# Test Function
testthat::test_that("iglu::agp_metrics", {
  # agp_metrics_output_test_df <- iglu::agp_metrics(test_df)
  # expect_equal(agp_metrics_output_test_df$active_percent, expected_metrics_test_df$active_percent, tolerance = 0.0001)
  # expect_equal(agp_metrics_output_test_df$mean, expected_metrics_test_df$avg_glucose, tolerance = 0.0001)
  # expect_equal(agp_metrics_output_test_df$GMI, expected_metrics_test_df$gmi_value, tolerance = 0.0001)
  # expect_equal(agp_metrics_output_test_df$CV, expected_metrics_test_df$cv_value, tolerance = 0.0001)
  # expect_equal(agp_metrics_output_test_df$below_54[1], expected_metrics_test_df$percent_below_54, tolerance = 0.0001)
  # expect_equal(agp_metrics_output_test_df$below_70[2], expected_metrics_test_df$percent_below_70, tolerance = 0.0001)
  # expect_equal(agp_metrics_output_test_df$in_range_70_180, expected_metrics_test_df$percent_in_range, tolerance = 0.0001)
  # expect_equal(agp_metrics_output_test_df$above_180[1], expected_metrics_test_df$percent_above_180, tolerance = 0.0001)
  # expect_equal(agp_metrics_output_test_df$above_250[2], expected_metrics_test_df$percent_above_250, tolerance = 0.0001)

  agp_metrics_output_test3 <- iglu::agp_metrics(test3[test3$id == "Subject 1", ])
  expect_equal(agp_metrics_output_test3$active_percent, expected_metrics_test3_subject1$active_percent, tolerance = 0.0001)
  expect_equal(agp_metrics_output_test3$mean, expected_metrics_test3_subject1$avg_glucose, tolerance = 0.0001)
  expect_equal(agp_metrics_output_test3$GMI, expected_metrics_test3_subject1$gmi_value, tolerance = 0.0001)
  expect_equal(agp_metrics_output_test3$CV, expected_metrics_test3_subject1$cv_value, tolerance = 0.0001)
  expect_equal(agp_metrics_output_test3$below_54, expected_metrics_test3_subject1$percent_below_54, tolerance = 0.0001)
  expect_equal(agp_metrics_output_test3$below_70, expected_metrics_test3_subject1$percent_below_70, tolerance = 0.0001)
  expect_equal(agp_metrics_output_test3$in_range_70_180, expected_metrics_test3_subject1$percent_in_range, tolerance = 0.0001)
  expect_equal(agp_metrics_output_test3$above_180, expected_metrics_test3_subject1$percent_above_180, tolerance = 0.0001)
  expect_equal(agp_metrics_output_test3$above_250, expected_metrics_test3_subject1$percent_above_250, tolerance = 0.0001)


})
