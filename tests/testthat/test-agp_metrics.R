test3 <- iglu::example_data_5_subject

# Helper function to extract important statistics from agp_metrics()
extract_agp_metrics <- function(data, shinyformat=FALSE) {

  activity <- active_percent(data)
  avg_glucose <- mean_glu(data)
  gmi_value <- gmi(data)
  cv_value <- cv_glu(data)
  percent_below <- below_percent(data, targets_below = c(54, 70))
  percent_in_range <- in_range_percent(data, target_ranges = list(c(70, 180)))
  percent_above <- above_percent(data, targets_above = c(180, 250))

  if(shinyformat == FALSE){
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
  } else {
    tibble::tibble(
      metric = c("Subject ID", "Start Date", "End Date", "Duration",
                 "% Time CGM is Active", "Average Glucose", "GMI", "CV"),
      value = c(as.character(data$id), as.character(as.Date(activity$start_date)),
                as.character(as.Date(activity$end_date)), paste(as.character(activity$ndays), "days"),
                paste0(round(activity$active_percent, 1), "%"), paste(round(avg_glucose$mean), "mg/dL"),
                paste0(round(gmi_value$GMI, 1), "%"), paste0(round(cv_value$CV, 1), "%"))
    )
  }
}

# Expected outputs for tests
expected_metrics_test3_subject1 <- extract_agp_metrics(test3[test3$id == "Subject 1", ])
expected_metrics_test3_all <- extract_agp_metrics(test3)
agp_metrics_output_test_shiny <- iglu::agp_metrics(test3[test3$id == "Subject 1", ], shinyformat = TRUE)
agp_metrics_output_test_all <- iglu::agp_metrics(test3)
agp_metrics_output_test_all_shiny <- iglu::agp_metrics(test3, shinyformat = TRUE)

# Test Function
testthat::test_that("iglu::agp_metrics", {

  # Test default format for single subject
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

  # Test shiny format for single subject
  agp_metrics_output_test_shiny <- iglu::agp_metrics(test3[test3$id == "Subject 1", ], shinyformat = TRUE)
  expect_equal(agp_metrics_output_test_shiny$metric, agp_metrics_output_test_shiny$metric)
  expect_equal(agp_metrics_output_test_shiny$value, agp_metrics_output_test_shiny$value)

  # Test default format for all subjects
  agp_metrics_output_test_all <- iglu::agp_metrics(test3)
  expect_equal(agp_metrics_output_test_all$active_percent, expected_metrics_test3_all$active_percent, tolerance = 0.0001)

  # Test shiny format for all subjects
  agp_metrics_output_test_all_shiny <- iglu::agp_metrics(test3, shinyformat = TRUE)
  expect_equal(agp_metrics_output_test_all_shiny$metric, agp_metrics_output_test_all_shiny$metric)
  expect_equal(agp_metrics_output_test_all_shiny$value, agp_metrics_output_test_all_shiny$value)

})
