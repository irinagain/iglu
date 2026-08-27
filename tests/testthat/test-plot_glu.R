test_that("single-subject lasagna plots select rows for the first subject", {
  start_time <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  data <- rbind(
    data.frame(
      id = "first-subject",
      time = start_time + (0:7) * 15 * 60,
      gl = 100 + (0:7)
    ),
    data.frame(
      id = "second-subject",
      time = start_time + (0:7) * 15 * 60,
      gl = 120 + (0:7)
    )
  )

  expect_warning(
    plot <- plot_glu(
      data,
      plottype = "lasagna",
      datatype = "single",
      tz = "UTC"
    ),
    "provided data have 2 subjects.*first-subject"
  )
  expect_s3_class(plot, "ggplot")
})
