test3 = iglu::example_data_5_subject

test_that("single-subject lasagna plots select rows for the first subject", {
  expect_warning(
    plot <- plot_glu(
      test3,
      plottype = "lasagna",
      datatype = "single",
      tz = "UTC"
    ),
    "The provided data have 5 subjects. The plot will only be created for subject *Subject 1"
  )
  expect_s3_class(plot, "ggplot")
})
