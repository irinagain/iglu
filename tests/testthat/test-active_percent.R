test_that("sampling frequency is inferred separately for each subject", {
  start_time <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  data <- rbind(
    data.frame(
      id = "five-minute",
      time = start_time + c(0, 5, 10, 15) * 60,
      gl = 100:103
    ),
    data.frame(
      id = "fifteen-minute",
      time = start_time + c(0, 15, 30, 45) * 60,
      gl = 110:113
    )
  )

  combined <- active_percent(data, tz = "UTC")
  separate <- vapply(
    unique(data$id),
    function(subject) {
      active_percent(data[data$id == subject, ], tz = "UTC")$active_percent
    },
    numeric(1)
  )

  expect_equal(combined$active_percent, c(100, 100))
  expect_equal(
    combined$active_percent,
    unname(separate[match(combined$id, names(separate))])
  )
})

test_that("an explicit sampling frequency still applies to every subject", {
  start_time <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  data <- rbind(
    data.frame(
      id = "five-minute",
      time = start_time + c(0, 5, 10, 15) * 60,
      gl = 100:103
    ),
    data.frame(
      id = "fifteen-minute",
      time = start_time + c(0, 15, 30, 45) * 60,
      gl = 110:113
    )
  )

  result <- active_percent(data, dt0 = 5, tz = "UTC")

  expect_equal(
    result$active_percent[match(c("five-minute", "fifteen-minute"), result$id)],
    c(100, 40)
  )
})
