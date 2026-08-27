test_that("all mode forwards interpolation arguments to episode calculations", {
  start_time <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  minutes <- seq(0, 1435, by = 5)
  minutes <- minutes[!(minutes %in% seq(130, 150, by = 5))]
  glucose <- rep(100, length(minutes))
  glucose[minutes %in% c(120, 125, 155, 160)] <- 60
  data <- data.frame(
    id = "synthetic-subject",
    time = start_time + minutes * 60,
    gl = glucose
  )

  episodes <- episode_calculation(
    data,
    dt0 = 5,
    inter_gap = 20,
    tz = "UTC"
  )
  expected <- episodes$avg_ep_per_day[
    episodes$type == "hypo" & episodes$level == "lv1"
  ]
  result <- all_metrics(data, dt0 = 5, inter_gap = 20, tz = "UTC")

  expect_equal(result$hypo_lv1, expected)
  expect_equal(result$hypo_lv1, 0)
})

test_that("consensus mode forwards interpolation arguments to episodes", {
  start_time <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  minutes <- seq(0, 300, by = 5)
  minutes <- minutes[!(minutes %in% seq(125, 145, by = 5))]
  data <- data.frame(
    id = "synthetic-subject",
    time = start_time + minutes * 60,
    gl = ifelse(minutes >= 60 & minutes <= 210, 60, 100)
  )

  episodes <- episode_calculation(
    data,
    dt0 = 5,
    inter_gap = 20,
    tz = "UTC"
  )
  expected <- episodes$total_episodes[
    episodes$type == "hypo" & episodes$level == "extended"
  ]
  result <- all_metrics(
    data,
    dt0 = 5,
    inter_gap = 20,
    tz = "UTC",
    metrics_to_include = "consensus_only"
  )

  expect_equal(result$total_extended_hypo_episodes, expected)
  expect_equal(result$total_extended_hypo_episodes, 0)
})

test_that("all_metrics forwards sampling frequency to active_percent", {
  start_time <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  data <- data.frame(
    id = "synthetic-subject",
    time = start_time + c(0, 15, 30, 45) * 60,
    gl = 100:103
  )

  expected <- active_percent(data, dt0 = 5, tz = "UTC")$active_percent
  result <- all_metrics(
    data,
    dt0 = 5,
    inter_gap = 45,
    tz = "UTC",
    metrics_to_include = "consensus_only"
  )

  expect_equal(result$active_percent, expected)
  expect_equal(result$active_percent, 40)
})
