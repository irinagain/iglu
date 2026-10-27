test_that("Libre raw data are imported", {
  out <- expect_no_error(read_raw_data(
    test_path("data", "read_raw_data_libre.csv"),
    sensor = "libre", id = "test-subject", tz = "UTC"
  ))

  expect_identical(names(out), c("id", "time", "gl"))
  expect_identical(out$id, rep("test-subject", 2))
  expect_identical(nrow(out), 2L)
  expect_s3_class(out$time, "POSIXct")
  expect_identical(format(out$time, tz = "UTC"),
                   c("2024-01-02 03:04:05", "2024-01-02 03:09:05"))
  expect_equal(out$gl, c(99, 112.5))
})

test_that("Libre Pro raw data are imported", {
  out <- expect_no_error(read_raw_data(
    test_path("data", "read_raw_data_librepro.csv"),
    sensor = "librepro", id = "test-subject", tz = "UTC"
  ))

  expect_identical(names(out), c("id", "time", "gl"))
  expect_identical(out$id, rep("test-subject", 2))
  expect_identical(nrow(out), 2L)
  expect_s3_class(out$time, "POSIXct")
  expect_identical(format(out$time, tz = "UTC"),
                   c("2024-01-02 03:04:00", "2024-01-02 03:19:00"))
  expect_identical(out$gl, c(111, 121))
})

test_that("ASC raw data columns and timestamps are imported", {
  out <- expect_no_error(read_raw_data(
    test_path("data", "read_raw_data_asc.csv"),
    sensor = "asc", id = "test-subject", tz = "UTC"
  ))

  expect_identical(names(out), c("id", "time", "gl"))
  expect_identical(out$id, rep("test-subject", 2))
  expect_identical(nrow(out), 2L)
  expect_s3_class(out$time, "POSIXct")
  expect_identical(format(out$time, tz = "UTC"),
                   c("2024-01-02 03:04:05", "2024-01-02 03:09:05"))
  expect_identical(out$gl, c(130, 140))
})

test_that("iPro raw data are imported", {
  out <- expect_no_error(read_raw_data(
    test_path("data", "read_raw_data_ipro.csv"),
    sensor = "ipro", id = "test-subject", tz = "UTC"
  ))

  expect_identical(names(out), c("id", "time", "gl"))
  expect_identical(out$id, rep("test-subject", 2))
  expect_identical(nrow(out), 2L)
  expect_s3_class(out$time, "POSIXct")
  expect_identical(format(out$time, tz = "UTC"),
                   c("2024-01-02 03:04:00", "2024-01-02 03:09:00"))
  expect_identical(out$gl, c(150, 160))
})
