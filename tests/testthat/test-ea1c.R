
test_df <- data.frame(id = 'test 1', gl = c(100, 120, 150, 200))

test_out <- test_df %>%
  dplyr::group_by(id) %>%
  dplyr::summarize(
    eA1C = (46.7 + sum(gl, na.rm = TRUE) / sum(!is.na(gl))) / 28.7
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-id)

result =
  testthat::test_that("iglu::ea1c", {
  expect_equal(iglu::ea1c(test_df$gl), test_out, tolerance = 0.0001)
})


