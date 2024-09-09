test_data = c(100, 120, 150, 200) # col 'gl'
test_id = 'test 1' # col 'id'
test_df <- data.frame(id = test_id, gl = test_data)

test_out <- test_df %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    eA1C = (46.7+mean(gl, na.rm = TRUE) )/28.7
  )

test_out$id = NULL

result =
test_that("iglu::ea1c == base::ea1c", {
  expect_equal(iglu::ea1c(test_data), test_out , tolerance = 0.0001)
})
