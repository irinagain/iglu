# Test on one subject data
out = episode_calculation(example_data_1_subject[1:288, ],
                          lv1_hypo = 100, lv2_hypo = 70,
                          lv1_hyper = 120, lv2_hyper = 140)

##order requires order: hypo (1,2,ext), hyper (1,2), lv1_excl (hypo,hyper)

# avg_ep_per_day
test_that("no changes on 1 subject data for episodes per day", {
  expect_equal(out$avg_ep_per_day[1], 4.72, tolerance = 0.2)
  expect_equal(out$avg_ep_per_day[2], 0, tolerance = 0.2)
  expect_equal(out$avg_ep_per_day[3], 1.57, tolerance = 0.2)
  expect_equal(out$avg_ep_per_day[4], 0.787, tolerance = 0.2)
  expect_equal(out$avg_ep_per_day[5], 2.36, tolerance = 0.2)
  expect_equal(out$avg_ep_per_day[6], 4.72, tolerance = 0.2)
  expect_equal(out$avg_ep_per_day[7], 0, tolerance = 0.2)
})

# avg_ep_duration
test_that("no changes on 1 subject data for episodes duration", {
  expect_equal(out$avg_ep_duration[1], 112, tolerance = 0.2)
  expect_equal(out$avg_ep_duration[2], 0, tolerance = 0.2)
  expect_equal(out$avg_ep_duration[3], 268, tolerance = 0.2)
  expect_equal(out$avg_ep_duration[4], 385, tolerance = 0.2)
  expect_equal(out$avg_ep_duration[5], 61.7, tolerance = 0.2)
  expect_equal(out$avg_ep_duration[6], 112, tolerance = 0.2)
  expect_equal(out$avg_ep_duration[7], 0, tolerance = 0.2)
})

# avg_ep_gl
test_that("no changes on 1 subject data for episodes glucose", {
  expect_equal(out$avg_ep_gl[1], 112, tolerance = 0.2)
  expect_equal(out$avg_ep_gl[2], NA_real_)
  expect_equal(out$avg_ep_gl[3], 93.2, tolerance = 0.2)
  expect_equal(out$avg_ep_gl[4], 142, tolerance = 0.2)
  expect_equal(out$avg_ep_gl[5], 153, tolerance = 0.2)
  expect_equal(out$avg_ep_gl[6], 96.1, tolerance = 0.2)
  expect_equal(out$avg_ep_gl[7], NA_real_)
})
