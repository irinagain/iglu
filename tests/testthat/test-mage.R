### 0. load in data
filepath = paste0(getwd(), '/data/')

manual_calc <- read.csv(paste0(filepath, "manual_calculations.csv"))

Dubosson2018 <- read.csv(paste0(filepath, "Dubosson2018_processed.csv"), header = TRUE)
Tsalikian2005 <- read.csv(paste0(filepath, "Tsalikian2005_processed.csv"), header = TRUE)
JHU <- iglu::example_data_5_subject

# A. exclude the samples where the comment is "exclude"
cgm_all_data <- lapply(1:length(manual_calc$dataset), function(x) {
  as.list(manual_calc[x, ])
})

cgm_all_data <- Filter(function(x) is.na(x$comment) || x$comment != "exclude", cgm_all_data)

# B. Subset the complete data sets by the row numbers found in the manual calculations
cgm_dataset_df <- lapply(cgm_all_data, function(x) {
  dataset <- x$dataset

  if (dataset != "Hall2018") {
    eval(parse(text=dataset))[x$start:x$end, ] # evaluate the text
  }
})

cgm_manual_calc <- sapply(cgm_all_data, function(x) x$manual) # get manual calculations

idx_to_remove = manual_calc[manual_calc$dataset == "Hall2018", ]$X

cgm_dataset_df <- cgm_dataset_df[-idx_to_remove]
cgm_manual_calc <- cgm_manual_calc[-idx_to_remove]

### 0.25 Helper F(x)
make_pdfs <- function(filepath, filename_prefix) {
  prefix = paste0(filepath, "/", filename_prefix)

  make_plot <- function(direction) {
    pdf(file = paste0(prefix, "_mage_", direction, ".pdf"), width=10, height=5, onefile=TRUE)
    for (i in 1:length(cgm_manual_calc)) {
      manual = cgm_manual_calc[i]
      val = iglu::mage(cgm_dataset_df[[i]], short_ma = short_ma, long_ma = long_ma, inter_gap = inter_gap, max_gap = max_gap, return_type = "num", direction = direction)$MAGE
      p = iglu::mage(cgm_dataset_df[[i]], short_ma = short_ma, long_ma = long_ma, inter_gap = inter_gap, max_gap = max_gap, direction = direction, plot = TRUE, plot_type = "ggplot", title=paste0("[i=", i, "] MAGE", direction, " = ", val, " (% diff from manual calc: ", round((val - manual)/manual*100, 2), "%)"))
      print(p)
    }
    dev.off()
  }

  make_plot("plus")
  make_plot("minus")
  make_plot("service")
  make_plot("avg")
  # make_plot("max") # TODO: plotting functionality for MAGEmax hasn't been implemented yet
}

### 0.5 Params
short_ma = 5
long_ma = 32
inter_gap = 45
max_gap = 180
return_type = "num"

### 1. Generate Ground Truth Labels
### If you are sure that the current iglu::mage has been thoroughly validated and is accurate, you can
### generate "ground truth labels" (i.e., what the output of the current iglu::mage f(x) is on some dataframes).
### Then, future tests will check for compliance/identicalness w/ the current MAGE outputs

# df = setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("idx", "mage_plus", "mage_minus", "mage_avg", "mage_service", "mage_max"))
#
# for (i in 1:length(cgm_dataset_df)) {
#   mage_plus = iglu::mage(cgm_dataset_df[[i]], short_ma = short_ma, long_ma = long_ma, inter_gap = inter_gap, max_gap = max_gap, return_type = "num", direction = "plus")$MAGE
#   mage_minus = iglu::mage(cgm_dataset_df[[i]], short_ma = short_ma, long_ma = long_ma, inter_gap = inter_gap, max_gap = max_gap, return_type = "num", direction = "minus")$MAGE
#   mage_avg = iglu::mage(cgm_dataset_df[[i]], short_ma = short_ma, long_ma = long_ma, inter_gap = inter_gap, max_gap = max_gap, return_type = "num", direction = "avg")$MAGE
#   mage_service = iglu::mage(cgm_dataset_df[[i]], short_ma = short_ma, long_ma = long_ma, inter_gap = inter_gap, max_gap = max_gap, return_type = "num", direction = "service")$MAGE
#   mage_max = iglu::mage(cgm_dataset_df[[i]], short_ma = short_ma, long_ma = long_ma, inter_gap = inter_gap, max_gap = max_gap, return_type = "num", direction = "max")$MAGE
#
#   df <- base::rbind(df, data.frame(idx=i, mage_plus=mage_plus, mage_minus=mage_minus, mage_avg=mage_avg, mage_service=mage_service, mage_max=mage_max))
# }
#
# write.csv(df, file=base::paste0(filepath, 'mage_ground_truth.csv'))
# make_pdfs(paste0(getwd(), '/data/mage_plots'), 'ground_truth') # see [here](https://github.com/Nathaniel-Fernandes/iglu/tree/df78ce4d1cde06a9b744afd71b8e1605971c5c54/tests/testthat/data/mage_plots) for ground truth plots

### 2. TESTS: Check for compliance (i.e., identicalness) to previous validated version of MAGE

ground_truth <- read.csv(paste0(filepath, 'mage_ground_truth.csv'))

# test MAGE+
test_that("iglu::MAGE+ == old_iglu::MAGE+", {
  for (i in 1:nrow(ground_truth)) {
    row = ground_truth[i, ]
    expect_equal(iglu::mage(cgm_dataset_df[[row$idx]], short_ma = short_ma, long_ma = long_ma, inter_gap = inter_gap, max_gap = max_gap, return_type = "num", direction="plus")$MAGE, row$mage_plus, tolerance = 0.1)
  }
})

# test MAGE-
test_that("iglu::MAGE- == old_iglu::MAGE-", {
  for (i in 1:nrow(ground_truth)) {
    row = ground_truth[i, ]
    expect_equal(iglu::mage(cgm_dataset_df[[row$idx]], short_ma = short_ma, long_ma = long_ma, inter_gap = inter_gap, max_gap = max_gap, return_type = "num", direction="minus")$MAGE, row$mage_minus, tolerance = 0.1)
  }
})

# test MAGE Avg
test_that("iglu::MAGEavg == old_iglu::MAGEavg", {
  for (i in 1:nrow(ground_truth)) {
    row = ground_truth[i, ]
    expect_equal(iglu::mage(cgm_dataset_df[[row$idx]], short_ma = short_ma, long_ma = long_ma, inter_gap = inter_gap, max_gap = max_gap, return_type = "num", direction="avg")$MAGE, row$mage_avg, tolerance = 0.1)
  }
})

# test MAGE Service
test_that("iglu::MAGEservice == old_iglu::MAGEservice", {
  for (i in 1:nrow(ground_truth)) {
    row = ground_truth[i, ]
    expect_equal(iglu::mage(cgm_dataset_df[[row$idx]], short_ma = short_ma, long_ma = long_ma, inter_gap = inter_gap, max_gap = max_gap, return_type = "num", direction="service")$MAGE, row$mage_service, tolerance = 0.1)
  }
})

# test MAGE Max
test_that("iglu::MAGEmax == old_iglu::MAGEmax", {
  for (i in 1:nrow(ground_truth)) {
    row = ground_truth[i, ]
    expect_equal(iglu::mage(cgm_dataset_df[[row$idx]], short_ma = short_ma, long_ma = long_ma, inter_gap = inter_gap, max_gap = max_gap, return_type = "num", direction="max")$MAGE, row$mage_max, tolerance = 0.1)
  }
})

### 3. Debugging Help
# make_pdfs(paste0(getwd(), '/tests/testthat/data/mage_plots'), 'debug') # if running from command line, you need to change 1st param to be 'iglu' directory
