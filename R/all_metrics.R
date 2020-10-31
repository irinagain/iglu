#' Combination of all iglu functions:
#' ADRR, AUC, CONGA, CV_GLU, CV_Measures, eA1C, GMI, GRADE,
#' GRADE_Euglycemia, GRADE_Hyperglycemia, GRADE_Hypoglycemia,
#' GVP, HBGI, LBGI, Hyper_Index, Hypo_Index, IGC, IQR_GLU,
#' J_Index, M_Value, Mad_GLU, MAGE, MODD, Percent_Above,
#' Percent_Below, Percent_In_Range, Range, SD_GLU,
#' SD_Measures, SD_ROC, Summary
#'
#' @description
#' The function all_metrics runs all of the iglu functions:
#' ADRR, AUC, CONGA, CV_GLU, CV_Measures, eA1C, GMI, GRADE,
#' GRADE_Euglycemia, GRADE_Hyperglycemia, GRADE_Hypoglycemia,
#' GVP, HBGI, LBGI, Hyper_Index, Hypo_Index, IGC, IQR_GLU,
#' J_Index, M_Value, Mad_GLU, MAGE, MODD, Percent_Above,
#' Percent_Below, Percent_In_Range, Range, SD_GLU,
#' SD_Measures, SD_ROC, Summary
#' and combines them into a tibble object.
#'
#' @usage
#' all_metrics(data)
#'
#' @param
#' data DataFrame object with column names "id", "time", and "gl", or numeric vector of glucose values.
#'
#' @return
#' If a data.frame object is passed, then a tibble object with 1 row for each subject, and 49 columns is returned:
#' a column for subject id,
#' a column for adrr value,
#' a column for hourly_auc value,
#' a column for conga value,
#' a column for cv value,
#' a column for CVmean value,
#' a column for CVsd value,
#' a column for ea1c value,
#' a column for gmi value,
#' a column for grade value,
#' a column for grade_eugly value,
#' a column for grade_hyper value,
#' a column for grade_hypo value,
#' a column for gvp value,
#' a column for hbgi value,
#' a column for lbgi value,
#' a column for hyper_index value,
#' a column for hypo_index value,
#' a column for igc value,
#' a column for iqr value,
#' a column for j_index value,
#' a column for m_value value,
#' a column for MAD value,
#' a column for mage value,
#' a column for modd value,
#' a column for above_140 value,
#' a column for above_180 value,
#' a column for above_200 value,
#' a column for above_250 value,
#' a column for below_50 value,
#' a column for below_80 value,
#' a column for in_range_70_140 value,
#' a column for in_range_70_180 value,
#' a column for in_range_80_200 value,
#' a column for range value,
#' a column for sd value,
#' a column for SdW value,
#' a column for SdHHMM value,
#' a column for SdWSH value,
#' a column for SdDM value,
#' a column for SdB value,
#' a column for SdBDM value,
#' a column for sd_roc value,
#' a column for Min. value,
#' a column for 1st Qu. value,
#' a column for Median value,
#' a column for Mean value,
#' a column for 3rd Qu. value,
#' a column for Max. value.
#'
#' @export
#'
#' @details
#' Returns a tibble object with 1 row for each subject, and 49 columns:
#' a column for subject id,
#' a column for adrr value,
#' a column for hourly_auc value,
#' a column for conga value,
#' a column for cv value,
#' a column for CVmean value,
#' a column for CVsd value,
#' a column for ea1c value,
#' a column for gmi value,
#' a column for grade value,
#' a column for grade_eugly value,
#' a column for grade_hyper value,
#' a column for grade_hypo value,
#' a column for gvp value,
#' a column for hbgi value,
#' a column for lbgi value,
#' a column for hyper_index value,
#' a column for hypo_index value,
#' a column for igc value,
#' a column for iqr value,
#' a column for j_index value,
#' a column for m_value value,
#' a column for MAD value,
#' a column for mage value,
#' a column for modd value,
#' a column for above_140 value,
#' a column for above_180 value,
#' a column for above_200 value,
#' a column for above_250 value,
#' a column for below_50 value,
#' a column for below_80 value,
#' a column for in_range_70_140 value,
#' a column for in_range_70_180 value,
#' a column for in_range_80_200 value,
#' a column for range value,
#' a column for sd value,
#' a column for SdW value,
#' a column for SdHHMM value,
#' a column for SdWSH value,
#' a column for SdDM value,
#' a column for SdB value,
#' a column for SdBDM value,
#' a column for sd_roc value,
#' a column for Min. value,
#' a column for 1st Qu. value,
#' a column for Median value,
#' a column for Mean value,
#' a column for 3rd Qu. value,
#' a column for Max. value.
#'
#' @examples
#' data(example_data_1_subject)
#' all_metrics(example_data_1_subject)
#'


# needed packages
#library(dplyr)
#library(iglu)

# function calls all metrics on a dataset.
# returns a list
all_metrics <- function(data){
  # Mean, Median, and Quantile Metrics not included. Summary covers all
  out = list("ADRR" = adrr(data),
             "AUC" = auc(data),
             "CONGA" = conga(data),
             "CV_GLU" = cv_glu(data),
             "CV_Measures" = cv_measures(data),
             "eA1C" = ea1c(data),
             "GMI" = gmi(data),
             "GRADE" = grade(data),
             "GRADE_Euglycemia" = grade_eugly(data),
             "GRADE_Hyperglycemia" = grade_hyper(data),
             "GRADE_Hypoglycemia" = grade_hypo(data),
             "GVP" = gvp(data),
             "HBGI" = hbgi(data),
             "LBGI" = lbgi(data),
             "Hyper_Index" = hyper_index(data),
             "Hypo_Index" = hypo_index(data),
             "IGC" = igc(data),
             "IQR_GLU" = iqr_glu(data),
             "J_Index" = j_index(data),
             "M_Value" = m_value(data),
             "Mad_GLU" = mad_glu(data),
             "MAGE" = mage(data),
             "MODD" = modd(data),
             "Percent_Above" = above_percent(data),
             "Percent_Below" = below_percent(data),
             "Percent_In_Range" = in_range_percent(data),
             "Range" = range_glu(data),
             "SD_GLU" = sd_glu(data),
             "SD_Measures" = sd_measures(data),
             "SD_ROC" = sd_roc(data),
             "Summary" = summary_glu(data))
  outTable <- out %>%
        Reduce(function(dtf1,dtf2) dplyr::left_join(dtf1,dtf2,by="id"), .)

  return(outTable)
}

