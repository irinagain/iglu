#' @title Example CGM data for one subject with Type II diabetes
#'
#' @description Dexcom G4 CGM measurements from 1 subject with Type II diabetes, this is a subset of \link{example_data_5_subject}.
#'
#' @format A data.frame with 2915 rows and 3 columns, which are:
#' \describe{
#' \item{id}{identifier of subject}
#' \item{time}{5-10 minute time value}
#' \item{gl}{glucose level}
#' }
"example_data_1_subject"

#' @title Example CGM data for 5 subjects with Type II diabetes
#'
#' @description Dexcom G4 CGM measurements for 5 subjects with Type II diabetes.
#'  These data are part of a larger study sample that consisted of patients with Type 2
#'   diabetes recruited from the general community. To be eligible, patients with Type 2 diabetes, not using insulin therapy and with a glycosylated hemoglobin (\eqn{HbA_{1c}}) value at least 6.5%, were screened with an overnight home sleep study using a type 3 sleep monitor. Only patients with an oxygen desaturation index (ODI) of at least 15 events/hr were selected. After the initial screening procedures, eligible patients completed continuous glucose monitoring using the Dexcom G4 sensor, which was placed 6 cm lateral to the umbilicus. Participants were instructed to wear both monitors for 7 days and provide calibration glucose data for the Dexcom sensor twice a day as per manufacturer instructions. Informed consent was obtained from all study participants and the protocol was approved by the local Institutional Review Board of the Johns Hopkins University School of Medicine.
#'
#' @format A data.frame with 13866 rows and 3 columns, which are:
#' \describe{
#' \item{id}{identifier of subject}
#' \item{time}{date and time stamp}
#' \item{gl}{glucose level as measured by CGM (mg/dL)}
#' }
"example_data_5_subject"

#' @title Example data from Hall et al. (2018)
#'
#' @description Dexcom G4 CGM measurements for 19 subjects from the Hall
#' publicly available dataset. Chosen as a subset of all subjects to be only those
#' with diabetes or pre-diabetes. Primarily intended for use with example_meals_hall
#'
#'
#' @format a data.frame with 34890 rows and 4 columns, which are:
#' \describe{
#' \item{id}{identifier of subject}
#' \item{time}{date and time stamp}
#' \item{gl}{glucose level as measured by CGM (mg/dL)}
#' \item{diagnosis}{character indicating diabetes diagnosis: diabetic or pre-diabetic}
#' }
#'
#' @details
#' This dataset can be used along with the example_meals_hall dataset in this package
#' to calculate meal_metrics.
#'
#'
#' @references
#' Hall et al. (2018) : Glucotypes reveal new patterns of glucose dysregulation
#' \emph{Plos Biology} \strong{16} (7): 3:e2005143
#' \doi{10.1371/journal.pbio.2005143}.
#'

'example_data_hall'

#' @title Example mealtimes data from Hall et al. (2018)
#'
#' @description Example of mealtimes data format for meal_metrics function,
#' corresponds to example_data_hall data.
#'
#' @format A data.frame with 9 rows and 3 columns, which are:
#' \describe{
#' \item{id}{identifier of subject}
#' \item{meal}{meal type identifier}
#' \item{mealtime}{time of meal}
#' }
#'
#' @details
#' There are 3 types of meals available: Cereal Flakes (CF), Peanut Butter
#' Sandwich (PB), and Protein Bar (Bar). The number after the abbreviation
#' refers to the replication number for the original study. For more details
#' on nutritional differences, please see the original study reference.
#'
#' This dataset should be used along with example_data_hall to calculate meal_metrics.
#'
#'
#' @references
#' Hall et al. (2018) : Glucotypes reveal new patterns of glucose dysregulation
#' \emph{Plos Biology} \strong{16} (7): 3:e2005143
#' \doi{10.1371/journal.pbio.2005143}.

"example_meals_hall"
