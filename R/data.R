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
#' @description Dexcom G4 CGM measurements for 5 subjects with Type II diabetes. These data are part of a larger study sample that consisted of patients with Type 2 diabetes recruited from the general community. To be eligible, patients with Type 2 diabetes, not using insulin therapy and with a glycosylated hemoglobin (HbA$_{1c}$) value at least 6.5%, were screened with an overnight home sleep study using a type 3 sleep monitor. Only patients with an oxygen desaturation index (ODI) of at least 15 events/hr were selected. After the initial screening procedures, eligible patients completed continuous glucose monitoring using the Dexcom G4 sensor, which was placed 6 cm lateral to the umbilicus. Participants were instructed to wear both monitors for 7 days and provide calibration glucose data for the Dexcom sensor twice a day as per manufacturer instructions. Informed consent was obtained from all study participants and the protocol was approved by the local Institutional Review Board of the Johns Hopkins University School of Medicine.
#'
#' @format A data.frame with 13866 rows and 3 columns, which are:
#' \describe{
#' \item{id}{identifier of subject}
#' \item{time}{date and time stamp}
#' \item{gl}{glucose level as measured by CGM (mg/dL)}
#' }
"example_data_5_subject"
