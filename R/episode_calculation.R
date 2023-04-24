#' Calculates the number of Hypo/Hyperglycemic events as well as other statistics
#' @name episode_calculation
#'
#' @description
#' The function episode calculation produces the number of Hypo/Hyperglycemic events as well as other statistics such as mean duration
#'
#' @param data DataFrame object with column names "id", "time", and "gl"
#'
#' @param lv1_hypo A double specifying a hypoglycemia threshold for level 1
#'
#' @param lv2_hypo A double specifying a hypoglycemia threshold for level 2
#'
#' @param lv1_hyper A double specifying a hyperglycemia threshold for level 1
#'
#' @param lv2_hyper A double specifying a hyperglycemia threshold for level 2
#'
#' @param dur_length An integer or a double specifying a duration length in minutes
#'
#' @inheritParams CGMS2DayByDay
#'
#' @return A dataframe with
#' \item{Hypo_ep}{A mean value that counts the number of hypogyclemia episodes per days}
#' \item{Hyper_ep}{A mean value that counts the number of hypergyclemia episodes per days}
#' \item{hypo_duration}{A mean value of the hypoglycemia durations per days}
#' \item{hyper_duration}{A mean value of the hyperclycemia durations per days}
#' \item{low_alert}{A mean percentage of time in level 1 and 2 hypoglycemic range}
#' \item{target_range}{A mean percentage of time in target range}
#' \item{high_alert}{A mean percentage of time in level 1 and 2 hyperglycemic ranges}
#' \item{hypo_min_avg}{A mean percentage of time for the hypoglycemia per days}
#' \item{hyper_min_avg}{A mean Percentage of time for the hyperglycemia per days}
#'
#' @export
#'
#' @author Johnathan Shih, Jung Hoon Seo
#'
#' @examples episode_calculation(example_data_5_subject, lv1_hypo=100, lv1_hyper= 120)
#'

episode_calculation <- function (data, lv1_hypo=100.0,lv2_hypo = 70, lv1_hyper= 120, lv2_hyper = 180,
                                 dur_length = 15, dt0 = NULL, inter_gap = 45, tz = "") {

  episode_calculator <- function(data, params) {

    episode_one_day<- function(params) {

        stat_computation <- function(gl_by_id_ip, params, is_hypo) {

          # IF THE TIME SERIES DATA IS LESS THAN OR EQUAL TO THE THRESHOLD THAT WAS GIVEN, THOSE ARE TRUE. OTHERWISE, FALSE.
          if (is_hypo){
            hyp_episode = gl_by_id_ip <= params[[2]]
          } else{
            hyp_episode = gl_by_id_ip >= params[[3]]
          }

          # HYPO OR HYPER DURATION LENGTH CALCULATION
          hyp_result = hyp_episode[2:length(hyp_episode)] - hyp_episode[1:length(hyp_episode)-1]
          hyp_end_point = which(hyp_result %in% -1) + 1
          hyp_starting_point = which(hyp_result %in% 1) + 1

          # add first/last points as start/end if they meet the threshold
          if (is_hypo){
                if(gl_by_id_ip[length(gl_by_id_ip)] <= params[[2]]){
                  hyp_end_point = c(hyp_end_point, length(gl_by_id_ip))
                }
                if(gl_by_id_ip[1] <= params[[2]] ) {
                  hyp_starting_point = c(1, hyp_starting_point)
                }
          }else{
                if(gl_by_id_ip[length(gl_by_id_ip)] >= params[[3]]){
                  hyp_end_point = c(hyp_end_point, length(gl_by_id_ip))
                }
                if(gl_by_id_ip[1] >= params[[3]] ) {
                  hyp_starting_point = c(1, hyp_starting_point)
                }
          }

          hyp_dur_length = hyp_end_point - hyp_starting_point

          # HANDLING EXCEPTION: WHEN FIRST POINT OR SEQUENCE ARE NA
          if(is.null(hyp_dur_length)) hyp_dur_length = c(0)

          # COMPUTING EPISODES THAT GO BELOW THE THRESHOLD
          hyp_dur_length = hyp_dur_length[hyp_dur_length >= (params[[4]]/params[[5]])]
          hyp_duration = mean(hyp_dur_length * params[[5]])
          hyp_total = length(hyp_dur_length)

          i = 1; hyp_mean = 0
          while(i <= length(hyp_dur_length)){
            if( (!is.na(hyp_end_point[i]) & !is.na(hyp_starting_point[i])) & (hyp_end_point[i] - hyp_starting_point[i]) >= params[[4]]/params[[5]]){
              interval = seq(hyp_starting_point[i],hyp_end_point[i], 1)
              interval_val = gl_by_id_ip[interval]
              hyp_mean = hyp_mean + mean(interval_val)
            }
            i = i + 1
          }

          Low_Or_High_Alert = 0

          if (length(hyp_dur_length != 0))
            Low_Or_High_Alert = (sum(hyp_dur_length)) / length(gl_by_id_ip) * 100
          else
            Low_Or_High_Alert = 0

          avg_Min_hyp = 0

          if(!all(is.na(gl_by_id_ip))){
            avg_Min_hyp = sum(hyp_dur_length * dt0) / (length(gl_by_id_ip) * 5)
          }
          r = c(hyp_total, hyp_duration, avg_Min_hyp, Low_Or_High_Alert)
          return (r)
      } # stat_computation FUNCTION ENDS

      if(params[[2]] > params[[3]])
          warning("Warning. Hypoglycemia threshold is greater than hyperglycemia threshold. This may affect result")

      gl_by_id_ip = params[[1]][!is.na(params[[1]])]

      stat_outputs = stat_computation(gl_by_id_ip, params, TRUE)
      hypo_total = stat_outputs[1]
      hypo_duration = stat_outputs[2]
      avg_Min_hypo = stat_outputs[3]
      Low_Alert = stat_outputs[4]

      stat_outputs = stat_computation(gl_by_id_ip, params, FALSE)
      hyper_total = stat_outputs[1]
      hyper_duration = stat_outputs[2]
      avg_Min_hyper = stat_outputs[3]
      High_Alert = stat_outputs[4]

      episodes = c(hypo_total, hyper_total)
      durations = c(hypo_duration, hyper_duration)
      Target_Range = 100 - High_Alert - Low_Alert
      range = c(Low_Alert, Target_Range, High_Alert)
      Min_avg = c(avg_Min_hypo, avg_Min_hyper)

      dataframe <- data.frame("Hypo_ep" = episodes[1], "Hyper_ep" = episodes[2],
                              "hypo_duration" = durations[1], "hyper_duration" = durations[2], check.names = FALSE,
                              "low_alert" = range[1], "high_alert" = range[3],
                              "target_range" = range[2], "Min_hypo" = Min_avg[1], "Min_hyper" = Min_avg[2])

      return (dataframe)
    }
    ##################### Episode Day computation End       ###############

    ##################### Eipsode computation for multidays ###############
    multidays <-function(data_ip, params){
      i = 1

      gl_by_id_ip = data_ip[[1]]

      num_rows = dim(gl_by_id_ip)[1]

      result = NA; Average_Glucose=c(); Hypo_ep = c(); Hyper_ep= c();hypo_duration=c();hyper_duration=c();Hypo_mean=c();Hyper_mean=c();low_alert=c();high_alert=c();target_range=c();
      hypo_min_avg = c();hyper_min_avg = c();

      # each row is a day of interpolated data
      while(i <= num_rows){

        params[[1]] = gl_by_id_ip[i,]

        if(!all(is.na(params[[1]]))){

          temp = episode_one_day(params)

          Hypo_ep = c(Hypo_ep, temp$Hypo_ep); Hyper_ep = c(Hyper_ep, temp$Hyper_ep);

          hypo_duration = c(hypo_duration, temp$hypo_duration); hyper_duration = c(hyper_duration, temp$hyper_duration);

          low_alert = c(low_alert, temp$low_alert); high_alert = c(high_alert, temp$high_alert); target_range = c(target_range, temp$target_range);

          hypo_min_avg = c(hypo_min_avg, temp$Min_hypo); hyper_min_avg = c(hyper_min_avg, temp$Min_hyper)

          df_multiday <- data.frame("Hypo_ep" = mean(Hypo_ep, na.rm= TRUE), "Hyper_ep" = mean(Hyper_ep, na.rm= TRUE),
                                  "hypo_duration" = mean(hypo_duration, na.rm= TRUE), "hyper_duration" = mean(hyper_duration, na.rm= TRUE), check.names = FALSE,
                                  "low_alert" = mean(low_alert, na.rm= TRUE), "high_alert" = mean(high_alert, na.rm= TRUE),
                                  "target_range" = mean(target_range, na.rm= TRUE),
                                  "hypo_min_avg" = mean(hypo_min_avg, na.rm = TRUE), "hyper_min_avg" = mean(hyper_min_avg, na.rm = TRUE))
        }

        i = i + 1
      }
      df_multiday <- round(df_multiday, digits = 2)
      return (df_multiday)
    } #multidays FUNCTION ENDS

    ##################### Input Processing        #####################
    data_ip = gl_by_id_ip = NULL
    rm(list = c("data_ip", "gl_by_id_ip"))

    data_ip = CGMS2DayByDay(data, dt0 = dt0, inter_gap = inter_gap, tz = tz)
    gl_by_id_ip = data_ip[[1]]
    dt0 = data_ip[[3]]
    params = list(gl_by_id_ip, params[[1]], params[[2]], params[[3]], dt0)

    #gl_by_id_ip       : CGM data for a subject
    #lv_hypo(hyper)    : Hypoglycemia and hyperglycemia threshold (lv1 = level 1 and lv2 = level2)
    #dur_length        : Duaration length
    #dt0               : Default value for an inteveral (default = 5 mins)
    ###################################################################
    result = 0
    result = multidays(data_ip, params)
    return (result)

  } #episode calculator function ends

  id = NULL
  rm("id")

  #####################         WRAPPER FUNCTION         ####################
  ################  THIS WRAPPER FUNCTION FORMATS THE OUTPUT  ###############
  wrapper_function <-function(data, params){
    out = NULL
    rm("out")
    out <- data %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(episode_calculator(data.frame(id, time, gl), params))
  }

  # Remove global some global variables
  df1 = df2 = out = out2 = result1 = result2 = final =NULL
  rm(list = c("df1", "df2", "out", "out2", "result1", "result2", "final"))

  params = list(lv1_hypo, lv1_hyper, dur_length)

  # This returns episode values for hypo and hyper level 1
  result1 = wrapper_function(data, params)

  # Changing parameters for level 2
  params = list(lv2_hypo, lv2_hyper, dur_length)

  # This returns episode values for hypo and hyper level 2
  result2 = wrapper_function(data,params)

  df1 = data.frame(result1)
  df2 = data.frame(result2)

  i <- 1

  # Process of mering lv1 data with lv2 data
  row_names_lv1 = c()
  row_names_lv2 = c()
  while(i <= nrow(df1)){
      names_lv1 <- paste(df1$id[i], "(Lv1)")
      names_lv2 <- paste(df2$id[i], "(Lv2)")
      row_names_lv1 <- c(row_names_lv1, names_lv1)
      row_names_lv2 <- c(row_names_lv2, names_lv2)
      i = i + 1
  }

  df1$id <- toString(df1$id)
  df1$id <- row_names_lv1

  df2$id <- toString(df2$id)
  df2$id <- row_names_lv2

  # Bind two data frames
  final = rbind(df1,df2)

  return(final)

}#end Function

