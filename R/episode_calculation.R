#' Calculates the number of Hypo/Hyperglycemic events as well as other statistics
#' @name episode_calculation
#'
#' @description
#' The function episode calculation produces the number of Hypo/Hyperglycemic events as well as other statistics such as the average glucose level, mean duration, and the percentage of time spent in the ranges.
#'
#' @usage
#' episode_calculation(data, lv1_hypo=100.0,lv2_hypo = 70, lv1_hyper= 120.0, lv2_hyper = 160, dur_length = 15)
#'
#' @param data DataFrame object with column names "id", "time", and "gl"
#'
#' @param lv1_hypo A double specifying a hypoglycemia threshold for level 1
#'
#' @param lv2_hypo A double specifying a hypoglycemia threshold for level 2
#'
#' @param lv1_hyper A double specifying a hyperglycemia threshold for level 1
#'
#' @param lv2_hyper A double specifying a hyperglycemia threshold for level 1
#'
#' @param dur_length An integer or a double specfiying a duration length in minutes
#'
#' @return Data frame including Average Glucose, number of hypo and hyper episodes, Hypo and hyper mean values, the percentages of low, high and target alerts
#'
#' @export
#'
#' @author Johnathan Shih, Jung Hoon Seo
#'
#' @examples
#'
#' episode_calculation(example_data_5_subject, lv1_hypo=100.0,lv2_hypo = 70, lv1_hyper= 120.0, lv2_hyper = 160, dur_length = 15)
# library(iglu)
# library(ggplot2)
# library(dplyr)

episode_calculation <- function (data, lv1_hypo=100.0,lv2_hypo = 70, lv1_hyper= 120.0, lv2_hyper = 160, dur_length = 15) {

  params = list(lv1_hypo, lv1_hyper, dur_length)

  episode_calculator <- function(data, params){
    ##################### Input Processing        #####################
    data_ip = CGMS2DayByDay(data, dt0 = 5)
    gl_by_id_ip = data_ip[[1]]
    dt0 = data_ip[[3]]
    params = list(gl_by_id_ip, params[[1]], params[[2]], params[[3]], dt0)
    #gl_by_id_ip       : CGM data for a subject
    #hypo(hyper)_thres : Hypoglycemia and hyperglycemia threshold
    #dur_length        : Duaration length
    #dt0               : Default value for an inteveral (default = 5 mins)
    ##################### Input Ended             #####################

    ##################### Episode Day computation   ###################
    episode_one_day<- function(params) {
      if(params[[2]] > params[[3]])
        warning("WARNING. Hypoglycemia threshold is greater than hyperglycemia threshold. This may affect result")

      gl_by_id_ip = params[[1]][!is.na(params[[1]])]
      hypo_episode = gl_by_id_ip <= params[[2]] # if data is less than or equal to the threshold containing true or false value
      hypo_result = hypo_episode[2:length(hypo_episode)] - hypo_episode[1:length(hypo_episode)-1]
      hypo_end_point = which(hypo_result %in% -1) + 1
      hypo_starting_point = which(hypo_result %in% 1) + 1

      # Handle when hypoglycemia continues until the end
      if(gl_by_id_ip[length(gl_by_id_ip)] <= params[[2]]){
        hypo_end_point = c(hypo_end_point, length(hypo_result))
        hypo_end_point= sort(hypo_end_point)
      }

      # Handle exception when first point or first sequence is NA
      if(gl_by_id_ip[1] <= params[[2]] ) {
        hypo_starting_point = c(hypo_starting_point, 1)
        hypo_starting_point= sort(hypo_starting_point)
      }

      hypo_dur_length = hypo_end_point - hypo_starting_point

      if(is.null(hypo_dur_length)) hypo_dur_length = c(0)

      hypo_dur_length = hypo_dur_length[hypo_dur_length >= 3]
      hypo_duration = mean(hypo_dur_length * dt0)
      hypo_total = length(hypo_dur_length)
      hyper_episode = gl_by_id_ip >= params[[3]]
      hyper_result = hyper_episode[2:length(hyper_episode)] - hyper_episode[1:length(hyper_episode)-1]
      hyper_end_point = which(hyper_result %in% -1) + 1
      hyper_starting_point = which(hyper_result %in% 1) + 1

      # Handle when hyperglycemia continues until the end

      if(gl_by_id_ip[length(gl_by_id_ip)] >= params[[3]]){
        hyper_end_point = c(hyper_end_point, length(hyper_result))
        hyper_end_point= sort(hyper_end_point)
      }
      # Handle exception when first point is greater than hyperglycemia
      if(gl_by_id_ip[1] >= params[[3]] ) {
        hyper_starting_point = c(hyper_starting_point, 1)
        hyper_starting_point= sort(hyper_starting_point)
      }

      hyper_dur_length = hyper_end_point - hyper_starting_point

      if(is.null(hypo_dur_length)) hypo_dur_length = c(0)

      hyper_dur_length = hyper_dur_length[hyper_dur_length >= 3]
      hyper_duration = mean(hyper_dur_length * dt0)
      hyper_total = length(hyper_dur_length)
      episodes = c(hypo_total, hyper_total)
      durations = c(hypo_duration, hyper_duration)

      i = 1; hypo_mean = 0

      while(i <= length(hypo_dur_length)){
        if( (!is.na(hypo_end_point[i]) & !is.na(hypo_starting_point[i])) & (hypo_end_point[i] - hypo_starting_point[i]) >= 3){
          interval = seq(hypo_starting_point[i],hypo_end_point[i], 1)
          interval_val = gl_by_id_ip[interval]
          hypo_mean = hypo_mean + mean(interval_val)
        }
        i = i + 1
      }

      i = 1; hyper_mean = 0

      while(i <= length(hyper_dur_length)){
        if((!is.na(hyper_end_point[i]) & !is.na(hyper_starting_point[i])) & (hyper_end_point[i] - hyper_starting_point[i]) >= 3 ){
          interval = seq(hyper_starting_point[i],hyper_end_point[i], 1)
          interval_val = gl_by_id_ip[interval]
          hyper_mean = hyper_mean + mean(interval_val)
        }
        i = i + 1
      }

      means = c(hypo_mean, hyper_mean)

      if (length(hypo_dur_length != 0))
        Low_Alert = (sum(hypo_dur_length)) / length(gl_by_id_ip) * 100
      else
        Low_Alert = 0
      if (length(hyper_dur_length != 0))
        High_Alert = (sum(hyper_dur_length)) / length(gl_by_id_ip) * 100
      else
        High_Alert = 0

      Target_Range = 100 - High_Alert - Low_Alert
      range = c(Low_Alert, Target_Range, High_Alert)
      dataframe <- data.frame("Hypo_ep" = episodes[1], "Hyper_ep" = episodes[2],
                              "hypo_duration" = durations[1], "hyper_duration" = durations[2], check.names = FALSE,
                              "Hypo_mean" = means[1], "Hyper_mean" = means[2],
                              "low_alert" = range[1], "high_alert" = range[3],
                              "target_range" = range[2])
      return (dataframe)
    }
    ##################### Episode Day computation End       ###############

    ##################### Eipsode computation for multidays ###############
    multidays <-function(data_ip, params){
      i = 1
      gl_by_id_ip = data_ip[[1]]

      num_rows = dim(gl_by_id_ip)[1]

      result = NA; Average_Glucose=c(); Hypo_ep = c(); Hyper_ep= c();hypo_duration=c();hyper_duration=c();Hypo_mean=c();Hyper_mean=c();low_alert=c();high_alert=c();target_range=c();
      while(i <= num_rows){
        params[[1]] = gl_by_id_ip[i,]
        if(!all(is.na(params[[1]]))){
          temp = episode_one_day(params)
          Hypo_ep = c(Hypo_ep, temp$Hypo_ep); Hyper_ep = c(Hyper_ep, temp$Hyper_ep);
          hypo_duration = c(hypo_duration, temp$hypo_duration); hyper_duration = c(hyper_duration, temp$hyper_duration); Hypo_mean = c(Hypo_mean, temp$Hypo_mean);
          Hyper_mean = c(Hyper_mean, temp$Hyper_mean); low_alert = c(low_alert, temp$low_alert); high_alert = c(high_alert, temp$high_alert); target_range = c(target_range, temp$target_range);
          dataframe <- data.frame("Hypo_ep" = mean(Hypo_ep, na.rm= TRUE), "Hyper_ep" = mean(Hyper_ep, na.rm= TRUE),
                                  "hypo_duration" = mean(hypo_duration, na.rm= TRUE), "hyper_duration" = mean(hyper_duration, na.rm= TRUE), check.names = FALSE,
                                  "Hypo_mean" = mean(Hypo_mean, na.rm= TRUE), "Hyper_mean" = mean(Hyper_mean, na.rm= TRUE),
                                  "low_alert" = mean(low_alert, na.rm= TRUE), "high_alert" = mean(high_alert, na.rm= TRUE),
                                  "target_range" = mean(target_range, na.rm= TRUE))
        }
        i = i + 1
      }
      dataframe <- round(dataframe, digits = 2)
      return (dataframe)
    }
    ##################### Eipsode computation for multidays END ###############
    result = 0
    result = multidays(data_ip, params)
    return (result)
  }
  id = NULL
  rm("id")


  #####################         Wrapper Function         ####################
  wrapper_function <-function(data,params){
    out <- data %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(episode_calculator(data.frame(id, time, gl), params))
  }

  out = wrapper_function(data,params)

  params = list(lv2_hypo, lv2_hyper, dur_length)

  out2 = wrapper_function(data,params)

  #print(typeof(out2))
  final = list(out, out2)

  #final = data.frame(level1 = out, level2 = out2)
  return(final)
}#end Function




