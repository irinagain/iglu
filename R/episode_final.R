#' Calculates the number of Hypo/Hyperglycemic events as well as other statistics
#'
#' @description
#' The function episode calculation produces the number of Hypo/Hyperglycemic events as well as other statistics such as the average glucose level, mean duration, and the percentage of time spent in the ranges.
#'
#' @usage
#' episode_calculation(data, hypo_thres=90.0, hyper_thres= 120.0, dur_length = 15)
#'
#' @details
#'
#' @return Data frame including Average Glucose, number of hypo and hyper episodes, Hypo and hyper mean values, the percentages of low, high and target alerts
#'
#' @export
#' library(iglu)
#' library(ggplot2)
#'
#' @author Johnathan Shih, Jung Hoon Seo
#'
#' @references
#'
#' @examples
#'
#' episode_calculation(example_data_4_subject, dt0 = 5)
#'
# library(iglu)
# library(ggplot2)
# Author: Johnathan Shih, Jung Hoon Seo

episode_calculation <- function(data, hypo_thres=90.0, hyper_thres= 120.0, dur_length = 15){

  data_ip = CGMS2DayByDay(data, dt0 = 5)

  gl_by_id_ip = data_ip[[1]][2,]

  dt0 = data_ip[[3]]

  params = list(gl_by_id_ip, hypo_thres, hyper_thres, dur_length, dt0)

  plot_episode<- function(params){
    df = data.frame(params[[1]])
    df <- cbind(index = cbind(1:length(params[[1]])), df)
    p2 <- ggplot(df, aes(x=index, y=gl_by_id_ip)) + geom_point()
    p2 <- p2 + geom_hline(yintercept=cbind(params[[2]], params[[3]]), linetype="dashed", color = "blue", size=.5)
    print(p2)
  }

  episode<- function(params) {
    hypo_episode = params[[1]] <= params[[2]]
    hypo_result = hypo_episode[2:length(hypo_episode)] - hypo_episode[1:length(hypo_episode)-1]

    if(params[[1]][length(params[[1]])] <= params[[2]])
      hypo_result[length(hypo_result)] = -1

    if(params[[1]][1] <= params[[2]] & !is.na(params[[1]][1])) # Handle exception when first point or first sequence is NA
      hypo_result[1] = 1

    else if (is.na(params[[1]][1])){
      NonNA <- which(!is.na(params[[1]]))
      firstNA <- min(NonNA)
      if(params[[1]][firstNA] < params[[2]]){
        hypo_result[firstNA] = 1
      }
    }

    hypo_starting_point = which(hypo_result %in% 1) + 1
    hypo_end_point = which(hypo_result %in% -1) + 1

    hypo_dur_length = hypo_end_point - hypo_starting_point

    if(is.null(hypo_dur_length))
      hypo_dur_length = c(0)

    i = 1; hypo_mean = 0

    while(i <= length(hypo_dur_length)){
      if((hypo_end_point[i] - hypo_starting_point[i]) >= 3 ){
        interval = seq(hypo_starting_point[i],hypo_end_point[i], 1)
        interval_val = params[[1]][interval]
        hypo_mean = hypo_mean + mean(interval_val[!is.na(interval_val)])
      }
      i = i + 1
    }
    hypo_dur_length = hypo_dur_length[hypo_dur_length >= 3]
    hypo_duration = mean(hypo_dur_length * dt0)
    hypo_total <- hypo_result[!is.na(hypo_result) & hypo_result ==1]

    hyper_episode = params[[1]] >= params[[3]]
    hyper_result = hyper_episode[2:length(hyper_episode)] - hyper_episode[1:length(hyper_episode)-1]

    if(params[[1]][length(params[[1]])] >= params[[3]]) # Handle exception When data ends but still did not go down to the threshold
      hyper_result[length(hyper_result)] = -1

    if(params[[1]][1] >= params[[3]] & !is.na(params[[1]][1])) # Handle exception when first point or first sequence is NA
      hyper_result[1] = 1
    else if (is.na(params[[1]][1])){
      NonNA <- which(!is.na(params[[1]]))
      firstNA <- min(NonNA)
      if(params[[1]][firstNA] >= params[[3]]){
        hyper_result[firstNA] = 1
      }
    }

    hyper_starting_point = which(hyper_result %in% 1) + 1

    hyper_end_point = which(hyper_result %in% -1) + 1

    hyper_dur_length = hyper_end_point - hyper_starting_point

    #print(hyper_dur_length)
    if(is.null(hyper_dur_length))
      hyper_dur_length = c(0)

    i = 1; hyper_mean = 0

    while(i <= length(hyper_dur_length)){
      if((hyper_end_point[i] - hyper_starting_point[i]) >= 3 ){
        interval = seq(hyper_starting_point[i],hyper_end_point[i], 1)
        print(interval)
        interval_val = params[[1]][interval]
        hyper_mean = hyper_mean + mean(interval_val[!is.na(interval_val)])
      }
      i = i + 1
    }
    hyper_dur_length = hyper_dur_length[hyper_dur_length >= 3]

    hyper_duration = mean(hyper_dur_length * dt0)

    hyper_total <- hyper_result[!is.na(hyper_result) & hyper_result ==1]

    episodes = c(length(hypo_dur_length), length(hyper_dur_length))

    durations = c(hypo_duration, hyper_duration)

    means = c(hypo_mean, hyper_mean)

    if (length(hypo_dur_length != 0))
      Low_Alert = (sum(hypo_dur_length)) / length(params[[1]][!is.na(params[[1]])]) * 100
    else
      Low_Alert = 0
    if (length(hyper_dur_length != 0))
      High_Alert = (sum(hyper_dur_length)) / length(params[[1]][!is.na(params[[1]])]) * 100
    else
      High_Alert = 0

    Target_Range = 100 - High_Alert - Low_Alert

    Avg_Glucose = mean(params[[1]][!is.na(params[[1]])] )

    range = c(Low_Alert, Target_Range, High_Alert)

    dataframe <- data.frame("Average_Glucose" = Avg_Glucose,
                            "Hypo_ep" = episodes[1], "Hyper_ep" = episodes[2],
                            "hypo_duration" = durations[1], "hyper_duration" = durations[2], check.names = FALSE,
                            "Hypo_mean" = means[1], "Hyper_mean" = means[2],
                            "low_alert" = range[1], "high_alert" = range[3],
                            "target_range" = range[2])

    return (dataframe)
  }

  plot_episode(params)

  result = episode(params)

  return (result)

}#end Function

