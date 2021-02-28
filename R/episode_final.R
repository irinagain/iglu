library(iglu)
library(ggplot2)
# Author: Johnathan Shih, Jung Hoon Seo
# Date: February 13, 2021

episode_calculation <- function(data, hypo_thres=100.0, hyper_thres= 100.0, dur_length = 15){

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
    hypo_episode = params[[1]] < params[[2]]
    hypo_result = hypo_episode[2:length(hypo_episode)] - hypo_episode[1:length(hypo_episode)-1]

    if(params[[1]][length(params[[1]])] < params[[2]])
          hypo_result[length(hypo_result)] = -1
    if(params[[1]][1] < params[[2]])
      hypo_result[1] = 1

    hypo_starting_point = which(hypo_result %in% 1)
    hypo_end_point = which(hypo_result %in% -1)

    hypo_dur_length = hypo_end_point - hypo_starting_point

    if(is.null(hypo_dur_length))
        hypo_dur_length = c(0)

    i = 1; hypo_mean = 0

    while(i < length(hypo_end_point)){
        interval = seq(hypo_starting_point[i],hypo_end_point[i], 1)
        hypo_mean = hypo_mean + mean(params[[1]][interval])
        i = i + 1
    }

    hypo_duration = mean(hypo_dur_length * dt0)
    hypo_total <- hypo_result[!is.na(hypo_result) & hypo_result ==1]

    hyper_episode = params[[1]] > params[[3]]
    hyper_result = hyper_episode[2:length(hyper_episode)] - hyper_episode[1:length(hyper_episode)-1]

    if(params[[1]][length(params[[1]])] > params[[3]])
      hyper_result[length(hyper_result)] = -1
    if(params[[1]][1] > params[[3]])
      hyper_result[1] = 1

    hyper_starting_point = which(hyper_result %in% 1)
    hyper_end_point = which(hyper_result %in% -1)

    hyper_dur_length = hyper_end_point - hyper_starting_point

    if(is.null(hyper_dur_length))
        hyper_dur_length = c(0)

    i = 1; hyper_mean = 0

    while(i < length(hyper_end_point)){
      interval = seq(hyper_starting_point[i],hyper_end_point[i], 1)
      hyper_mean = hyper_mean + mean(params[[1]][interval])
      i = i + 1
    }

    hyper_duration = mean(hyper_dur_length * dt0)

    hyper_total <- hyper_result[!is.na(hyper_result) & hyper_result ==1]

    episodes = c(length(hypo_total), length(hyper_total))
    durations = c(hypo_duration, hyper_duration)
    means = c(hypo_mean, hyper_mean)
    dataframe <- data.frame("Mean Episodes Day(Hypo)" = episodes[1], "Mean Episodes Day(Hyper)" = episodes[2],
                            "Mean Duration (Hypo)" = durations[1], "Mean Duration (Hyper)" = durations[2], check.names = FALSE,
                            "Hypo Episode Mean" = means[1], "Hyper Episode Mean" = means[2])
    return (dataframe)
  }

  plot_episode(params)

  result = episode(params)

  print(result)

}#end Function

episode_calculation(example_data_5_subject)

