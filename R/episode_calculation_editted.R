library(iglu)
library(ggplot2)
# Author: Johnathan Shih, Jung Hoon Seo
# Date: February 13, 2021

episode_calculation <- function(data, hypo_thres=91.0, hyper_thres= 120.0, dur_length = 15){

  data_ip = CGMS2DayByDay(data, dt0 = 5)

  gl_by_id_ip = data_ip[[1]][4,]

  dt0 = data_ip[[3]]

  params = list(gl_by_id_ip, hypo_thres, hyper_thres, dur_length, dt0)

  plot_episode<- function(params){
    df = data.frame(params[[1]])
    df <- cbind(index = cbind(1:length(params[[1]])), df)
    p2 <- ggplot(df, aes(x=index, y=gl_by_id_ip)) + geom_point() 
    p2 <- p2 + geom_hline(yintercept=cbind(params[[2]],params[[3]]), linetype="dashed", color = "blue", size=.5)
    print(p2)
  }

  episode<- function(params) {
    hypogl_continue = c(); hypo_exceeded_duration = FALSE; hypo_episode = 0;hypo_vals = c();
    hypo_day_AVG = c(); hypo_dur_Mean = c();
    hypergl_continue = c(); hyper_exceeded_duration = FALSE; hyper_episode = 0; hyper_vals = c();
    hyper_day_AVG = c(); hyper_dur_Mean = c();

    for (x in params[[1]]){

      if((x < params[[2]] && !is.na(x)) || (x > params[[3]] && !is.na(x))){
        if (x < params[[2]]){
          hypo_vals <- c(hypo_vals, x)
          hypogl_continue <- c(hypogl_continue, TRUE)
        }
        else{
          hyper_vals <- c(hyper_vals, x)
          hypergl_continue <- c(hypergl_continue, TRUE)
        }

        if(length(hypogl_continue) * params[[5]] > params[[4]] && !is.na(hypogl_continue))
          hypo_exceeded_duration = TRUE
        if(length(hypergl_continue) * params[[5]] > params[[4]] && !is.na(hypergl_continue))
          hyper_exceeded_duration = TRUE
      }
      else{
        if (hypo_exceeded_duration == TRUE){
          hypo_day_AVG <- c(hypo_day_AVG, mean(hypo_vals))
          hypo_dur_Mean <- c(hypo_dur_Mean, length(hypo_vals *params[[5]]))
          hypo_episode = hypo_episode + 1
        }
        else if(hyper_exceeded_duration == TRUE){
          hyper_day_AVG <- c(hyper_day_AVG, mean(hyper_vals))
          hyper_dur_Mean <- c(hyper_dur_Mean, length(hyper_vals *params[[5]]))
          hyper_episode = hyper_episode + 1
        }
        hypogl_continue = c(); hypo_exceeded_duration = FALSE
        hypergl_continue = c(); hyper_exceeded_duration = FALSE
      }

    }
    #episodes = c(hypo_episode, hyper_episode)
    #dur_mean = c(mean(hypo_dur_Mean), mean(hyper_dur_Mean))
    #day_AVG = c(mean(hypo_day_AVG), mean(hyper_day_AVG))
    dataframe <- data.frame( "hypo_ep" = hypo_episode, "hyper_ep" = hyper_episode,
                             "hypo_ep_mean" = mean(hypo_dur_Mean), "hyper_ep_mean" = mean(hyper_dur_Mean),
                             "hypo_avg" = mean(hypo_day_AVG), "hyper_avg" = mean(hyper_day_AVG))
    
    return (dataframe)
  }

  plot_episode(params)

  result = episode(params)

  return (result)

}#end Function

episode_calculation(example_data_5_subject)

#data_ip = CGMS2DayByDay(example_data_5_subject, dt0 = 5)

