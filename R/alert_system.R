library(iglu)

data1 = data("example_data_1_subject")

example_data_1_subject

#plot(example_data_1_subject$gl)

example_data = CGMS2DayByDay(example_data_5_subject)

subject_5 = example_data[[1]][4,]

subject_5
plot(subject_1)


alert_system <- function(user_data, hypo=91.0, hyper= 130.0){

  plot(user_data)

  #plot(hypo)
  abline(h=c(0,hypo), col="red")
  abline(h=c(0,hyper), col="red")

  hypogl_continue = c()
  current_state = FALSE
  exceeded_fiftmin = FALSE
  i = 0
  i_list = list()
  episode = 0
  hypogl_min = c()
  mean_episodes_per_day = 0
  avg_min_per_day =c()

  duration_list = c()

  for (x in user_data){

      if(x < hypo && !is.na(x)){

          current_hypo_state = TRUE

          hypogl_continue <- c(hypogl_continue,current_hypo_state)

          hypogl_min <- c(hypogl_min, x)

          if(length(hypogl_continue) >= 3 && !is.na(hypogl_continue)){
            exceeded_fiftmin = TRUE
          }
      }
      else{

          if (exceeded_fiftmin == TRUE){
              episode = episode + 1
              avg_min_per_day = c(avg_min_per_day, mean(hypogl_min))
              duration_list = c(duration_list, length(hypogl_min) * 5)
              print(length(hypogl_min))
          }

          hypogl_min = c()
          current_hypo_state = FALSE
          hypogl_continue = c()
          exceeded_fiftmin = FALSE

      }
  }
  #(hypogl)
  #print(i_list)
  mean_episodes_per_day = mean(avg_min_per_day)
  cat("\nduration list: ", mean(duration_list))
  cat("\nmean episodes per day: ",mean_episodes_per_day)
  #print(avg_min_per_day)
  #print("")
  return (episode)
}
alert_system(subject_5)

#subject_1
#subject_5

