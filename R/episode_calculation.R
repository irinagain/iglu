library(iglu)

example_data = CGMS2DayByDay(example_data_5_subject)
subject_5 = example_data[[1]][4,]
subject_1

print(example_data[3])


episode_calculation <- function(user_data, hypo=91.0, hyper= 120.0){

  plot(user_data,pch =19, col = ifelse(user_data < hypo |(user_data > hyper), "red", "black"), xlab="Time (5 min intervals)", ylab="Glucose Profile (mg/dL)", main= "Subject 1 Glucose levels")

  #Threshold lines
  abline(h=c(0,hypo), col="red")
  abline(h=c(0,hyper), col="red")

  #Initializing variables
  hypogl_continue = c()
  hypo_current_state = FALSE
  hypo_exceeded_fiftmin = FALSE

  hypergl_continue = c()
  hyper_current_state = FALSE
  hyper_exceeded_fiftmin = FALSE

  hypo_episode = 0
  hyper_episode = 0

  hypogl_container = c()
  hypo_duration_mean_episodes_per_day = 0
  hypo_episode_dayAvg =c()
  hypo_duration_list = c()

  hypergl_container = c()
  hyper_duration_mean_episodes_per_day = 0
  hyper_episode_dayAvg =c()

  hyper_duration_list = c()
  # Initilization ended


  # Loop until 288
  for (x in user_data){

    # If Hypoglycemia

    #Counting number of Hypoglycemia episodes if it continues more than 15 mins
    if(x < hypo && !is.na(x)){

      current_hypo_state = TRUE

      # Store a boolean value (Current_hypo_state) to check if it exceeds 15 minutes
      # Ex) [TRUE TRUE TRUE] means it exceeded the threshold 15 minutes since it happens three times in sequence.
      # Else we are going to disgard values
      hypogl_continue <- c(hypogl_continue,current_hypo_state)

      # Storing the value to calculate duration, mean time, and average value for episodes
      # Only it exceeds 15 mins
      hypogl_container <- c(hypogl_container, x)


      if(length(hypogl_continue) >= 3 && !is.na(hypogl_continue)){

        hypo_exceeded_fiftmin = TRUE
      }
    }
    else{

      if (hypo_exceeded_fiftmin == TRUE){

        # Add one if its duration exceeded 15 mins
        hypo_episode = hypo_episode + 1

        # Storing each episode's mean value into a list for hypoglycemia
        hypo_episode_dayAvg = c(hypo_episode_dayAvg, mean(hypogl_container))

        # Storing duration for each episode
        hypo_duration_list = c(hypo_duration_list, length(hypogl_container) * example_data[3])
      }
      hypogl_container = c()
      current_hypo_state = FALSE
      hypogl_continue = c()
      hypo_exceeded_fiftmin = FALSE

    }

    #Counting number of Hyperglycemia episodes
    if(x > hyper && !is.na(x)){

      current_hyper_state = TRUE

      # Store a boolean value (Current_hyper_state)  to check if it exceeds 15 minutes
      # Ex) [TRUE TRUE TRUE] means it exceeded the threshold 15 minutes since it happens three times in sequence.
      # Else we are going to disgard values
      hypergl_continue <- c(hypergl_continue,current_hyper_state)

      # Storing the value to calculate duration, mean time, and average value for episodes
      # Only it exceeds 15 mins
      hypergl_container <- c(hypergl_container, x)

      if(length(hypergl_continue) >= 3 && !is.na(hypergl_continue)){

        hyper_exceeded_fiftmin = TRUE
      }
    }
    else{

      if (hyper_exceeded_fiftmin == TRUE){

        # Add one if its duration exceeded 15 mins
        hyper_episode = hyper_episode + 1

        # Storing each episode's mean value into a list for hyperglycemia
        hyper_episode_dayAvg = c(hyper_episode_dayAvg, mean(hypergl_container))

        # Storing duration for each episode
        hyper_duration_list = c(hyper_duration_list, length(hypergl_container) * example_data[3])
      }
      hypergl_container = c()
      current_hyper_state = FALSE
      hypergl_continue = c()
      hyper_exceeded_fiftmin = FALSE


    }
  }

  #Calculating mean time in an episode


  #Outputting results
  output_hypo <- ("Number of Hypoglycemic episodes: ")
  output_hyper <- ("Number of Hyperglycemic episodes: ")

  print(paste(output_hypo, hypo_episode))
  print(paste(output_hyper,hyper_episode))

  output_hypo_duration_mean <- ("Duration Mean (Hypoglycemic episode): ")
  output_hyper_duration_mean <- ("Duration Mean (Hyperglycemic episode): ")

  hypo_duration_mean <- mean(hypo_duration_list)
  hyper_duration_mean <- mean(hyper_duration_list)

  print(paste(output_hypo_duration_mean, hypo_duration_mean))
  print(paste(output_hyper_duration_mean, hyper_duration_mean))

  hypo_episode_avgVal = mean(hypo_episode_dayAvg)
  hyper_episode_avgVal = mean(hyper_episode_dayAvg)

  print(paste("Episode Average value (Hypo) : ", hypo_episode_avgVal))
  print(paste("Episode Average value (Hyper): ", hyper_episode_avgVal))


  number_of_episodes = c(hypo_episode, hyper_episode)

  duration_mean = c(hypo_duration_mean, hyper_duration_mean)

  episode_dayAvg = c(hypo_episode_avgVal, hyper_episode_avgVal)

  # Return list (# of epsidoes, duration mean, and episode day avg)
  results = c(number_of_episodes, duration_mean, episode_dayAvg)

  #print(episode_dayAvg)
  return (results)


}#end alert_system

#episode_calculation(subject_5)
