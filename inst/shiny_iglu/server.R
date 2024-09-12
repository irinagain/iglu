shinyServer(function(input, output) {
  ############################## DATA SECTION ############################

  data <- reactive({
    req(input$datafile)
    # if input is already processed just read.csv
    # else select associated sensor type
    out = switch(input$datatype,
                 "processed" = read.csv(input$datafile$datapath),
                 "FreeStyle Libre" = iglu::read_raw_data(input$datafile$datapath, sensor = "libre", id = input$subjid),
                 "Dexcom" = iglu::read_raw_data(input$datafile$datapath, sensor = "dexcom", id = input$subjid),
                 "Libre Pro" = iglu::read_raw_data(input$datafile$datapath, sensor = "librepro", id = input$subjid),
                 "ASC" = iglu::read_raw_data(input$datafile$datapath, sensor = "asc", id = input$subjid),
                 "iPro" = iglu::read_raw_data(input$datafile$datapath, sensor = "ipro", id = input$subjid)
    )
    return(out)
  })

  output$data <- DT::renderDataTable({

    validate(
      need(input$demodata != '', "Please select Dataset")
    )

    DT::datatable(
      (if (input$demodata == "user_data") {
        out <- data()
      } else if (input$demodata == "example_data") {
        out <- iglu::example_data_5_subject
        out$time <- as.character(out$time)
        out
      }),
      extensions = "Buttons",
      options = list(dom = "Btip",
                     buttons = c("copy", "csv", "excel", "pdf", "print"),
                     paging = FALSE)
    )
  })

  output$downloaddata <- downloadHandler(
    filename = function() {
      filename <- paste0(gsub("\\.csv", "",basename(input$datafile$name)), "_processed")
      paste0(filename, ".csv")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )

  transform_data <- reactive({

    req(input$demodata)

    if (input$demodata == "user_data") {
      data = data()
    } else if (input$demodata == "example_data") {
      data = iglu::example_data_5_subject
    }

    # what does this line do?
    # iglu:::read_df_or_vec(data, id = input$id, time = input$time, gl = input$gl)

    return(data)
  })

  ############################# METRIC SECTION ######################################################
  #add metric based on the parameter it takes in
  parameter_type <- reactive({
    #metric is considered as parameter type "none" if it only requires data as a parameter
    if(input$metric %in% c("adrr", "cv_glu", "ea1c", "gmi", "cv_measures", "episode_calculation", "grade", "gri", "gvp", "hbgi", "iqr_glu", "j_index", "lbgi",
                           "mean_glu", "median_glu", "range_glu", "sd_glu", "sd_measures", "summary_glu")){
      return("none")
    }
    else if(input$metric == "all_metrics") {
      return("all_metrics")
    }
    #metric is considered as parameter type "time" if it takes in data and time zone as parameters
    else if(input$metric %in% c("auc","cv_measures", "sd_measures")){
      return("time")
    }
    #metric is considered as parameter type "list" if it takes in data and a list of values as parameters
    else if(input$metric %in% c("above_percent", "below_percent", "cogi", "quantile_glu")){
      return("list")
    }
    #metric is considered as parameter type "value" if it takes in data and a single value as parameters
    else if(input$metric %in% c("grade_hyper", "grade_hypo","m_value","mad_glu", "active_percent")){
      return("value")
    }
    else if(input$metric == 'mage') {
      return("mage")
    }
    else if(input$metric == 'pgs'){
      return('pgs')
    }
    else if(input$metric %in% c("hyper_index", "hypo_index")){
      return("value1")
    }
    #metric is considered as parameter type "value_time" if it takes in data, a single value, and timezone as parameters
    else if(input$metric %in% c("conga","mag","modd","roc","sd_roc")){
      return("value_time")
    }
    #metric is considered as parameter type "lwrupr" if it takes in data, lower threshold, and upper threshold as parameters
    else if(input$metric %in% c("grade_eugly")){
      return("lwrupr")
    }
    else if(input$metric %in% c("igc")){
      return("lwrupr1")
    }
    #metric is considered as parameter type "nested" if it takes in data and a list of ranges
    else if(input$metric %in% c("in_range_percent")){
      return("nested")
    }
  })

  #specify first parameter and the default values
  output$select_parameter <- renderUI({
    parameter_type = parameter_type()

    if(parameter_type == "list"){
      if(input$metric == "above_percent"){
        textInput("parameter", "Specify Parameter", value = "140, 180, 250")
      }

      else if(input$metric == "below_percent"){
        textInput("parameter", "Specify Parameter", value = "50, 80")
      }

      else if(input$metric == "cogi"){
        textInput("parameter", "Specify Parameter", value = "70, 180")
      }

      else if(input$metric == "quantile_glu"){
        textInput("parameter", "Specify Parameter", value = "0, 25, 50, 75, 100")
      }
    }
    else if(parameter_type == "all_metrics") {
      if(input$metric == "all_metrics") {
        radioButtons("parameter", "Metrics to Include", choiceNames=c('All', 'Consensus Only'), choiceValues=c('all', 'consensus_only'))
      }
    }
    else if(parameter_type == "mage") {
      if(input$metric == "mage") {
        radioButtons("parameter", "Mage Version", choiceNames=c('Version 2: ma (recommended)', 'Version 1: naive (superseded)'),choiceValues=c("ma","naive"))
      }
    }
    else if(parameter_type == 'pgs') {
      textInput('parameter', 'Episode Duration', value = 20)
    }
    else if(parameter_type == "value"){

      if(input$metric == "grade_hyper"){
        textInput("parameter", "Specify Parameter", value = "140")
      }

      else if(input$metric == "grade_hypo"){
        textInput("parameter", "Specify Parameter", value = "80")
      }

      else if(input$metric == "m_value"){
        textInput("parameter", "Specify Reference Value", value = "90")
      }

      else if(input$metric == "mad_glu"){
        textInput("parameter", "Specify Parameter", value = "1.4826")
      }

      else if(input$metric == "active_percent"){
        textInput("parameter", "Specify Parameter", value = "5")
      }
    }
    else if(parameter_type == "value1"){
      if(input$metric == "hyper_index"){
        textInput("parameter", "Specify Upper Limit", value = "180")
      }

      else if(input$metric == "hypo_index"){
        textInput("parameter", "Specify Lower Limit", value = "70")
      }
    }
    else if(parameter_type == "value_time"){
      if(input$metric == "conga"){
        textInput("parameter", "Specify Parameter", value = "24")
      }

      else if(input$metric == "mag"){
        textInput("parameter", "Specify Parameter", value = "60")
      }
      else if(input$metric == "modd"){
        textInput("parameter", "Specify Parameter", value = "1")
      }
      else if(input$metric == "roc"){
        textInput("parameter", "Specify Parameter", value = "15")
      }

      else if(input$metric == "sd_roc"){
        textInput("parameter", "Specify Parameter", value = "15")
      }
    }
    else if(parameter_type == "lwrupr"){
      if(input$metric == "grade_eugly"){
        textInput("parameter", "Specify Parameter", value = "80, 140")
      }

    }
    else if(parameter_type == "lwrupr1"){
      if(input$metric == "igc"){
        textInput("parameter", "Specify Lower and Upper Limits", value = "70, 180")
      }
      else if(input$metric == "episode_calculation"){
        textInput("parameter", "Specify Parameter", value = "70, 54")
      }
    }
    else if(parameter_type == "nested"){

      if(input$metric == "in_range_percent"){
        textInput("parameter", "Specify Parameter", value = "(80, 200), (70, 180), (70,140)")
      }
    }

  })

  #add description of first parameter
  output$help_text <- renderUI({
    parameter_type = parameter_type()

    if(parameter_type == "none"){
      helpText("No parameters need to be specified.")
    }
    else if(parameter_type == "time"){
      helpText("No parameters need to be specified.")
    }

    else if(parameter_type == "list"){
      if(input$metric == "above_percent"){
        helpText("Enter target glucose thresholds separated by comma.")
      }

      else if(input$metric == "below_percent"){
        helpText("Enter target glucose thresholds separated by comma.")
      }

      else if(input$metric == "cogi"){
        helpText("Enter lower and upper glucose limits separated by comma")
        helpText("Default weights applie to time in range, time below range,
                 and glucose variability, respectively, are (.5, .35, .15)")

      }

      else if(input$metric == "quantile_glu"){
        helpText("Enter quantile values separated by comma.")
      }
    }

    else if(parameter_type == "value"){

      if(input$metric == "active_percent"){
        helpText("Enter CGM frequency in minutes.")
      }

      else if(input$metric == "grade_hyper"){
        helpText("Enter the upper bound hyperglycemia cutoff value.")
      }

      else if(input$metric == "grade_hypo"){
        helpText("Enter the lower bound hypoglycemia cutoff value.")
      }

      else if(input$metric == "m_value"){
        helpText("Enter the reference value for normal basal glycemia.")
      }

      else if(input$metric == "active_percent"){
        helpText("Enter CGM frequency in minutes.")
      }
      else if(input$metric == "mad_glu"){
        helpText("Enter the value of the scaling factor.")
      }

      else if(input$metric == "mage"){
        helpText("Enter algorithm version in single quotes: 'ma' (default) or 'naive'")
      }

    }
    else if(parameter_type == 'pgs'){
      helpText('Enter minimum duration in minutes for an episode')
    }
    else if(parameter_type == "value1"){
      if(input$metric == "hyper_index"){
        helpText("Enter the upper limit of target glucose range.")
      }

      else if(input$metric == "hypo_index"){
        helpText("Enter the lower limit of target glucose range.")
      }
    }
    else if(parameter_type =="value_time"){
      if(input$metric == "conga"){
        helpText("Enter the hours between observations for the CONGA calculation.")
      }
      else if(input$metric == "mag"){
        helpText("Enter the interval (in minutes) to calculate change in glucose.")
      }
      else if(input$metric == "modd"){
        helpText("Enter the lag in days.")
      }
      else if(input$metric == "roc"){
        helpText("Enter time interval (in minutes) for rate of change.")
      }

      else if(input$metric == "sd_roc"){
        helpText("Enter time interval (in minutes) for rate of change.")
      }

    }
    else if(parameter_type == "lwrupr"){
      if(input$metric == "grade_eugly"){
        helpText("Enter a lower and an upper glycemic bound separated by a comma.")
      }
    }
    else if(parameter_type == "lwrupr1"){
      if(input$metric == "igc"){
        helpText("Enter the lower and upper limits of the target range separated by a comma.")
      }
    }
    else if(parameter_type == "nested"){
      if(input$metric == "in_range_percent"){
        helpText("Enter target ranges in list format - e.g. (lower, upper), (lower, upper)")
      }
    }
  })

  #specify second parameter and its default values
  output$select_second_parameter <- renderUI({
    parameter_type = parameter_type()
    if(parameter_type == "value1"){
      if(input$metric == "hyper_index"){
        textInput("parameter2", "Specify Exponent", value = "1.1")
      }

      else if(input$metric == "hypo_index"){
        textInput("parameter2", "Specify Exponent", value = "2")
      }
    }
    else if(parameter_type == "lwrupr1"){
      if(input$metric == "igc"){
        textInput("parameter2", "Specify Exponent", value = "1.1, 2")
      }

    }
    else if(parameter_type == "mage") {
      if(input$parameter == "ma") {
        textInput("parameter2", "Short MA length", value="5")
      }
    }
    else if(parameter_type == 'pgs') {
      textInput('parameter2', 'Episode Ending Duration', value = '30')
    }
  })

  #add description of second parameter
  output$second_parameter_helptext <- renderUI({
    parameter_type = parameter_type()
    if(parameter_type == "value1"){
      if(input$metric == "hyper_index"){
        helpText("Enter the upper limit exponent.")
      }

      else if(input$metric == "hypo_index"){
        helpText("Enter the lower limit exponent.")
      }
    }
    else if(parameter_type == "lwrupr1"){
      if(input$metric == "igc"){
        helpText("Enter the exponents separated by a comma with the upper limit as the first input and the lower limit as the second input.")
      }
    }
    else if(parameter_type == "mage"){
      if(input$parameter == "ma") {
        helpText("Allowable values between 5 and 15. (5 recommended)")
      }
    }
    else if(parameter_type == 'pgs'){
      helpText("Enter minimum duration in minutes of improved glycemia for episodes to end")
    }

  })

  #specify third parameter and its default value
  output$select_third_parameter <- renderUI({
    parameter_type = parameter_type()
    if(parameter_type == "value1"){
      if(input$metric == "hyper_index"){
        textInput("parameter3", "Specify Scaling Factor", value = "30")
      }

      else if(input$metric == "hypo_index"){
        textInput("parameter3", "Specify Scaling Factor", value = "30")
      }
    }

    else if(parameter_type == "lwrupr1"){
      if(input$metric == "igc"){
        textInput("parameter3", "Specify Scaling Factor", value = "30,30")
      }
    }
    else if(parameter_type == "mage") {
      if(input$parameter == "ma") {
        textInput("parameter3", "Long MA length", value="32")
      }
    }
  })

  #add description on third parameter
  output$third_parameter_helptext <- renderUI({
    parameter_type = parameter_type()
    if(parameter_type == "value1"){
      if(input$metric == "hyper_index"){
        helpText("Enter the upper limit scaling factor.")
      }

      else if(input$metric == "hypo_index"){
        helpText("Enter the lower limit scaling factor.")
      }
    }

    else if(parameter_type == "lwrupr1"){
      if(input$metric == "igc"){
        helpText("Enter the scaling factors separated by a comma with the upper limit as the first input and the lower limit as the second input.")
      }
    }
    else if(parameter_type == "mage"){
      if(input$parameter == "ma") {
        helpText("Allowable values between 20 and 40. (32 recommended)")
      }
    }
  })

  #specify fourth parameter and its default value
  output$select_fourth_parameter <- renderUI({
    parameter_type = parameter_type()

    if(parameter_type == "mage") {
      if(input$parameter == "ma") {
        radioButtons("parameter4", "Direction", choiceNames=c('Average', 'Service', 'Max', 'Plus', 'Minus'),choiceValues=c('avg', 'service', 'max', 'plus', 'minus'))
      }
    }
  })

  #add description on fourth parameter
  output$fourth_parameter_helptext <- renderUI({
    parameter_type = parameter_type()

    if(parameter_type == "mage"){
      if(input$parameter == "ma") {
        helpText("Whether you calculate MAGE+, MAGE-, MAGEservice, MAGEavg, or MAGEmax")
      }
    }
  })

  #specify third parameter and its default value
  output$select_fifth_parameter <- renderUI({
    parameter_type = parameter_type()

    if(parameter_type == "mage") {
      if(input$parameter == "ma") {
        textInput("parameter5", "Max Gap", value="180")
      }
    }
  })

  #add description on third parameter
  output$fifth_parameter_helptext <- renderUI({
    parameter_type = parameter_type()

    if(parameter_type == "mage"){
      if(input$parameter == "ma") {
        helpText("Maximum gap (minutes) in the glucose trace before MAGE is calculated on each segment independently. Recommended: >=120 min")
      }
    }
  })

  output$sleep_wake_help <- renderUI({
    helpText("Enter a real number 0-24.")
  })

  # reactive function
  metric_table <- reactive({
    string = ""
    parameter_type = parameter_type()
    data = transform_data()

    #### Validate catches for parameter 1 #####
    if (is.null(input$parameter)) {
      validate(
        need(!is.null(input$parameter), "Please wait - Rendering")
      )
    } else if (input$parameter %in% c("ma", "naive")) {
      validate (
        need (input$metric == "mage" | parameter_type == "none",
              "Please wait - Rendering")
      )
    }
    else if (parameter_type == "all_metrics") {
      validate (
        need(input$parameter %in% c("all", "consensus_only"), "Please wait - Rendering")
      )
    }
    else if (grepl(',', input$parameter) & !grepl("\\(", input$parameter)) {
      if (length(strsplit(input$parameter, split = ",")[[1]]) != 2) {
        validate (
          need(parameter_type %in% c("list", "none","time"), "Please wait - Rendering")
        )
      } else {
        validate(
          need(parameter_type %in% c("list", "lwrupr","lwrupr1","none","time"), "Please wait - Rendering")
        )
      }
    } else if (grepl("\\(", input$parameter)) {
      validate(
        need(parameter_type %in% c("nested", "none","time"), "Please wait - Rendering")
      )
    } else if (!grepl(',', input$parameter)) {
      validate(
        need(parameter_type %in% c("value", "value1", "value_time", "none","time", "pgs"),
             "Please wait - Rendering")
      )
    }

    # because MAGE input is unique (character)
    if (input$metric == "mage") {
      validate(
        # print message instead of warning
        need(input$parameter %in% c("ma", "naive"), "Parameter must be one of ma, or naive")
      )
    }

    ##### Validate for parameters2/3 ####

    ## parameter2 and parameter3 currently (7/29/21) include the same 3 metrics
    ## hyper index, hypo index, and igc

    if (input$metric %in% c("hyper_index", "hypo_index", "mage")) {
      validate(
        need(!grepl(",", input$parameter3), "Please wait - Rendering")
      )
    }

    #loading iglu library and using metric function
    library(iglu)
    if(is.null(input$parameter) | parameter_type == "none"){
      string = paste0("iglu::", input$metric, "(data)")
    }
    else if(is.null(input$parameter) | parameter_type == "time"){
      string = paste0("iglu::", input$metric, "(data,", "tz=","'",input$tz,"')")
    }

    else if(parameter_type == "list"){
      string = paste0("iglu::", input$metric, "(data, c(", input$parameter, "))")
    }

    else if(parameter_type == "value"){
      string = paste0("iglu::", input$metric, "(data, ", input$parameter, ")")
    }
    else if(parameter_type == "value1"){
      string = paste0("iglu::", input$metric, "(data, ", input$parameter, ",",input$parameter2,",",input$parameter3, ")")
    }
    else if(parameter_type == "value_time"){
      string = paste0("iglu::", input$metric, "(data, ", input$parameter, ",","tz=" ,"'", input$tz,"'" ,")")
    }
    else if(parameter_type == "lwrupr"){
      string = paste0("iglu::", input$metric, "(data, " , input$parameter, ")")
    }
    else if(parameter_type == "lwrupr1"){
      string = paste0("iglu::", input$metric, "(data, " , input$parameter,",",input$parameter2,",",input$parameter3, ")")
    }
    else if(parameter_type == "mage"){
      if(input$parameter == "ma") {
        string = paste0("iglu::", input$metric, "(data, version='",input$parameter, "', short_ma=", input$parameter2,", long_ma=",input$parameter3,", direction='", input$parameter4,"', max_gap=", input$parameter5 ,")")
      }
      else if(input$parameter == "naive") {
        string = paste0("iglu::", input$metric, "(data, version='",input$parameter,"')")
      }
    }
    else if(parameter_type == "all_metrics") {
      string = paste0("iglu::", input$metric, "(data, , , , , , '", input$parameter , "')")
      print(string)
    }
    else if(parameter_type == 'pgs'){
      string = paste0('iglu::', input$metric, '(data, ', input$parameter, ', ', input$parameter2, ')')
    }
    else if(parameter_type == "nested"){
      strlist = strsplit(input$parameter, ")")[[1]]
      paramstr = rep.int(0, length(strlist))
      if(length(strlist) == 1){
        paramstr = paste0("c", strlist[1], ")")
      }

      else {
        for(i in 2:length(strlist)){
          strlist[i] = substring(strlist[i], 3)
        }
        for(s in 1:length(strlist)){
          paramstr[s] = paste0("c", strlist[s], ")")
        }
        paramstr = paste(paramstr, collapse = ", ")
      }
      string = paste0("iglu::", input$metric, "(data, list(", paramstr, "))")
    }

    if (input$filter_sleep_wake) {

      validate (
        need(input$sleep_start != input$sleep_end, "Sleep start cannot equal sleep end, please change one of the inputs")
      )

      if (parameter_type == "none") {
        out_str = paste0("iglu::calculate_sleep_wake(data, FUN = ", input$metric, ", calculate = \'", input$sleep_or_wake, "\', sleep_start = ", input$sleep_start, ", sleep_end = ", input$sleep_end, ")")
      }
      else if (parameter_type == "list") {
        param_string = strsplit(string, "\\(data, c\\(")[[1]][2]
        argname = formalArgs(input$metric)
        out_str = paste0("iglu::calculate_sleep_wake(data, FUN = ", input$metric, ", calculate = \'", input$sleep_or_wake, "\', sleep_start = ", input$sleep_start, ", sleep_end = ", input$sleep_end, ", ", argname[2], " = c(", param_string)
      }
      else if (parameter_type == "nested") {
        param_string = strsplit(string, "\\(data, list\\(")[[1]][2]
        argname = formalArgs(input$metric)
        out_str = paste0("iglu::calculate_sleep_wake(data, FUN = ", input$metric, ", calculate = \'", input$sleep_or_wake, "\', sleep_start = ", input$sleep_start, ", sleep_end = ", input$sleep_end, ", ", argname[2], " = list(", paramstr, "))")
      } else if (parameter_type == "time") {
        out_str = paste0("iglu::calculate_sleep_wake(data, FUN = ", input$metric, ", calculate = \'", input$sleep_or_wake, "\', sleep_start = ", input$sleep_start, ", sleep_end = ", input$sleep_end, ", ", "tz='", input$tz, "')")
      } else if (parameter_type == "mage") {
        if(input$parameter == "ma") {
          out_str = paste0("iglu::calculate_sleep_wake(data, FUN = ",input$metric," , calculate = '",input$sleep_or_wake,"' , sleep_start=",input$sleep_start,",sleep_end=",input$sleep_end,
                           ", version='",input$parameter, "', short_ma=", input$parameter2,", long_ma=",input$parameter3,")")
        }
        else if(input$parameter == "naive") {
          out_str = paste0("iglu::calculate_sleep_wake(data, FUN = ",input$metric," , calculate = '",input$sleep_or_wake,"' , sleep_start=",input$sleep_start,",sleep_end=",input$sleep_end, ", version='naive')")
        }
      }
      else {
        param_string = strsplit(string, "\\(data")[[1]][2]
        list_of_params = strsplit(param_string, ", ")[[1]]
        list_of_params = list_of_params[2:length(list_of_params)]
        build_str = c(paste0("iglu::calculate_sleep_wake(data, FUN = ", input$metric, ", calculate = \'", input$sleep_or_wake, "\', sleep_start = ", input$sleep_start, ", sleep_end = ", input$sleep_end))
        argnames = formalArgs(input$metric)
        for (index in 1:length(list_of_params)) {
          build_str = c(build_str, paste0(argnames[index+1], " = ", list_of_params[index]))
        }
        out_str = paste(build_str, collapse = ", ")
      }
      string = out_str
    }

    eval(parse(text = string))

  })

  output$metric <- DT::renderDataTable(metric_table(), extensions = "Buttons",
                                       options = list(dom = "Btip",
                                                      buttons = c("copy", "csv", "excel", "pdf", "print"),
                                                      scrollX = TRUE))


  ############################ PLOTTING SECTION #####################################################

  plottype <- reactive({  # wrap plottype input in a reactive for rendering UI and Plot
    if(input$plottype == "tsplot"){
      return("tsplot")
    }
    else if(input$plottype == "lasagnamulti"){
      return("lasagnamulti")
    }
    else if(input$plottype == "lasagnasingle"){
      return("lasagnasingle")
    }
    else if(input$plottype == "plot_roc"){
      return("plot_roc")
    }
    else if(input$plottype == "hist_roc"){
      return("hist_roc")
    }
    else if(input$plottype == "mage") {
      return("mage")
    }
  })

  subset_data <- reactive({ # define reactive function to subset data for plotting each time user changes subjects list
    data = transform_data()
    data = data[data$id == input$plot_subjects,] # reactively subset data when subjects input is modified
    return(data)
  })

  output$plot_mage <- renderUI({
    plottype = plottype()
    if(plottype != "mage"){
      return(NULL)
    }

    data = transform_data()
    subject = unique(data$id)[1]

    tags$div(
      textInput("plot_subjects", "Enter Subject ID", value = subject), # TODO: change this to select/dropdown
      helpText("Enter the ID of a subject to display their MAGE plot"),
      textInput("mage_short_ma", "Short MA length", value="5"),
      textInput("mage_long_ma", "Long MA length", value="32"),
      selectInput("direction", "Direction", c(
        'Average' = 'avg',
        'Service' = 'service',
        'Max' = 'max',
        'Plus' = 'plus',
        'Minus' = 'minus'
      )),
      helpText("Whether you calculate MAGE+, MAGE-, MAGEservice, MAGEavg, or MAGEmax"),
      textInput("max_gap", "Max Gap", value="180"),
      helpText("Maximum gap (minutes) in the glucose trace before MAGE is calculated on each segment independently. Recommended: >=120 min"),
      radioButtons("mage_show_ma", "Show Moving Averages on Plot", c(
        "No" = FALSE,
        "Yes" = TRUE
      ), inline=TRUE),
      radioButtons("show_excursions", "Show Excursions on Plot", c(
        "No" = FALSE,
        "Yes" = TRUE
      ), selected=TRUE, inline=TRUE),
      textInput("mage_title", "Title", value = "default"),
      textInput("mage_xlab", "X Label", value = "default"),
      textInput("mage_ylab", "Y Label", value = "default"),
      # TODO: eventually incorporate Plotly
      # radioButtons("plot_type", "Select Plot Type", c(
      #   "Plotly (interactive)" = 'plotly',
      #   "GGPlot (static)" = 'ggplot'
      # ), inline=TRUE),
    )
  })

  output$lasagna_sidebar <- renderUI({
    data = transform_data()
    plottype = plottype()

    if (plottype %in% c("lasagnamulti", "lasagnasingle")) {
      outputs <- tagList()
      outputs[[5]] <- textInput("plot_TR", "Specify Lower and Upper Target Values, separated by a Comma", value = "70, 180")
      outputs[[6]] <- textInput("plot_midpoint", "Enter Midpoint Glucose Value for Color Scale", value = 105)
      outputs[[7]] <- textInput("plot_limits", "Enter Limit Glucose Values for Color Scale Separated by a Comma", value = "50, 500")
      outputs[[8]] <- helpText("The color bar can be modified by changing the values of the target range, the midpoint,
               and the color bar limits")
      outputs[[9]] <- radioButtons('plot_color_scheme', 'Transformation type',
                                   choices = c(`Blue/Red` = '"blue-red"', `Red/Orange` = '"red-orange"'))
      outputs[[10]] <- radioButtons('plot_log', 'Transformation type',
                                    choices = c(`None` = 'FALSE', `Log10` = 'TRUE'))
      if (plottype == "lasagnamulti") {
        outputs[[1]] <- radioButtons("plot_lasagnatype", "Lasagna Plot Type",
                                     choices = c(`Unsorted` = "unsorted",
                                                 `Subject-sorted` = "subjectsorted",
                                                 `Time-sorted` = "timesorted"
                                     ))
        outputs[[2]] <- textInput("plot_maxd", "Enter Maximum # of Days to Plot", value = 14)
        outputs[[3]] <- radioButtons("plot_datatype", "Data Aggregation Type",
                                     choices = c(`Average across days` = "average",
                                                 `All data points` = "all"
                                     ))
        outputs[[4]] <- helpText("Select whether to use all data points in the first maxd days, or whether
               to take the average value at each time point across the first maxd days")
      }
      else if (plottype == "lasagnasingle") {
        outputs[[1]] <- radioButtons("plot_lasagnatype", "Lasagna Plot Type",
                                     choices = c(`Unsorted` = "unsorted",
                                                 `Time-sorted` = "timesorted"))
        subject = unique(data$id)[1]
        outputs[[2]] <- textInput("plot_subjects", "Enter Subject ID", value = subject)
        outputs[[3]] <- helpText("Enter the ID of a subject to display their individualized lasagna plot")
        outputs[[4]] <- NULL

      }
    } else {outputs = NULL}

    outputs
  })

  output$tsplot_sidebar <- renderUI({
    data = transform_data()
    plottype = plottype()

    if (plottype == "tsplot") {
      outputs <- tagList()
      outputs[[1]] <- textInput("plot_TR", "Specify Lower and Upper Target Values, separated by a Comma", value = "70, 180")
      outputs[[2]] <- radioButtons('plot_log', 'Transformation type',
                                   choices = c(`None` = 'FALSE', `Log10` = 'TRUE'))
    } else {outputs = NULL}

    outputs
  })

  output$rocplots_sidebar <- renderUI({
    data = transform_data()
    plottype = plottype()

    if (plottype %in% c("plot_roc", "hist_roc")) {
      outputs <- tagList()
      subject = unique(data$id)[1]
      outputs[[1]] <- textInput("plot_subjects", "Enter Subject ID", value = subject)
      outputs[[2]] <- helpText("Enter the ID of a subject to display their individualized SD of ROC histogram")
      outputs[[3]] <- textInput("plot_timelag", "Enter Timelag for ROC calculation", value = 15)
    } else {outputs = NULL}

    outputs
  })

  ### Render Plot

  plotFunc <- reactive({
    library(iglu)

    plottype = plottype() # bring reactive input variable into this renderPlot call

    if(plottype == "tsplot"){
      #plot_glu(data, plottype = "tsplot")

      validate (
        need(all(!is.null(input$plot_log)),
             "Please wait - Rendering")
      )

      data = transform_data()
      string = paste0('iglu::plot_glu(data = data, plottype = "tsplot", datatype = "all", lasagnatype = NULL, ',
                     input$plot_TR, ', subjects = NULL, inter_gap = 45, tz = "", "blue-orange", log = ', input$plot_log, ')')
      eval(parse(text = string))
    }
    else if(plottype == "lasagnamulti"){

      validate (
        need(all(!is.null(input$plot_lasagnatype) & !is.null(input$plot_datatype)),
             "Please wait - Rendering")
      )

      data = transform_data()
      string = paste0('iglu::plot_lasagna(data = data, datatype = "', input$plot_datatype, '", lasagnatype = "',
                     input$plot_lasagnatype, '", maxd = ', input$plot_maxd, ', limits = c(', input$plot_limits, ') ,',
                     input$plot_midpoint, ', ', input$plot_TR, ', dt0 = NULL, inter_gap = 60, tz ="",',
                     input$plot_color_scheme, ', ', input$plot_log, ')')
      eval(parse(text = string))
    }
    else if(plottype == "lasagnasingle"){

      validate (
        need(!is.null(input$plot_subjects), "Please wait - Rendering")
      )

      data = subset_data() # subset data to only user-specified subject
      string = paste0('iglu::plot_lasagna_1subject(data = data, lasagnatype = "',
                     input$plot_lasagnatype, '", limits = c(', input$plot_limits, ') ,',
                     input$plot_midpoint, ', ', input$plot_TR, ', dt0 = NULL, inter_gap = 60, tz = "",',
                     input$plot_color_scheme, ', ', input$plot_log, ')')
      eval(parse(text = string))
    }
    else if(plottype == "plot_roc"){
      data = subset_data() # subset data to only user-specified subject
      string = paste0('iglu::plot_roc(data = data',
                     ', timelag = ', input$plot_timelag, ', tz = "")')
      eval(parse(text = string))
    }
    else if(plottype == "hist_roc"){
      data = subset_data() # subset data to only user-specified subject
      string = paste0('iglu::hist_roc(data = data',
                     ', timelag = ', input$plot_timelag, ', tz = "")')
      eval(parse(text = string))
    }
    else if(plottype == "mage"){
      validate (
        need(all(!is.null(input$mage_short_ma), !is.null(input$mage_long_ma)), "Please wait - Rendering")
      )

      data = subset_data() # subset data to only user-specified subject

      mage_title = ifelse(tolower(input$mage_title) == "default", NA, paste0("'",input$mage_title,"'"))
      mage_xlab = ifelse(tolower(input$mage_xlab) == "default", NA, paste0("'",input$mage_xlab,"'"))
      mage_ylab = ifelse(tolower(input$mage_ylab) == "default", NA, paste0("'",input$mage_ylab,"'"))

      string = paste0("iglu::mage_ma_single(data=data,plot=TRUE,short_ma=",input$mage_short_ma,",long_ma=",
                      input$mage_long_ma,", direction='", input$direction, "', max_gap='", input$max_gap,
                      "', show_ma=", input$mage_show_ma,", show_excursions=", input$show_excursions, ",title=",mage_title,",xlab=", mage_xlab,",ylab=",mage_ylab,")")

      eval(parse(text=string))
    }
  })

  # TODO: Eventually incorporate Shiny for MAGE, using: https://stackoverflow.com/questions/48017341/how-to-correctly-output-plotly-plots-in-shiny
  output$plot <- renderPlot({
        plotFunc()
      })

  options(shiny.usecairo = T)

  output$pdfButton <- downloadHandler(
    filename = function() {
      plottype = plottype()
      paste0(plottype, '.pdf')
    },
    content = function(file) {
      cairo_pdf(filename = file, width = 20, height = 18, bg = "transparent")
      plot(plotFunc())
      dev.off()
    }
  )

  output$pngButton <- downloadHandler(
    filename = function() {
      plottype = plottype()
      paste0(plottype, '.png')
    },
    content = function(file) {
      png(file)
      plot(plotFunc())
      dev.off()
    }
  )

  output$epsButton <- downloadHandler(
    filename = function() {
      plottype = plottype()
      paste0(plottype, '.eps')
    },
    content = function(file) {
      postscript(file)
      plot(plotFunc())
      dev.off()
    }
  )

  ############################ AGP SECTION #####################################################

  ### Get desired subject
  output$agp_subject <- renderUI({
    data = transform_data() # bring reactive data input into this renderUI call to default to all subjects
    subject = unique(data$id)[1]
    textInput("agp_subject", "Enter Subject ID", value = subject)
  })

  output$agp_subject_help_text <- renderUI({
    helpText("Enter the ID of a subject to display their AGP Report")
  })

  output$agp_span <- renderUI({
    sliderInput("agp_span", "Enter amount of smoothing for AGP plot",
                min = 0.1, max = 0.7, value = 0.3, step = 0.1)
  })

  agp_data <- reactive({ # define reactive function to subset data for plotting each time user changes subjects list

    validate (
      if (is.null(input$agp_subject)) {
        need(!is.null(input$agp_subject), "Please wait - Rendering")
      } else {
        need(input$agp_subject %in% transform_data()$id, "Please check Subject ID")
      }
    )

    data = transform_data()
    data = data[data$id == input$agp_subject,] # reactively subset data when subjects input is modified
    return(data)
  })

  ### Rendering Sections

  agpMetrics <- reactive({

    library(iglu)
    data = agp_data()
    string = paste("iglu::agp_metrics(data = data, shinyformat = TRUE)")
    eval(parse(text = string))
  })

  output$agp_metrics <- DT::renderDataTable({
    DT::datatable(agpMetrics(), options = list(dom = 't'), rownames = FALSE, colnames = "")
  })

  plotRanges <- reactive({

    library(iglu)
    data = agp_data()
    string = paste('iglu::plot_ranges(data = data)')
    eval(parse(text = string))
  })

  output$plot_ranges <- renderPlot({

    plotRanges()
  })

  plotAGP <- reactive({

    library(iglu)
    data = agp_data()
    string = paste0('iglu::plot_agp(data = data, smooth = TRUE, ', 'span = ',
                    input$agp_span, ')')
    eval(parse(text = string))
  })

  output$plot_agp <- renderPlot({
    plotAGP()
  })

  plotDaily <- reactive({
    library(iglu)
    data = agp_data()
    string = paste('iglu::plot_daily(data = data)')
    eval(parse(text = string))
  })

  output$plot_daily <- renderPlot({
    plotDaily()
  })

  options(shiny.usecairo = T)

  output$pdfAGP<- downloadHandler(
    filename = function() {
      paste0("AGP", '.pdf')
    },
    content = function(file) {
      cairo_pdf(filename = file, width = 20, height = 18, bg = "transparent")
      p = gridExtra::grid.arrange(gridExtra::arrangeGrob(gridExtra::tableGrob(agpMetrics(), rows = NULL),
                                                         plotRanges(), ncol = 2), plotAGP(), plotDaily())
      plot(p)
      dev.off()
    }
  )

  output$pngAGP <- downloadHandler(
    filename = function() {
      paste0("AGP", '.png')
    },
    content = function(file) {
      png(file)
      p = gridExtra::grid.arrange(gridExtra::arrangeGrob(gridExtra::tableGrob(agpMetrics(), rows = NULL),
                                                         plotRanges(), ncol = 2), plotAGP(), plotDaily())
      plot(p)
      dev.off()
    }
  )

  output$epsAGP <- downloadHandler(
    filename = function() {
      paste0("AGP", '.eps')
    },
    content = function(file) {
      postscript(file)
      p = gridExtra::grid.arrange(gridExtra::arrangeGrob(gridExtra::tableGrob(agpMetrics(), rows = NULL),
                                                         plotRanges(), ncol = 2), plotAGP(), plotDaily())
      plot(p)
      dev.off()
    }
  )

  ########## Episode calculation section#########
  ### Get desired subject
  output$episode_subject <- renderUI({
    data = transform_data() # bring reactive data input into this renderUI call to default to all subjects
    subject = unique(data$id)[1]
    textInput("episode_subject", "Enter Subject ID", value = subject)
  })

  episode_data <- reactive({ # define reactive function to subset data for plotting each time user changes subjects list
    validate (
      if (is.null(input$episode_subject)) {
        need(!is.null(input$episode_subject), "Please wait - Rendering")
      } else {
        need(input$episode_subject %in% transform_data()$id, "Please check Subject ID")
      }
    )

    data = transform_data()
    data = data[data$id == input$episode_subject,] # reactively subset data when subjects input is modified
    return(data)
  })

  plotEpisodeCalc <- reactive({
    library(iglu)
    data = episode_data()
    lv1_hypo = input$lv1hypoThreshold
    lv2_hypo = input$lv2hypoThreshold
    lv1_hyper = input$lv1hyperThreshold
    lv2_hyper = input$lv2hyperThreshold
    dur_length = input$DurationLength
    string = paste('iglu::epicalc_profile(data = data, lv1_hypo= lv1_hypo, lv2_hypo = lv2_hypo,
                   lv1_hyper=lv1_hyper,lv2_hyper=lv2_hyper, dur_length=dur_length)')
    eval(parse(text = string))
  })

  output$plot_episode_calculation <- renderPlot({
    plotEpisodeCalc()
  })

  output$pdfEpisode<- downloadHandler(
    filename = function() {
      paste0("Episode", '.pdf')
    },
    content = function(file) {
      cairo_pdf(filename = file, width = 20, height = 18, bg = "transparent")
      p = plotEpisodeCalc()
      plot(p)
      dev.off()
    }
  )

  output$pngEpisode <- downloadHandler(
    filename = function() {
      paste0("Episode", '.png')
    },
    content = function(file) {
      png(file)
      p = plotEpisodeCalc()
      plot(p)
      dev.off()
    }
  )

  output$epsEpisode <- downloadHandler(
    filename = function() {
      paste0("Episode", '.eps')
    },
    content = function(file) {
      postscript(file)
      p = plotEpisodeCalc()
      plot(p)
      dev.off()
    }
  )
})
