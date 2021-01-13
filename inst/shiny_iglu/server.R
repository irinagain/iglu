#library(shiny)
#library(DT)

shinyServer(function(input, output) {

  ############################## DATA SECTION ############################

  data <- reactive({
    req(input$datafile)
    read.csv(input$datafile$datapath)
  })

  output$data <- renderTable({
    data()
  })

  transform_data <- reactive({
    data = data()
    iglu:::read_df_or_vec(data, id = input$id, time = input$time, gl = input$gl)
  })


  ############################# METRIC SECTION ######################################################


parameter_type <- reactive({

  if(input$metric %in% c("adrr", "cv_glu", "ea1c", "gmi", "cv_measures", "grade", "gvp", "hbgi", "iqr_glu", "j_index", "lbgi", "mad_glu",
                         "mean_glu", "median_glu", "range_glu", "sd_glu", "sd_measures", "summary_glu", "all_metrics")){
    return("none")
  }

    else if(input$metric %in% c("above_percent", "below_percent", "quantile_glu")){
      return("list")
    }

    else if(input$metric %in% c("conga", "grade_hyper", "grade_hypo", "hyper_index", "hypo_index", "m_value",
                                "mage", "modd", "roc", "sd_roc", "active_percent")){
      return("value")
    }

    else if(input$metric %in% c("grade_eugly", "igc")){
      return("lwrupr")
    }

    else if(input$metric %in% c("in_range_percent")){
      return("nested")
    }
  })

  output$select_parameter <- renderUI({
    parameter_type = parameter_type()

    if(parameter_type == "list"){
      if(input$metric == "above_percent"){
        textInput("parameter", "Specify Parameter", value = "140, 180, 250")
      }
      else if(input$metric == "below_percent"){
        textInput("parameter", "Specify Parameter", value = "50, 80")
      }
      else if(input$metric == "quantile_glu"){
        textInput("parameter", "Specify Parameter", value = "0, 25, 50, 75, 100")
      }
    }

  else if(parameter_type == "value"){
    if(input$metric == "conga"){
      textInput("parameter", "Specify n", value = "24")
    }

      else if(input$metric == "grade_hyper"){
        textInput("parameter", "Specify Parameter", value = "140")
      }

      else if(input$metric == "grade_hypo"){
        textInput("parameter", "Specify Parameter", value = "80")
      }

      else if(input$metric == "hyper_index"){
        textInput("parameter", "Specify Parameter", value = "140")
      }

      else if(input$metric == "hypo_index"){
        textInput("parameter", "Specify Parameter", value = "80")
      }

      else if(input$metric == "m_value"){
        textInput("parameter", "Specify Reference Value", value = "90")
      }

      else if(input$metric == "mage"){
        textInput("parameter", "Specify Parameter", value = "1")
      }

      else if(input$metric == "modd"){
        textInput("parameter", "Specify Parameter", value = "1")
      }

      else if(input$metric == "active_percent"){
        textInput("parameter", "Specify Parameter", value = "5")
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

      else if(input$metric == "igc"){
        textInput("parameter", "Specify Parameter", value = "80, 140")
      }
    }

   else if(parameter_type == "nested"){
    if(input$metric == "in_range_percent"){
      textInput("parameter", "Specify Parameter", value = "(80, 200), (70, 180), (70,140)")
    }
    }

  })

  output$help_text <- renderUI({
    parameter_type = parameter_type()
    if(parameter_type == "none"){
      helpText("No parameters need to be specified.")
    }
    else if(parameter_type == "list"){
      helpText("Enter numeric target values separated by comma.")
    }
    else if(parameter_type == "value"){
      helpText("Enter numeric value corresponding to parameter.")
    }
    else if(parameter_type == "lwrupr"){
      helpText("Enter numeric values corresponding to the lower and upper bounds, respectively, separated by commas.")
    }
    else if(parameter_type == "nested"){
      helpText("Enter pairs of numeric values in parentheses, with commas separating values in each pair and the pairs themselves.")
    }
  })

  metric_table <- reactive({
    parameter_type = parameter_type()
    data = transform_data()

    if (is.null(input$parameter)) {
      validate(
        need(!is.null(input$parameter), "Please wait - Rendering")
      )
    } else if (grepl(',', input$parameter) & !grepl("\\(", input$parameter)) {
      if (length(strsplit(input$parameter, split = ",")[[1]]) != 2) {
        validate (
          need(parameter_type %in% c("list", "none"), "Please wait - Rendering")
        )
      } else {
        validate(
          need(parameter_type %in% c("list", "lwrupr", "none"), "Please wait - Rendering")
        )
      }
    } else if (grepl("\\(", input$parameter)) {
      validate(
        need(parameter_type %in% c("nested", "none"), "Please wait - Rendering")
      )
    } else if (!grepl(',', input$parameter)) {
      validate(
        need(parameter_type %in% c("value", "none"), "Please wait - Rendering")
      )
    }

    library(iglu)
    if(is.null(input$parameter) | parameter_type == "none"){
      string = paste("iglu::", input$metric, "(data)", sep = "")
      eval(parse(text = string))
    }

    else if(parameter_type == "list"){
      string = paste("iglu::", input$metric, "(data, c(", input$parameter, "))", sep = "")
      eval(parse(text = string))
    }

    else if(parameter_type == "value"){
      string = paste("iglu::", input$metric, "(data, ", input$parameter, ")", sep = "")
      eval(parse(text = string))
    }

    else if(parameter_type == "lwrupr"){
      string = paste("iglu::", input$metric, "(data, " , input$parameter, ")", sep = "")
      eval(parse(text = string))
    }
    else if(parameter_type == "nested"){
      strlist = strsplit(input$parameter, ")")[[1]]
      paramstr = rep.int(0, length(strlist))
      if(length(strlist) == 1){
        paramstr = paste("c", strlist[1], ")", sep = "")
      }

      else {
        for(i in 2:length(strlist)){
          strlist[i] = substring(strlist[i], 3)
        }
        for(s in 1:length(strlist)){
          paramstr[s] = paste("c", strlist[s], ")", sep = "")
        }
        paramstr = paste(paramstr, collapse = ", ")
      }
      string = paste("iglu::", input$metric, "(data, list(", paramstr, "))", sep= "")
      eval(parse(text = string))

    }
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
  })

  ### Get Lasagna Type (lasagnatype)

  output$plot_lasagnatype <- renderUI({
    plottype = plottype()
    if(plottype == "tsplot"){
      NULL # lasagnatype doesn"t matter for tsplot, so no input UI is necessary
    }
    else if(plottype == "lasagnamulti"){
      radioButtons("plot_lasagnatype", "Lasagna Plot Type",
                   choices = c(`Unsorted` = "unsorted",
                               `Subject-sorted` = "subjectsorted",
                               `Time-sorted` = "timesorted"
                   ))
    }
    else if(plottype == "lasagnasingle"){
      radioButtons("plot_lasagnatype", "Lasagna Plot Type",
                   choices = c(`Unsorted` = "unsorted",
                               `Time-sorted` = "timesorted"))
    }
    else if(plottype == "plot_roc"){
      NULL
    }
    else if(plottype == "hist_roc"){
      NULL
    }
  })

  ### Get desired subjects
  output$plot_subjects <- renderUI({
    data = transform_data() # bring reactive data input into this renderUI call to default to all subjects
    plottype = plottype() # bring reactive input variable into this renderUI call
    if(plottype == "tsplot"){
      NULL
    }
    else if(plottype == "lasagnamulti"){
      NULL
    }
    else if(plottype == "lasagnasingle"){
      subject = unique(data$id)[1]
      textInput("plot_subjects", "Enter Subject ID", value = subject)
    }
    else if(plottype == "lasagnasingle"){
      subject = unique(data$id)[1]
      textInput("plot_subjects", "Enter Subject ID", value = subject)
    }
    else if(plottype == "plot_roc"){
      subject = unique(data$id)[1]
      textInput("plot_subjects", "Enter Subject ID", value = subject)
    }
    else if(plottype == "hist_roc"){
      subject = unique(data$id)[1]
      textInput("plot_subjects", "Enter Subject ID", value = subject)
    }
  })

  output$plot_subjects_help_text <- renderUI({
    data = transform_data()
    plottype = plottype()
    if(plottype == "tsplot"){
      NULL
    }
    else if(plottype == "lasagnamulti"){
      NULL
    }
    else if(plottype == "lasagnasingle"){
      helpText("Enter the ID of a subject to display their individualized lasagna plot")
    }
    else if(plottype == "plot_roc"){
      helpText("Enter the ID of a subject to display their individualized ROC time series plot")
    }
    else if(plottype == "hist_roc"){
      helpText("Enter the ID of a subject to display their individualized SD of ROC histogram")
    }

  })


  subset_data <- reactive({ # define reactive function to subset data for plotting each time user changes subjects list

    data = transform_data()
    data = data[data$id == input$plot_subjects,] # reactively subset data when subjects input is modified
    return(data)
  })


### Get time lag for Rate of Change plots
  output$plot_timelag <- renderUI({
    plottype = plottype() # bring reactive input variable into this renderUI call
    if(plottype == "tsplot"){
      NULL # time lag is only for ROC plots
    }
    else if(plottype == "lasagnamulti"){
      NULL
    }
    else if(plottype == "lasagnasingle"){
      NULL
    }
    else if(plottype == "plot_roc"){
      textInput("plot_timelag", "Enter Timelag for ROC calculation", value = 15)
    }
    else if(plottype == "hist_roc"){
      textInput("plot_timelag", "Enter Timelag for ROC calculation", value = 15)
    }
  })

### Get max days to plot (maxd)
  output$plot_maxd <- renderUI({
    plottype = plottype() # bring reactive input variable into this renderUI call
    if(plottype == "tsplot"){
      NULL
    }
    else if(plottype == "lasagnamulti"){
      textInput("plot_maxd", "Enter Maximum # of Days to Plot", value = 14)
    }
    else if(plottype == "lasagnasingle"){
      NULL
    }
    else if(plottype == "plot_roc"){
      NULL
    }
    else if(plottype == "hist_roc"){
      NULL
    }
  })

  ### Get datatype

  output$plot_datatype <- renderUI({  # Request input parameters depending on type of plot
    plottype = plottype() # bring reactive input variable into this renderUI call
    if(plottype == "tsplot"){
      NULL # datatype doesn"t matter for tsplot, so no input is necessary
    }
    else if(plottype == "lasagnamulti"){
      radioButtons("plot_datatype", "Data Aggregation Type",
                   choices = c(`Average across days` = "average",
                               `All data points` = "all"
                   ))
    }
    else if(plottype == "lasagnasingle"){
      NULL  # datatype doesn"t matter for single subject lasagna plots, so no input is necessary
    }
    else if(plottype == "plot_roc"){
      NULL
    }
    else if(plottype == "hist_roc"){
      NULL
    }
  })

  output$plot_datatype_help_text <- renderUI({  # Request input parameters depending on type of plot
    plottype = plottype() # bring reactive input variable into this renderUI call
    if(plottype == "tsplot"){
      NULL # datatype doesn"t matter for tsplot, so no input is necessary
    }
    else if(plottype == "lasagnamulti"){
      helpText("Select whether to use all data points in the first maxd days, or whether
               to take the average value at each time point across the first maxd days")
    }
    else if(plottype == "lasagnasingle"){
      NULL # datatype doesn"t matter for single lasagna plot, so no input is necessary
    }
    else if(plottype == "plot_roc"){
      NULL
    }
    else if(plottype == "hist_roc"){
      NULL
    }
  })

  ### Get time zone (tz)

  # output$plot_tz <- renderUI({ # Optionally accept new input for timezone
  #   plottype = plottype() # bring reactive input variable into this renderUI call
  #
  #   textInput("plot_tz", "Specify Timezone", value = "")
  # })
  #
  # output$plot_tz_help_text <- renderUI({ # Display help text related to timezone
  #   plottype = plottype() # bring reactive input variable into this renderUI call
  #     helpText("Enter time zone specification as characters, if one is required. Default (blank) is system current
  #              time zone, and "GMT" is UTC.")
  # })


  ### Get Target Range Limits (LLTR and ULTR)


  output$plot_TR <- renderUI({  # Request input parameters depending on type of plot
    plottype = plottype() # bring reactive input variable into this renderUI call
    if(plottype %in% c("tsplot", "lasagnamulti", "lasagnasingle")){
      textInput("plot_TR", "Specify Lower and Upper Target Values, separated by a Comma", value = "70, 180")
    }
    else if(plottype %in% c("plot_roc", "hist_roc")){
      NULL # ROC plots don't need TR
    }

  })

  # output$plot_TR_help_text <- renderUI({ # Display help text related to target range parameters
  #   plottype = plottype() # bring reactive input variable into this renderUI call
  #     helpText("Enter numeric values corresponding to the Lower and Upper Limits of the Target Range,
  #            respectively, separated by a comma.")
  # })


  ### Get midpoint

  output$plot_midpoint <- renderUI({
    plottype = plottype() # bring reactive input variable into this renderUI call
    if(plottype == "tsplot"){
      NULL
    }
    else if(plottype == "lasagnamulti"){
      textInput("plot_midpoint", "Enter Midpoint Glucose Value for Color Scale", value = 105)
    }
    else if(plottype == "lasagnasingle"){
      textInput("plot_midpoint", "Enter Midpoint Glucose Value for Color Scale", value = 105)
    }
    else if(plottype == "plot_roc"){
      NULL
    }
    else if(plottype == "hist_roc"){
      NULL
    }
  })

  ### Get color bar limits (limits)

  output$plot_limits <- renderUI({
    plottype = plottype() # bring reactive input variable into this renderUI call
    if(plottype == "tsplot"){
      NULL
    }
    else if(plottype == "lasagnamulti"){
      textInput("plot_limits", "Enter Limit Glucose Values for Color Scale Separated by a Comma", value = "50, 500")
    }
    else if(plottype == "lasagnasingle"){
      textInput("plot_limits", "Enter Limit Glucose Values for Color Scale Separated by a Comma", value = "50, 500")
    }
    else if(plottype == "plot_roc"){
      NULL
    }
    else if(plottype == "hist_roc"){
      NULL
    }
  })

  ### Color Bar help text

  output$plot_colorbar_help_text <- renderUI({ # render help text below color bar options
    plottype = plottype() # bring reactive input variable into this renderUI call
    if(plottype == "tsplot"){ # tsplot doesn"t make use of a colorbar, no helptext necessary
      NULL
    }
    else if(plottype == "lasagnamulti"){
      helpText("The color bar can be modified by changing the values of the target range, the midpoint,
               and the color bar limits")
    }
    else if(plottype == "lasagnasingle"){
      helpText("The color bar can be modified by changing the values of the target range, the midpoint,
               and the color bar limits")
    }
    else if(plottype == "plot_roc"){
      NULL
    }
    else if(plottype == "hist_roc"){
      NULL
    }
  })

  ### Get color scheme
  output$plot_color_scheme <- renderUI({
    plottype = plottype()
    if(plottype == "tsplot"){
      NULL
    }
    else if(plottype == "lasagnamulti"){
      radioButtons('plot_color_scheme', 'Transformation type',
                   choices = c(`Blue/Red` = '"blue-red"', `Red/Orange` = '"red-orange"'))
    }
    else if(plottype == "lasagnasingle"){
      radioButtons('plot_color_scheme', 'Transformation type',
                   choices = c(`Blue/Red` = '"blue-red"', `Red/Orange` = '"red-orange"'))
    }
    else if(plottype == "plot_roc"){
      NULL
    }
    else if(plottype == "hist_roc"){
      NULL
    }
  })

  ### Get log boolean

  output$plot_log <- renderUI({
    plottype = plottype()
    if(plottype == "tsplot"){
      radioButtons('plot_log', 'Transformation type',
                   choices = c(`None` = 'FALSE', `Log10` = 'TRUE'))
    }
    else if(plottype == "lasagnamulti"){
      radioButtons('plot_log', 'Transformation type',
                   choices = c(`None` = 'FALSE', `Log10` = 'TRUE'))
    }
    else if(plottype == "lasagnasingle"){
      radioButtons('plot_log', 'Transformation type',
                   choices = c(`None` = 'FALSE', `Log10` = 'TRUE'))
    }
    else if(plottype == "plot_roc"){
      NULL
    }
    else if(plottype == "hist_roc"){
      NULL
    }
  })

  ### Render Plot

  plotFunc <- reactive({

    plottype = plottype() # bring reactive input variable into this renderPlot call
    library(iglu)

  if(plottype == "tsplot"){
    #plot_glu(data, plottype = "tsplot")

    validate (
      need(all(!is.null(input$plot_log)),
           "Please wait - Rendering")
    )


    data = transform_data()
    string = paste('iglu::plot_glu(data = data, plottype = "tsplot", datatype = "all", lasagnatype = NULL, ',
                   input$plot_TR, ', subjects = NULL, tz = "", "blue-orange", log = ', input$plot_log, ')' ,sep = "")
    eval(parse(text = string))
  }
  else if(plottype == "lasagnamulti"){

    validate (
      need(all(!is.null(input$plot_lasagnatype) & !is.null(input$plot_datatype)),
           "Please wait - Rendering")
    )

    data = transform_data()
    string = paste('iglu::plot_lasagna(data = data, datatype = "', input$plot_datatype, '", lasagnatype = "',
                   input$plot_lasagnatype, '", maxd = ', input$plot_maxd, ', limits = c(', input$plot_limits, ') ,',
                   input$plot_midpoint, ', ', input$plot_TR, ', dt0 = NULL, inter_gap = 60, tz ="",',
                   input$plot_color_scheme, ', ', input$plot_log, ')', sep = "")
    eval(parse(text = string))
  }
  else if(plottype == "lasagnasingle"){

    validate (
      need(!is.null(input$plot_subjects), "Please wait - Rendering")
    )

    data = subset_data() # subset data to only user-specified subject
    string = paste('iglu::plot_lasagna_1subject(data = data, lasagnatype = "',
                   input$plot_lasagnatype, '", limits = c(', input$plot_limits, ') ,',
                   input$plot_midpoint, ', ', input$plot_TR, ', dt0 = NULL, inter_gap = 60, tz = "",',
                   input$plot_color_scheme, ', ', input$plot_log, ')', sep = "")
    eval(parse(text = string))
  }
  else if(plottype == "plot_roc"){
    data = subset_data() # subset data to only user-specified subject
    string = paste('iglu::plot_roc(data = data',
                   ', timelag = ', input$plot_timelag, ', tz = "")', sep = "")
    eval(parse(text = string))
  }
  else if(plottype == "hist_roc"){
    data = subset_data() # subset data to only user-specified subject
    string = paste('iglu::hist_roc(data = data',
                   ', timelag = ', input$plot_timelag, ', tz = "")', sep = "")
    eval(parse(text = string))
  }

  })

  output$plot <- renderPlot({
    plotFunc()
  })

  options(shiny.usecairo = T)

  output$pdfButton <- downloadHandler(
    filename = function() {
      plottype = plottype()
      paste(plottype, '.pdf', sep = '')
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
      paste(plottype, '.png', sep = '')
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
      paste(plottype, '.eps', sep = '')
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
    string = paste('iglu::plot_agp(data = data)')
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
      paste("AGP", '.pdf', sep = '')
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
      paste("AGP", '.png', sep = '')
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
      paste("AGP", '.eps', sep = '')
    },
    content = function(file) {
      postscript(file)
      p = gridExtra::grid.arrange(gridExtra::arrangeGrob(gridExtra::tableGrob(agpMetrics(), rows = NULL),
                                                         plotRanges(), ncol = 2), plotAGP(), plotDaily())
      plot(p)
      dev.off()
    }
  )

})


