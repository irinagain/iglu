library(shiny)
library(DT)

shinyServer(function(input, output){

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


parameter_type <- reactive({
  if(input$metric %in% c('adrr', 'conga', 'cv_glu', 'grade', 'hbgi', 'iqr_glu', 'j_index', 'lbgi', 'mean_glu', 'median_glu', 'range_glu', 'sd_glu', 'summary_glu')){
    return("none")
  }

  else if(input$metric %in% c('above_percent', 'below_percent', 'quantile_glu')){
    return("list")
  }

  else if(input$metric %in% c('grade_hyper', 'grade_hypo', 'hyper_index', 'hypo_index', 'mage', 'modd')){
    return("value")
  }

  else if(input$metric %in% c('grade_eugly', 'igc')){
    return("lwrupr")
  }

  else if(input$metric %in% c('in_range_percent')){
    return("nested")
  }
})

output$select_parameter <- renderUI({
  parameter_type = parameter_type()

  if(parameter_type == "list"){
    if(input$metric == 'above_percent'){
      textInput('parameter', 'Specify Parameter', value = '140, 180, 200, 250')
    }
    else if(input$metric == 'below_percent'){
      textInput('parameter', 'Specify Parameter', value = '50, 80')
    }
    else if(input$metric == 'quantile_glu'){
      textInput('parameter', 'Specify Parameter', value = '0, 25, 50, 75, 100')

    }
  }

  else if(parameter_type == "value"){
    if(input$metric == 'grade_hyper'){
      textInput('parameter', "Specify Parameter", value = '140')
    }

    else if(input$metric == 'grade_hypo'){
      textInput('parameter', 'Specify Parameter', value = '70')
    }

    else if(input$metric == 'hyper_index'){
      textInput('parameter', 'Specify Parameter', value = '140')
    }

    else if(input$metric == 'hypo_index'){
      textInput('parameter', 'Specify Parameter', value = '80')
    }

    else if(input$metric == 'mage'){
      textInput('parameter', 'Specify Parameter', value = '1')
    }

    else if(input$metric == 'modd'){
      textInput('parameter', 'Specify Parameter', value = '1')
    }
  }

  else if(parameter_type == "lwrupr"){

    if(input$metric == "grade_eugly"){
      textInput('parameter', 'Specify Parameter', value = '80, 140')
    }

    else if(input$metric == 'igc'){
      textInput('parameter', 'Specify Parameter', value = '80, 140')
    }
  }

  else if(parameter_type == 'nested'){
    if(input$metric == 'in_range_percent'){
      textInput('parameter', 'Specify Parameter', value = '(80, 200), (70, 180), (70,140)')
    }
  }

})

output$help_text <- renderUI({
  parameter_type = parameter_type()
  if(parameter_type == "none"){
    helpText("No parameters need to be specified.")
  }
  else if(parameter_type == "list"){
    helpText("Enter numeric target values separated by commas.")
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
  library(iglu)
  if(is.null(input$parameter) | parameter_type == 'none'){
    string = paste('iglu::', input$metric, '(data)', sep = '')
    eval(parse(text = string))
  }

  else if(parameter_type == "list"){
    string = paste('iglu::', input$metric, '(data, c(', input$parameter, '))', sep = '')
    eval(parse(text = string))
  }

  else if(parameter_type == "value"){
    string = paste('iglu::', input$metric, '(data, ', input$parameter, ')', sep = '')
    eval(parse(text = string))
  }

  else if(parameter_type == "lwrupr"){
    string = paste('iglu::', input$metric, '(data, ' , input$parameter, ')', sep = '')
    eval(parse(text = string))
  }
  else if(parameter_type == "nested"){
    strlist = strsplit(input$parameter, ')')[[1]]
    paramstr = rep.int(0, length(strlist))
    if(length(strlist) == 1){
      paramstr = paste('c', strlist[1], ')', sep = '')
    }

    else {
      for(i in 2:length(strlist)){
        strlist[i] = substring(strlist[i], 3)
      }
      for(s in 1:length(strlist)){
        paramstr[s] = paste('c', strlist[s], ')', sep = '')
      }
      paramstr = paste(paramstr, collapse = ', ')
    }
    string = paste('iglu::', input$metric, '(data, list(', paramstr, '))', sep= '')
    eval(parse(text = string))

  }

  output$metric <- renderDataTable(metric_table(), extensions = 'Buttons',
                                   options = list(dom = 'Btip',
                                                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))

  })

})
