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

metric_table <- reactive({
  parameter_type = parameter_type()
  data = transform_data()
  library(iglu)
  if(is.null(input$parameter) | parameter_type == 'none'){
    string = paste('iglu::', input$metric, '(data)', sep = '')
    eval(parse(text = string))
  }


  })

