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
