library(shiny)
library(DT)

shinyUI(fluidPage(

  titlePanel("Shiny iglu"),
  tabsetPanel(
    tabPanel("Data", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(fileInput("datafile", "Choose a CSV File",
                                      multiple =  FALSE,
                                      accept = ".csv"),
                            textInput('id', 'Enter column name corresponding to subject ID', value = 'id'),
                            textInput('time', 'Enter column name corresponding to timestamp', value = 'time'),
                            textInput('gl', 'Enter column name corresponding to glucose values', value = 'gl')
               ),
               mainPanel(tableOutput("data"))
             ))
  )

))
