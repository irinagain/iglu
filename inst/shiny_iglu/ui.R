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
               ),
               mainPanel(tableOutput("data"))
             ))
  )

))
