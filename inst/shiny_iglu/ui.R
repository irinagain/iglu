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
             )),
    tabPanel("Metrics", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput('metric', 'Choose Metric', choices = c(`Above Percent` = 'above_percent',
                                                                               `ADRR` = 'adrr',
                                                                               `Below Percent` = 'below_percent',
                                                                               `CONGA` = 'conga',
                                                                               `CV` = 'cv_glu',
                                                                               `GRADE` = 'grade',
                                                                               `GRADE Euglycemia` = 'grade_eugly',
                                                                               `GRADE Hyperglycemia` = 'grade_hyper',
                                                                               `GRADE Hypoglycemia` = 'grade_hypo',
                                                                               `High Blood Glucose Index` = 'hbgi',
                                                                               `Hyperglycaemia Index` = 'hyper_index',
                                                                               `Hypoglycaemia Index` = 'hypo_index',
                                                                               `Index of Glycemic Control` = 'igc',
                                                                               `In Range Percent` = 'in_range_percent',
                                                                               `Interquartile Range` = 'iqr_glu',
                                                                               `J Index` = 'j_index',
                                                                               `Low Blood Glucose Index` = 'lbgi',
                                                                               `MAGE` = 'mage',
                                                                               `Mean` = 'mean_glu',
                                                                               `Median` = 'median_glu',
                                                                               `MODD` = 'modd',
                                                                               `Quantiles` = 'quantile_glu',
                                                                               `Range` = 'range_glu',
                                                                               `Standard Deviation` = 'sd_glu',
                                                                               `Summary Statistics` = 'summary_glu'
               )),
               uiOutput("select_parameter"),
               uiOutput("help_text")),
               mainPanel(dataTableOutput("metric"))
               )),

    tabPanel("Plots", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 radioButtons("plottype",  "Plot Type",
                              choices = c(`Time Series` = 'tsplot',
                                          `Lasagna Plot (Multiple Subject)` = 'lasagnamulti',
                                          `Lasagna Plot (Single Subject)` = 'lasagnasingle'
               )),
               uiOutput("plot_lasagnatype"),
               uiOutput("plot_subjects"),
               uiOutput("plot_subjects_help_text"),
               #uiOutput("plot_maxd"),
               #uiOutput("plot_maxd_help_text"),
               uiOutput("plot_datatype"),
               uiOutput("plot_datatype_help_text"),
               #uiOutput("plot_tz"),
               #uiOutput("plot_tz_help_text"),
               uiOutput("plot_TR"),
               uiOutput("plot_TR_help_text")
               #uiOutput('plot_limits'),
               #uiOutput('plot_limits_help_text'),
               #uiOutput("plot_midpoint"),
               #uiOutput("plot_midpoint_help_text")
               ),
               mainPanel(plotOutput("plot"))
             ))


       )


))
