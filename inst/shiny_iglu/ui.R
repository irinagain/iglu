
#library(shiny)
#library(DT)

shinyUI(fluidPage(

  titlePanel("Shiny iglu"),

  tabsetPanel(
    tabPanel("Data", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput("demodata", label = "Select Dataset",
                                        choices = c('', `User Datafile` = 'user_data',
                                                    `Example Data` = 'example_data')),
                            conditionalPanel(
                              condition = "input.demodata == 'user_data'",
                              fileInput("datafile", "Choose a CSV File",
                                        multiple =  FALSE,
                                        accept = ".csv"),
                              selectInput("datatype",
                                          "Choose what format this .csv file is in",
                                          c("Processed" = "processed",
                                            "Dexcom" = "Dexcom",
                                            "FreeStyle Libre" = "FreeStyle Libre",
                                            "FreeStyle Libre Pro" = "Libre Pro",
                                            "ASC" = "ASC",
                                            "iPro" = "iPro"),
                                          selected = "Processed"),
                              textInput("subjid", "Enter subject id (for non processed formats, if id in data leave as default)", value = "default"),
                              textInput('id', 'Enter column name corresponding to subject ID', value = 'id'),
                              textInput('time', 'Enter column name corresponding to timestamp', value = 'time'),
                              textInput('gl', 'Enter column name corresponding to glucose values', value = 'gl'),
                              downloadButton("downloaddata", "Download Data"),
                              selectInput('tz', 'Select corresponding time zone', choices = c(OlsonNames()))
                            ),
               ),
               mainPanel(DT::dataTableOutput("data"))
             )),
    #full metric name and function name are added in alphabetical order
    tabPanel("Metrics", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput('metric', 'Choose Metric', choices = c(`Above Percent` = 'above_percent',
                                                                               `Active Percent` = 'active_percent',
                                                                               `ADRR` = 'adrr',
                                                                               `Area Under Curve` = 'auc',
                                                                               `Below Percent` = 'below_percent',
                                                                               `COGI` = 'cogi',
                                                                               `CONGA` = 'conga',
                                                                               `CV` = 'cv_glu',
                                                                               `CV Subtypes` = 'cv_measures',
                                                                               `eA1C` = 'ea1c',
                                                                               `Episode Calculation` = 'episode_calculation',
                                                                               `GMI` = 'gmi',
                                                                               `GRADE` = 'grade',
                                                                               `GRADE Euglycemia` = 'grade_eugly',
                                                                               `GRADE Hyperglycemia` = 'grade_hyper',
                                                                               `GRADE Hypoglycemia` = 'grade_hypo',
                                                                               `Glucose Variability Percentage` = 'gvp',
                                                                               `High Blood Glucose Index` = 'hbgi',
                                                                               `Hyperglycemia Index` = 'hyper_index',
                                                                               `Hypoglycemia Index` = 'hypo_index',
                                                                               `Index of Glycemic Control` = 'igc',
                                                                               `In Range Percent` = 'in_range_percent',
                                                                               `Interquartile Range` = 'iqr_glu',
                                                                               `J Index` = 'j_index',
                                                                               `Low Blood Glucose Index` = 'lbgi',
                                                                               `M-Value` = 'm_value',
                                                                               `MAD` = 'mad_glu',
                                                                               `MAG` = 'mag',
                                                                               `MAGE` = 'mage',
                                                                               `Mean` = 'mean_glu',
                                                                               `Median` = 'median_glu',
                                                                               `MODD` = 'modd',
                                                                               `Quantiles` = 'quantile_glu',
                                                                               `Range` = 'range_glu',
                                                                               `Rate of Change (ROC)` = 'roc',
                                                                               `Standard Deviation` = 'sd_glu',
                                                                               `Standard Deviation Subtypes` = 'sd_measures',
                                                                               `Standard Deviation ROC` = 'sd_roc',
                                                                               `Summary Statistics` = 'summary_glu',
                                                                               `All Metrics` = 'all_metrics'
               )),
               uiOutput("select_parameter"),
               uiOutput("help_text"),
               uiOutput("select_second_parameter"),
               uiOutput("second_parameter_helptext"),
               uiOutput("select_third_parameter"),
               uiOutput("third_parameter_helptext"),
               checkboxInput("filter_sleep_wake", "Calculate metric for sleeping/waking hours?", value = FALSE, width = NULL),
               conditionalPanel(
                 condition = "input.filter_sleep_wake",
                 numericInput("sleep_start", "Sleep start time", 0, min = 0, max = 24),
                 numericInput("sleep_end", "Sleep end time", 6, min = 0, max = 24),
                 uiOutput("sleep_wake_help"),
                 selectInput("sleep_or_wake", "Calculate for sleep, wake, or both?", choices = c("Sleep" = "sleep", "Wake" = "wake", "Both" = "both"), selected = "Sleep"))
               ),
               mainPanel(DT::dataTableOutput("metric")))
    ),

    tabPanel("Plots", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 radioButtons("plottype",  "Plot Type",
                              choices = c(`Time Series` = 'tsplot',
                                          `Lasagna Plot (Multiple Subject)` = 'lasagnamulti',
                                          `Lasagna Plot (Single Subject)` = 'lasagnasingle',
                                          `Rate of Change (Time Series)` = 'plot_roc',
                                          `Rate of Change (Histogram)` = 'hist_roc'
                              )),
                 uiOutput("plot_lasagnatype"),
                 uiOutput("plot_subjects"),
                 uiOutput("plot_subjects_help_text"),
                 uiOutput("plot_timelag"),
                 uiOutput("plot_maxd"),
                 uiOutput("plot_datatype"),
                 uiOutput("plot_datatype_help_text"),
                 #uiOutput("plot_tz"),
                 #uiOutput("plot_tz_help_text"),
                 uiOutput("plot_TR"),
                 #uiOutput("plot_TR_help_text"),
                 uiOutput("plot_midpoint"),
                 uiOutput('plot_limits'),
                 uiOutput('plot_colorbar_help_text'),
                 uiOutput('plot_color_scheme'),
                 uiOutput('plot_log'),
                 downloadButton(outputId = "pdfButton", label = "PDF"),
                 downloadButton(outputId = "pngButton", label = "PNG"),
                 downloadButton(outputId = "epsButton", label = "EPS")
               ),
               mainPanel(plotOutput("plot"))
             )),

    tabPanel("AGP", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 uiOutput("agp_subject"),
                 uiOutput("agp_subject_help_text"),
                 downloadButton(outputId = "pdfAGP", label = "pdf"),
                 downloadButton(outputId = "pngAGP", label = "png"),
                 downloadButton(outputId = "epsAGP", label = "eps")
               ),
               mainPanel(
                 fluidRow(
                   column(6, wellPanel("Glucose Statistics")),
                   column(6, wellPanel("Time in Ranges"))),
                 fluidRow(
                   column(6, DT::dataTableOutput("agp_metrics")),
                   column(6, plotOutput("plot_ranges"))
                 ),
                 fluidRow(
                   column(12, wellPanel("Ambulatory Glucose Profile (AGP)"))
                 ),
                 fluidRow(
                   column(12,  plotOutput("plot_agp"))
                 ),
                 fluidRow(
                   column(12, wellPanel("Daily Glucose Profiles"))
                 ),
                 fluidRow(
                   column(12, plotOutput("plot_daily"))
                 )
               )
             )),
    tabPanel("Episode Calculation", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 uiOutput("episode_subject"),
                 downloadButton(outputId = "pdfEpisode", label = "pdf"),
                 downloadButton(outputId = "pngEpisode", label = "png"),
                 downloadButton(outputId = "epsEpisode", label = "eps"),
                 numericInput(inputId = "lv1hyperThreshold", label = "\nEnter a value for HyperThreshold (level1)",
                              value = 120),
                 numericInput(inputId = "lv2hyperThreshold", label = "\nEnter a value for HyperThreshold (level2)",
                              value = 180),
                 numericInput(inputId = "lv1hypoThreshold", label = "\nEnter a value for HypoThreshold (level1)",
                              value = 100),
                 numericInput(inputId = "lv2hypoThreshold", label = "\nEnter a value for HypoThreshold (level2)",
                              value = 70),
                 radioButtons("colorScheme", "Color Scheme", c("Color Scheme 1", "Color Scheme 2", "Color Scheme 3"))
               ),
               mainPanel(
                 fluidRow(
                   column(12, wellPanel("Episode Calculation Profile (ECP)"))
                 ),

                 fluidRow(
                   column(12, plotOutput("plot_episode_calculation"))
                 )
               )
             ))
  )


))
