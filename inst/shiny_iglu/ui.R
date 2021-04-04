#library(shiny)
#library(DT)

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
                                                                               `Active Percent` = 'active_percent',
                                                                               `ADRR` = 'adrr',
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
               uiOutput("third_parameter_helptext")),
               mainPanel(DT::dataTableOutput("metric"))
             )),

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
                 downloadButton(outputId = "epsEpisode", label = "eps")
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

