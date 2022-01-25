library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinythemes)
library(shinyjs)

source("R/utils_ui.R")
source("R/mod_warm_start.R")
source("R/mod_randomizationProb.R")




shinyUI(fluidPage(
    shinyjs::useShinyjs(),
    theme = shinytheme("sandstone"),
    tags$head(
        tags$style(HTML('

                        .modal-lg {
                        width: 1500px;
                        max-width: 1500px;

                        }
                        
                        .modal-body { 
                        min-height:700px;
                        }
                      ')),
        includeCSS("www/accordion.css"), 
        includeCSS("www/style.css"), 
        includeScript("www/accordion.js"), 
    ),
    # Application title
    titlePanel("Dynamic Treatment Regimes"),
    actionButton("browser", "browser"),
    tags$script("$('#browser').hide();"),
    tabsetPanel(type = "tabs",
                tabPanel("Main",
                         column(3,
                                tags$div(
                                  h3(strong("Data Source")),
                                  h4("Generate data source by uploading an an existing file from your computer or simulating a data set from scratch"),
                                  actionButton("upload_file", "Upload Data"),
                                  actionButton("data_simulation", "Simulate"),
                                  br(),
                                  shinyjs::hidden(fileInput("file_info", 
                                                            "Upload File",
                                                            accept = ".csv")),
                                  h4("To clear the data that was either uploaded or simulated click the following button"),
                                  actionButton("reset", "reset")
                                  )
                                ),
                         shinyjs::hidden(
                           tags$div(id = "setup_dynr",
                         column(3,                                 
                                set_html_breaks(3),
                                           selectInput(
                                             'sel_covariates',
                                             label = "Choose Covariates",
                                             list(),
                                             multiple = TRUE,
                                             selectize =TRUE
                                           ),
                                           selectInput(
                                             'sel_outcome',
                                             label = "Choose Outcome",
                                             list(),
                                             multiple = FALSE
                                           ), 
                                           selectInput(
                                             'sel_action',
                                             label = "Specify actions",
                                             list(),
                                             multiple = FALSE
                                           )    
                                ),
                          column(3,
                                 set_html_breaks(3),
                                 textInput("created_var_name", "Enter name for variable:"),
                                 feature_gen_ui("var_transform"),
                                 actionButton("create_var", "Generate")
                                 )
                           )
                                )
                         ),
                tabPanel("Randomization",
                         randomizationProb_UI("randomize")
                         ),
                tabPanel("Treatment Regime",
                         column(3,
                                tags$div(
                                  h3(strong("Upload Warm Start")),
                                  h4("Initialize treatement regime by uploading warm start configuration."),
                                  warmStartUI("warmstrt"),
                                  actionButton("init_warmstrt", "Apply Warm Start"),
                                  plotOutput("warmStartBar")
                                )
                         ),
                         column(4,
                                h3(strong("Simulate Treatment Regime")),
                                h4("Simulate treatment regime using decision lists."),
                                actionButton("sim_DLs", "Simulate"),
                                set_html_breaks(3),
                                actionButton("dply_dlplot", "Toggle display"),
                                tags$div(id = "dl_display",
                                         sliderInput("view_stages", 
                                                     min = 1, 
                                                     value = c(1,2),
                                                     max=42,
                                                     "Choose stages to view"),
                                         plotOutput("dl_plot")
                                         )
                                ),
                         column(3
                                
                                )
                         )
    )
    )
)



