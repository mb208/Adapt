library(DT)
library(reactable)
library(shinydashboardPlus)

source("R/utils_ui.R")

#### Model Dialog UI ####
data_gen_modal <- modalDialog(
  fluidPage(
    shinyjs::useShinyjs(),
    fluidRow(
      column(
        3,
        numericInput("n_participants",
                     "Number of Participants",
                     value = 20)
      ),
      column(
        3,
        numericInput("decision_pts",
                     "Decision points per day",
                     value = 5)
      ),
      column(
        3,
        numericInput("n_days",
                     "Length of trial (in days)",
                     value = 40)
      ),
      column(1,
             offset = 2
             ,
             actionButton("browser2", "browser")
             )
    ),
    hr(),
    tabsetPanel(type = "tabs",
                tabPanel("Data Creation",
                         source(file.path("R", "ui_data_creation_tab.R"),  local = TRUE)$value),
                tabPanel(
                  "View Data",
                  # Button Download Simulated Data,
                  column(1),
                  column(10,
                    DT::dataTableOutput("sim_data"),
                    br(),
                    downloadButton("downloadSimData", "Download Data")
                  ),
                  column(1)
                ),
                tabPanel("Data List",
                         set_html_breaks(1),
                         tags$div(id = "variable_summary", 
                                  h2(strong("Variables"),
                                     tags$ul(id = "variable-list",
                                             style="list-style-type:disc;")
                                     )
                         
                        )
                )
                )
  )
    
  
  ,
  title = "Simulate Dataset",
  footer = actionButton("simulate_data", "Finish Simulation")
  ,
  size = "l" # made modal window large
  
  
)
