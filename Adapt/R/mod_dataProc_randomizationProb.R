library(shiny)
library(shinyjs)
library(tidyverse)

source("R/utils_server.R")
source("R/mod_weighted_sum.R")

dataProc_UI <- function(id) {
  ns <- NS(id)
  tagList(
    h4('Choose Data Transformation'),
    selectizeInput(ns('data_agg'), 
                   label = "",
                   list("sum",
                        "weighted average"),
                   options = list(maxItems = 1,
                                  placeholder = "select data transformation",
                                  onInitialize = I('function() { this.setValue(0); }')) 
                   ),
    wgt_sum_ui(ns("weights")),
    )
}



dataProc_Server <- function(id, X) {
  moduleServer(id,
               function(input, output, session) {
                 
                 # Need to fix this in original weighted_sum module
                 # operation <- reactive(if (str_detect(input$data_agg, "weighted")) {"weighted sum"} else { NULL})
                 operation <- reactive(input$data_agg)
                 var_names <- reactive(names(X()))
                 
                 # call weights from weighted sum UI
                 weights <- wgt_sum_server(
                   "weights",
                   var_names = var_names,
                   multi_operation = reactive(input$data_agg)
                 )
                 
                 #### Data Aggregation ####
                 aggregations <- reactive({
                   # if weighted average chosen
                   if (input$data_agg == "weighted average") {
                     apply(X(),
                           MARGIN = 1,
                           function(x) {
                             data_agg[[input$data_agg]](x, weights())
                           }
                     )
                   }
                   
                   # Aggregate data according to selected transformation
                   apply(X(),
                         MARGIN = 1,
                         data_agg[[input$data_agg]])
                   
                 })
                 
                 
                 return(list(data = aggregations,
                             agg_name = reactive(input$data_agg)))
               }
  )
}



# source("utils_server.R")
# source("mod_weighted_sum.R")
# 
# ui <- fluidPage(
#   mainPanel(actionButton("browser", "browser"),
#             dataProc_UI("data_proc")
# 
#   )
# 
# )
# 
# 
# server <- function(input, output, session) {
# 
#   data <- data.frame(matrix(rnorm(300),ncol=3))
#   result <- dataProc_Server("data_proc",
#                             X = reactive(data))
#   observeEvent(input$browser,{
#     browser()
#   })
# 
# }
# 
# shinyApp(ui, server)