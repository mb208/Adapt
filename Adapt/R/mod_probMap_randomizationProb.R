library(shiny)
library(shinyjs)
library(tidyverse)

# library("R/utils_server.R")


prob_map_UI <- function(id) {
  ns <- NS(id)
  tagList(
    h4('Choose Probability Mapping'),
    selectizeInput(ns('prob_map'),
                   label = "",
                   list("expit", 
                        "tanh", 
                        "arctan"),
                   options = list(maxItems = 1,
                                  placeholder = "select probability generation",
                                  onInitialize = I('function() { this.setValue(0); }')) 
    )
  )
}


prob_map_Server <- function(id, X) {
  moduleServer(id,
               function(input, output, session) { 
                 ### Get Probabilities ###
                 probabilities <- reactive({
                   req(input$prob_map)
                   head(X())
                   
                  # Apply selected transformation to calculate probability
                   prob_maps[[input$prob_map]](X())
                   
                 })
                 
                 return(list(probs = probabilities,
                             prob_map = reactive(input$prob_map)))
               } 
  )
}

source("utils_server.R")

# ui <- fluidPage(
#   mainPanel(actionButton("browser", "browser"),
#             prob_map_UI("prob_map")
#   )
# )
# 
# 
# server <- function(input, output, session) {
# 
#   data <- data.frame(matrix(rnorm(300),ncol=1))
#   result <- prob_map_Server("prob_map",
#                             X = reactive(data))
#   observeEvent(input$browser,{
#     browser()
#   })
# 
# }
# 
# shinyApp(ui, server)