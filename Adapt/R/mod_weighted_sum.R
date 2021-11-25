library(shiny)
library(shinyjs)
library(tidyverse)



wgt_sum_ui <- function(id, accept=NULL) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("weighted_sum_inputs"))
  )
}

wgt_sum_server <- function(id, var_names, multi_operation) {
  moduleServer(id,
               function(input, output, session) {
                 ### Get weights if weighted sum chosen
                 output$weighted_sum_inputs <- renderUI({
                   req(multi_operation() == "weighted sum")
                   tags$div(h4(strong("Assign weights:")),
                            purrr::map(
                              var_names(),
                              ~ numericInput(
                                inputId = paste('wgt_', .x, sep = "") ,
                                label = .x,
                                value = 1
                              )
                            ))
                 })
                 
                 
                 observe({
                   req(multi_operation())
                   if (multi_operation() == "weighted sum") {
                     shinyjs::show("weighted_sum_inputs")
                   } else {
                     shinyjs::hide("weighted_sum_inputs")
                   }
                   
                 })
                 
                 
               })
}
