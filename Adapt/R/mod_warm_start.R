library(shiny)
library(shinyjs)
library(tidyverse)
library(yaml)
# source("R/utils_ui.R")



warmStartUI <- function(id, accept=NULL) {
  ns <- NS(id)
  tagList(
    fileInput(ns("warm_start"), 
              "Upload Warm Start",
              accept = accept)
  )
}

warmStartServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 
                     warm_start  <- reactive({
                     
                     ext <- tools::file_ext(input$warm_start$datapath)
                     validate(need(ext == "yml", "Please upload a yaml file"))
                    
                     read_yaml(input$warm_start$datapath)
                 })
                 
                     return(warm_start)
                 
               })
}


# ui <- fluidPage(
#   mainPanel(actionButton("browser", "browser"),
#             warmStartUI("warmstrt"))
# 
# )
# 
# 
# 
# 
# 
# server <- function(input, output, session) {
#   
#   data <- warmStartServer("warmstrt")
#   
#   
# }
# 
# shinyApp(ui, server)