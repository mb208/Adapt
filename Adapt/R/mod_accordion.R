library(shiny)
library(shinyjs)
library(tidyverse)
source("R/utils_ui.R")



accordionUI <- function(id, div_id = "accordion") {
  ns <- NS(id)
  tagList(
    textInput(ns("textInput"), label = "Input Text"),
    actionButton(ns("btn"), label= "Update"),
    tags$div(id = div_id, "This is an accordion")
  )
}

accordionServer <- function(id, div_id = "accordion") {
  moduleServer(id,
               function(input, output, session) {
                 observeEvent(input$btn, {
                   insertUI(
                     selector = str_c("#", div_id, collapse = ""),
                     where = "beforeEnd",
                     immediate=TRUE,
                     ui = tagList(accordion_item(label = "accord 1", tags$p(input$textInput))
                                  ,
                                  tags$link(rel = 'stylesheet',
                                            type = 'text/css',
                                            href = 'www/accordion.css')
                                  )
                   )
                   runjs("$('.accordion').on('click', function(e) {
                              e.preventDefault();
                              $(this).next('.panel').toggle();
                              }) ;")
                   
                 })
                 
               })
}


# ui <- fluidPage(
#   useShinyjs(),
#   tags$head(
#     includeCSS("Adapt/www/accordion.css"),
#     includeScript("Adapt/www/accordion.js")
#   ),
#   mainPanel(accordionUI("accordion"))
#   
# )





server <- function(input, output, session) {
  accordionServer("accordion")
  
  
}

# shinyApp(ui, server)