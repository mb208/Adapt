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
                 
                
                 ns <- session$ns
                 
                 ### Get weights if weighted sum chosen
                 output$weighted_sum_inputs <- renderUI({
                   req(multi_operation() == "weighted sum")
                   tags$div(h4(strong("Assign weights:")),
                            purrr::map(
                              var_names(),
                              ~ numericInput(
                                inputId = ns(paste('wgt_', .x, sep = "")) ,
                                label = .x,
                                value = 1
                              )
                            ))
                 })
                 
                 
                 observe({
                   req(multi_operation())
                   if (length(var_names()) ==1) {
                     shinyjs::hide("weighted_sum_inputs")
                   }
                   else if (multi_operation() == "weighted sum") {
                     shinyjs::show("weighted_sum_inputs")
                   } else {
                     shinyjs::hide("weighted_sum_inputs")
                   }
                   
                 })
                 
                 # Store weights here
                 weights <- reactive({
                   sapply(var_names(),
                          function(x) {
                            input[[paste('wgt_', x, sep = "")]]
                          })
                 }) 
          
                 
                 return(weights)
                 
                 
               })
}

# ui <- fluidPage(
#   mainPanel(actionButton("browser", "browser"),
#             selectInput(
#               inputId = 'select_vars',
#               label = "Choose Variables",
#               choices = NULL,
#               multiple = TRUE,
#               selectize = TRUE
#             ),
#             selectizeInput(
#               inputId = 'multi_operation',
#               label = "Apply operation to chosen variables",
#               list("weighted sum",
#                    "multiply",
#                    "divide"),
#               options = list(
#                 maxItems = 1,
#                 placeholder = "select operation",
#                 onInitialize = I('function() { this.setValue(0); }')
#               )),
#             wgt_sum_ui("var_transform")
# 
#   )
# 
# )
# 
# 
# server <- function(input, output, session) {
# 
#   data <- data.frame(matrix(rnorm(300),ncol=3))
#   updateSelectizeInput(session = session, 
#                        inputId = "select_vars",
#                        choices = names(data))
#   
#   weights <- wgt_sum_server("var_transform",
#                             var_names = reactive(input$select_vars),
#                             multi_operation = reactive(input$multi_operation))
# 
#   
#   observeEvent(input$browser,{
#     browser()
#   })
# 
# }
# 
# shinyApp(ui, server)