library(shiny)
library(shinyjs)
library(tidyverse)

source("R/utils_server.R")
source("R/mod_weighted_sum.R")


feature_gen_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns('select_vars'),
      label = "Choose Variables",
      choices = NULL,
      multiple = TRUE,
      selectize = TRUE
    ),
    selectizeInput(
      inputId = ns('multi_operation'),
      label = "Apply operation to chosen variables",
      list("weighted sum",
           "multiply",
           "divide"),
      options = list(
        maxItems = 1,
        placeholder = "select operation",
        onInitialize = I('function() { this.setValue(0); }')
      )
    ),
    # actionButton(ns("gen"), "Generate"),
    wgt_sum_ui(ns("weights")),
  )
}

feature_gen_server <- function(id, data, gen) {
  moduleServer(id,
               function(input, output, session) {
                 
                
                 observe({
                   data <- data()
                  
                 })

                 observe({
                   updateSelectizeInput(session = session,
                                        inputId = "select_vars",
                                        choices = names(data()))
                 })
                 
                 
                 # call weights from weighted sum UI
                 weights <- wgt_sum_server(
                     "weights",
                     var_names = reactive(input$select_vars),
                     multi_operation = reactive(input$multi_operation)
                   )

                 
                 myval <- reactive({
                  
                     req(data())
                     req(input$select_vars)
                     vars <- data()[, input$select_vars]
                     operation <- input$multi_operation
                     # Logic changes if they choose weighted sum
                     if (operation == "weighted sum") {
                       
                       as.matrix(vars) %*% as.matrix(weights())
                       
                     } else {
                       n_ary_operator(operation, vars)
                     }
                   
                 }) %>% 
                   bindEvent(gen())
                 
                 # return(list(
                 #   value = myval,
                 #   gen = reactive(input$gen)
                 # )) 
                 
                 return(myval)
                 
               })
}

# source("utils_server.R")
# source("mod_weighted_sum.R")
# 
# ui <- fluidPage(
#   mainPanel(actionButton("browser", "browser"),
#              actionButton("create_var", "Generate"),
#             feature_gen_ui("var_transform")
# 
#   )
# 
# )
# 
# 
# server <- function(input, output, session) {
# 
#   data <- data.frame(matrix(rnorm(300),ncol=3))
#   choices <- feature_gen_server("var_transform",
#                             data = reactive(data),
#                             gen = reactive(input$create_var))
# 
#   # observe(if (choices$gen()) print("hello"))
# 
# 
# 
#   observeEvent(input$browser,{
#     browser()
#   })
# 
# }
# 
# shinyApp(ui, server)