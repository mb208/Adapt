library(shiny)
library(shinyjs)
library(tidyverse)

# source("R/utils_server.R")
# source("R/mod_weighted_sum.R")

source("utils_server.R")
source("mod_weighted_sum.R")



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
    actionButton(ns("create_var"), "Generate"),
    wgt_sum_ui(ns("weights")),
  )
}

feature_gen_server <- function(id, data, ext_input) {
  moduleServer(id,
               function(input, output, session) {
                 
                 value <-  reactiveValues(value="")
                 updateSelectizeInput(session = session, 
                                      inputId = "select_vars",
                                      choices = names(data))
                 
                 wgt_sum_server("weights",  
                                var_names = reactive({input$select_vars}), 
                                multi_operation = reactive({input$multi_operation}))
                 
                 observeEvent(input$create_var, {
                   vars <- data[ , input$select_vars] 
                   operation <- input$multi_operation
                   value$gen <- input$create_var
                   # Logic changes if they choose weighted sum
                   if (operation == "weighted sum") {
                     
                     weights <- sapply(input$select_vars,
                                       function(x){
                                         ext_input[[paste('wgt_',x, sep="")]]}
                     )
                     value$value <- as.matrix(vars) %*% as.matrix(weights)
                    
                   } else {
                     value$value <-  n_ary_operator(operation, vars)
                   }
                                                         
                 }
                 )
                 
                 return(value) 
                 
               })
}


ui <- fluidPage(
  mainPanel(actionButton("browser", "browser"),

            feature_gen_ui("var_transform")

  )

)


server <- function(input, output, session) {

  data <- data.frame(matrix(rnorm(300),ncol=3))
  choices <- feature_gen_server("var_transform",
                            data =data,
                            ext_input = input)

  observeEvent(input$browser,{
    browser()
  })

}

shinyApp(ui, server)