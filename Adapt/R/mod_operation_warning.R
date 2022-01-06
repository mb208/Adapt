library(shiny)


operation_warning_UI <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("var_creation_warning")),
    tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;
      }
    "))
  )
}

operation_warning_Server <- function(id, data, unary_operation, exponent) {
  moduleServer(id,
               function(input, output, session){
                 
                 output$var_creation_warning <- renderText({
                   req(data())
                   if (unary_operation() == "ln" & any(data() <= 0)) {
                     validate("Selected data must be postive real number for this transformation")
                   } else if (unary_operation() == "^" & !is.integer(exponent()) & any(data() < 0)) {
                     validate("Taking root of negative number may produce NaNs")
                   } 
                 })
                 })
}

# ui <- fluidPage(
#   mainPanel(actionButton("browser", "browser"),
#             selectizeInput(
#               inputId = 'unary_operation',
#               label = "Apply operation to chosen variable",
#               c("exp", "ln", "^", "None"),
#               options = list(
#                 maxItems = 1,
#                 placeholder = "select operation",
#                 onInitialize = I('function() { this.setValue(0); }')
#               )
#             ),
#             numericInput("power_val", "Exponent Value:", value = 1),
#             operation_warning_UI("operation_warning")
# 
#   )
# 
# )
# 
# 
# server <- function(input, output, session) {
# 
#   data <- data.frame(matrix(rnorm(300),ncol=3))
#   operation_warning_Server("operation_warning",
#                             data = reactive(data),
#                             unary_operation = reactive(input$unary_operation),
#                             exponent  = reactive(input$power_val))
#   
#   observeEvent(input$browser,{
#     browser()
#   })
# 
# }
# 
# shinyApp(ui, server)