library(shiny)
library(shinyjs)
library(tidyverse)
library(DT)

source("R/utils_server.R")
source("R/utils_ui.R")
source("R/utils_latex_render.R")


error_dist_UI <- function(id) {
  ns <- NS(id)
  tagList(
    h4(strong("Specify the error distribution")),
    selectizeInput(
      inputId = ns('error_dist'),
      label = "Select Error Distribution",
      list("Gaussian",
           "Double Exponential"),
      options = list(
        maxItems = 1,
        placeholder = "select distribution",
        onInitialize = I('function() { this.setValue(0); }')
      )
    ),
    uiOutput(ns("cond_err_dist")),
    actionButton(ns("calc_error"), "Generate Error")
  )
  
}



error_dist_Server <- function(id, n_obs) {
  moduleServer(id, 
               function(input, output, session) {
                 ns <- session$ns
                 error_id <- reactiveVal()
                 
                 error_latex <- reactiveVal()
                 simulated_error <- reactiveVal()
                 
                 observe({
                   # Init parameters for error specification
                   n_obs() # Reset parameters each time n_obs changes
                   error_latex(NULL)
                   simulated_error(NULL)
                 })
                 
                 output$cond_err_dist <-  renderUI({
                   
                   req(input$error_dist)
                   error_dist <- tolower(input$error_dist)
                   error_id__ = str_replace(error_dist, " ", "_")
                   error_id(error_id__)
                   
                   return(err_dist_input(error_dist,
                                  ns(error_id__)
                                  ))
                   
                 })
                 
                 
                 ## Sample noise from selected distribution
                 observeEvent(input$calc_error, {   
                   sim_error <- switch (
                     tolower(input$error_dist),
                     "gaussian" = rnorm(n_obs(),
                                        0,
                                        sd = input[[error_id()]]),
                     "double exponential" = nimble::rdexp(n_obs(),
                                                          0,
                                                          input[[error_id()]])
                   )
                   
                   simulated_error(sim_error)
                   
                   # Generated error latex expression
                   error_tex <- switch(tolower(input$error_dist),
                                                  "gaussian"  = guass_tex(mean = 0, sd = input[[error_id()]]),
                                                  "double exponential" = laplace_tex(location = 0, scale = input[[error_id()]]))  %>% 
                     str_c("\\epsilon \\sim", ., collapse = "") %>% 
                     render_tex_inline()
                   
                   error_latex(error_tex)
                 })
                 
                 return(list(simulated_error = simulated_error,
                             error_latex = error_latex))
               })
}

# source("utils_server.R")
# source("utils_ui.R")
# source("utils_latex_render.R")


# ui <- fluidPage(
#   useShinyjs(),
#   withMathJax(),
#   mainPanel(actionButton("browser", "browser"),
#             error_dist_UI("sim_error")
#   )
# )
# 
# 
# server <- function(input, output, session) {
# 
#   data <-data.frame(matrix(rnorm(300),ncol=3))
# 
#   result <- error_dist_Server("sim_error", n_obs = reactive(dim(data)[1]))
# 
#   observeEvent(input$browser,{
#     browser()
#   })
# 
# }
# 
# shinyApp(ui, server)