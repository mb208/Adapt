library(shiny)
library(shinyjs)
library(tidyverse)

source("R/utils_server.R")
source("R/utils_ui.R")
source("R/utils_latex_render.R")
source("R/mod_calc_mean.R")
source("R/mod_calc_sd.R")
source("R/mod_error_dist.R")
source("R/mod_operation_warning.R")


location_scale_UI <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(column(5,
                          tags$div(id = ns("cond_dist_title"),
                                   h3(
                                     strong("Constructing Conditional Distribution")
                                   )))),
          fluidRow(column(1, actionButton(ns("prev"),"Previous")), column(1, actionButton(ns("nxt"),"Next"))),
          tags$div(id = ns("mean"), calc_mean_UI(ns("calc_mean"))),
          shinyjs::hidden(tags$div(id = ns("variance"), calc_sd_UI(ns("calc_sd")))),
          shinyjs::hidden(tags$div(id = ns("error"), error_dist_UI(ns("sim_error")))),
          )
}

location_scale_Server <- function(id, df) {
  moduleServer(id,
               function(input, output, session) {
                 step_rv <- reactiveVal()
                 mean_params <- reactiveVal()
                 sd_params <- reactiveVal()
                 error_params <- reactiveVal()
                 
                 observe({
                   names(df()) # forcing dependency on new data
                   step_rv(1)
                   
                 })
                 
                 observe({
                   if (step_rv() == 1) {
                     shinyjs::disable("prev")
                     shinyjs::enable("nxt")
                     shinyjs::show("mean")
                     shinyjs::hide("variance")
                     shinyjs::hide("error")
                     
                     
                     
                   } else if (step_rv() == 3) {
                     shinyjs::disable("nxt")
                     shinyjs::hide("variance")
                     shinyjs::hide("mean")
                     shinyjs::show("error")
                     
                     
                   } else {
                     shinyjs::enable("prev")
                     shinyjs::enable("nxt")
                     
                     shinyjs::show("variance")
                     shinyjs::hide("mean")
                     shinyjs::hide("error")
                     
                     
                   }
                 })
                 
                 
                 mean_params <- calc_mean_Server("calc_mean", df)
                 
                 sd_params <- calc_sd_Server("calc_sd", df)
                 
                 error_params <- error_dist_Server("sim_error", n_obs = reactive(dim(df())[1]))
                 
                 observeEvent(input$prev, {
                   step_rv(step_rv()- 1)
                 })
                 
                 observeEvent(input$nxt, {
                   step_rv(step_rv() + 1)
                 })
                 
               
                 

                

                


                 return(list(mean_params=mean_params,
                             sd_params=sd_params,
                             error_params=error_params,
                             step_rv = step_rv))


                 
               })
}



# source("utils_server.R")
# source("utils_ui.R")
# source("utils_latex_render.R")
# source("mod_calc_mean.R")
# source("mod_calc_sd.R")
# source("mod_error_dist.R")
# source("mod_operation_warning.R")
# 
# ui <- fluidPage(
#   useShinyjs(),
#   mainPanel(actionButton("browser", "browser"),
#             location_scale_UI("location_Scale"),
#             #actionButton("gen", "Gen Variable")
#   )
# )
# 
# 
# server <- function(input, output, session) {
#   
#   data <-data.frame(matrix(rnorm(300),ncol=3))
#   
#   result <- location_scale_Server("location_Scale", 
#                                   df = reactive(data))
#   
#   observeEvent(input$browser,{
#     browser()
#   })
#   
# }
# 
# shinyApp(ui, server)
