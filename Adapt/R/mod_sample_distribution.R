library(shiny)
library(shinyjs)
library(tidyverse)
library(DT)

# source("R/utils_server.R")
# source("R/utils_ui.R")
# source("R/utils_latex_render.R")

distribution_sample_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(
      ns("constant_time"),
      label = "At what interval does the variable change?",
      choices = c("Each decision point" = "dec_pt", 
                  "Daily" = "daily",
                  "Constant over entire trial" = "trial")
  ),
  selectizeInput(
      inputId = ns('data_dist'),
      label = "Select Distribution",
      list("Gaussian",
           "Binomial",
           "Gamma",
           "Uniform"),
      options = list(
        maxItems = 1,
        placeholder = "select distribution",
        onInitialize = I('function() { this.setValue(0); }')
      )
  ),
  uiOutput(ns("dist_params")))
}

distribution_sample_Server <- function(id, tex_name, n_participants, n_days, decision_pts, gen, varnames = NULL) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 dist_id <- reactiveVal()
                 sampled_var <- reactiveVal()
                 name_latex <- reactiveVal()
                 
                 output$dist_params <-  renderUI({
                   
                   req(tex_name())
                   req(input$data_dist)
                   dist <- tolower(input$data_dist)
                   dist_id__ = str_replace(dist, " ", "_")
                   dist_id(dist_id__)
                   # get_dist_input(dist, ns(dist_id__)) #! Removing function to be more transparent in module
                   return(switch(
                     dist,
                     "gaussian" = tags$div(
                       h4(strong("Gaussian Parameterization")),
                       numericInput(paste0(ns(dist_id__), "-mu"), "Mean", value = 0),
                       numericInput(paste0(ns(dist_id__), "-sd"), "SD", value = 1)
                     ),
                     "binomial" = tags$div(
                       h4(strong("Binomial Parameterization")),
                       numericInput(paste0(ns(dist_id__), "-n"), "Size", value = 5),
                       numericInput(paste0(ns(dist_id__), "-p"), "Probability", value = .5)
                     ),
                     "gamma"   =  tags$div(
                       h4(strong("Gamma Parameterization")),
                       numericInput(paste0(ns(dist_id__), "-s"), "Shape", value = 1),
                       numericInput(paste0(ns(dist_id__), "-r"), "Rate", value =  1)
                     )
                     ,
                     "uniform"   =  tags$div(
                       h4(strong("Uniform Parameterization")),
                       numericInput(paste0(ns(dist_id__), "-min"), "Min", value = 0),
                       numericInput(paste0(ns(dist_id__), "-max"), "Max", value =  1)
                     )
                   ))
                   
                 })
                 
                 observe({
                   req(input$data_dist)
                   req(dist_id())
                   name_latex__ <- switch(
                     input$data_dist,
                     "Gaussian" = guass_tex(mean = input[[paste0(dist_id(),"-mu")]],
                                            sd   = input[[paste0(dist_id(),"-sd")]]),
                     "Binomial" = binomial_tex(size = input[[paste0(dist_id(), "-n")]],
                                                p   = input[[paste0(dist_id(), "-p")]]),
                     "Gamma"    = gamma_tex(shape = input[[paste0(dist_id(), "-s")]],
                                            rate  = input[[paste0(dist_id(), "-r")]]),
                     "Uniform"  = unif_tex(Min = input[[paste0(dist_id(),"-min")]],
                                          Max  = input[[paste0(dist_id(),"-max")]])
                   ) %>%
                     c(tex_var_name(tex_name()), " \\sim ", .) %>%
                     str_c(collapse = "") %>%
                     render_tex_inline()
                     name_latex(name_latex__)

                 })  %>%
                   bindEvent(gen())
                 

                 
                 observe({
                   req(input$data_dist)
                   req(dist_id())
                   n_participants <-  n_participants()
                   n_days <-  n_days()
                   decision_pts <- decision_pts()
                   total <- n_days*decision_pts

                   if (input$constant_time=="dec_pt") {
                   sampled_var__ <- switch(
                       input$data_dist,
                       "Gaussian"  = rnorm(total*n_participants,
                                           mean = input[[paste0(dist_id(),"-mu")]],
                                           sd   = input[[paste0(dist_id(),"-sd")]]),
                       "Binomial"  = rbinom(total*n_participants,
                                            size = input[[paste0(dist_id(), "-n")]],
                                            p    = input[[paste0(dist_id(), "-p")]]),
                       "Gamma"     = rgamma(total*n_participants,
                                            shape = input[[paste0(dist_id(), "-s")]],
                                            rate = input[[paste0(dist_id(), "-r")]]),
                       "Uniform"     = runif(total*n_participants,
                                             min = input[[paste0(dist_id(),"-min")]],
                                             max = input[[paste0(dist_id(),"-max")]])
                     )
                   sampled_var(sampled_var__)

                   } else if (input$constant_time=="daily") {
                     sampled_var__ <- switch (
                       input$data_dist,
                       "Gaussian"  = rep(rnorm(n_participants*decision_pts,
                                               mean = input[[paste0(dist_id(),"-mu")]],
                                               sd   = input[[paste0(dist_id(),"-sd")]]),
                                         n_days),
                       "Binomial"  = rep(rbinom(n_participants*decision_pts,
                                                size = input[[paste0(dist_id(), "-n")]],
                                                p    = input[[paste0(dist_id(), "-p")]]),
                                         n_days),
                       "Gamma"     = rep(rgamma(n_participants*decision_pts,
                                                shape = input[[paste0(dist_id(), "-s")]],
                                                rate  = input[[paste0(dist_id(), "-r")]]),
                                         n_days),
                       "Uniform"     = rep(runif(n_participants*decision_pts,
                                                 min = input[[paste0(dist_id(),"-min")]],
                                                 max = input[[paste0(dist_id(),"-max")]]),
                                         n_days)
                     )
                     sampled_var(sampled_var__)
                   } else if (input$constant_time=="trial") {
                     sampled_var__ <- switch (
                       input$data_dist,
                       "Gaussian"  = rep(rnorm(n_participants,
                                               mean = input[[paste0(dist_id(),"-mu")]],
                                               sd   = input[[paste0(dist_id(),"-sd")]]),
                                         total),
                       "Binomial"  = rep(rbinom(n_participants,
                                                size = input[[paste0(dist_id(), "-n")]],
                                                p    = input[[paste0(dist_id(), "-p")]]),
                                         total),
                       "Gamma"     = rep(rgamma(n_participants,
                                                shape = input[[paste0(dist_id(), "-s")]],
                                                rate  = input[[paste0(dist_id(), "-r")]]),
                                         total),
                       "Uniform"     = rep(runif(n_participants,
                                                 min = input[[paste0(dist_id(),"-min")]],
                                                 max = input[[paste0(dist_id(),"-max")]]),
                                         total)
                     )
                     sampled_var(sampled_var__)
                   }

                 })  %>%
                 bindEvent(gen())
                 
                 return(list(sampled_var = sampled_var,
                             name_latex = name_latex,
                             dist_id = dist_id))
               })
  
}

source("utils_server.R")
source("utils_ui.R")
source("utils_latex_render.R")
source("mod_weighted_sum.R")

ui <- fluidPage(
  useShinyjs(),
  fluidRow(
    column(
      3,
      numericInput("n_participants",
                   "Number of Participants",
                   value = 20)
    ),
    column(
      3,
      numericInput("decision_pts",
                   "Decision points per day",
                   value = 5)
    ),
    column(
      3,
      numericInput("n_days",
                   "Length of trial (in days)",
                   value = 40)
    )),
  mainPanel(actionButton("browser", "browser"),
            textInput("sim_var_name", "Enter name for variable:"),
            distribution_sample_UI("sample_variable"),
            actionButton("gen", "Gen Variable")
  )
)


server <- function(input, output, session) {

  data <-data.frame(matrix(rnorm(300),ncol=3))
  sim_var_name <- reactive({
    stringr::str_trim(input$sim_var_name)
  })
  
  result <- distribution_sample_Server("sample_variable",
                                       tex_name =sim_var_name,
                                       n_participants = reactive(input$n_participants),
                                       n_days = reactive(input$n_days),
                                       decision_pts = reactive(input$decision_pts),
                                       gen = reactive(input$gen))

  observeEvent(input$browser,{
    browser()
  })

}

shinyApp(ui, server)