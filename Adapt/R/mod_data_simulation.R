library(shiny)
library(shinyjs)
library(tidyverse)
library(DT)

# source("R/utils_server.R")
# source("R/utils_ui.R")
# source("R/utils_latex_render.R")
# source("R/mod_calc_mean.R")
# source("R/mod_calc_sd.R")
# source("R/mod_error_dist.R")
# source("R/mod_sample_distribution.R")
# source("R/mod_operation_warning.R")
# source("R/mod_downloadData.R")
# source("R/mod_location_scale.R")


data_simulation_UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    # shinyjs::useShinyjs(),
    fluidRow(
      column(
        3,
        numericInput(ns("n_participants"),
                     "Number of Participants",
                     value = 20)
      ),
      column(
        3,
        numericInput(ns("decision_pts"),
                     "Decision points per day",
                     value = 5)
      ),
      column(
        3,
        numericInput(ns("n_days"),
                     "Length of trial (in days)",
                     value = 40)
      ),
      column(1,
             offset = 2
      )
    ),
    hr(),
    tabsetPanel(type = "tabs",
                tabPanel("Data Creation",
                         fluidRow(
                           column(
                             3,
                             textInput(ns("sim_var_name"), "Enter name for variable:"),
                             uiOutput(ns("var_title")),
                             shinyjs::hidden(
                               radioButtons(
                                 ns("independ_dist"),
                                 label = "Are the new variable and previous variables\n independently distributed?",
                                 choices = c( "Not selected" = "", "Yes", "No")
                               )
                             ),
                             tags$div(id = ns("indepent_variable"), distribution_sample_UI(ns("sample_variable"))),
                             shinyjs::disabled(actionButton(ns("gen_var"), "Generate")),
                           ),
                           shinyjs::hidden(
                             column(
                               9,
                               id = ns("loc_scale_column"),
                               location_scale_UI(ns("location_Scale"))
                               )
                             ) # End of loc_scale_column / closing of shinyjs hidden
                           )
                         ),
                tabPanel("View Data",
                  # Button Download Simulated Data,
                  column(1),
                  column(10,
                         DT::dataTableOutput(ns("sim_data")),
                         br(),
                         dataDownload_UI(ns("sim_data"))
                  ),
                  column(1)
                ),
                tabPanel("Data List",
                         set_html_breaks(1),
                         tags$div(id = ns("variable_summary"), 
                                  h2(strong("Variables"),
                                     tags$ul(id = ns("variable-list"),
                                             style="list-style-type:disc;")
                                  )
                         )
                )
    )
  )
}


data_simulation_Server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
                 simulated_data <- reactiveVal()
                 num_vars <- reactiveVal(1)
                 
                 # # Reactive values for calculating variables conditioally dependent on other variables
                 sim_mean <- reactiveVal()
                 sim_sd <- reactiveVal()
                 sim_error <- reactiveVal()

                 # # Reactive Values for latex parameters
                 # var_names_latex <- reactiveVal()
                 # latex_list <- reactiveVal()
                 # mean_latex <- reactiveVal()
                 # variance_latex <- reactiveVal()
                 # error_latex <- reactiveVal()
                 # 
                 sim_var_name <- reactive({
                   stringr::str_trim(input$sim_var_name) 
                 })
                 
                 # Display name of new variable
                 output$var_title <- renderUI({
                   req(input$sim_var_name)
                   sim_var_name <- sim_var_name() # Remove leading / trailing white space
                   validate(
                     need(!stringr::str_detect(sim_var_name, "^\\d"), "Name cannot start with digit."),
                     need(!stringr::str_detect(sim_var_name, "^_"), "Name cannot start with '_'."),
                     need(!stringr::str_detect(sim_var_name, "[[:space:]]"), "Name cannot contain spaces (replace with _)."),
                     need(!stringr::str_detect(sim_var_name, "[^_[:^punct:]]"), "Name cannot contain punctuation."),
                     need(!stringr::str_detect(sim_var_name, "[A-Z]"), "Name should be lower case."),
                     need(!(sim_var_name %in% names(simulated_data())), "Name exists in data. Cannot have duplicate variable names.")
                   )
                   
                   tags$div(h3(strong(paste0("Generating ", input$sim_var_name))))
                 })
                
                 
                 

                 location_scale <- location_scale_Server("location_Scale",
                                                         df = simulated_data)
                 
                 
                 independent_variable <- distribution_sample_Server("sample_variable",
                                                                    tex_name = sim_var_name,
                                                                    n_participants = reactive(input$n_participants),
                                                                    n_days = reactive(input$n_days),
                                                                    decision_pts = reactive(input$decision_pts),
                                                                    gen = reactive(input$gen_var))
                 
                 # Logic enabling different UI components for relevant stage
                 
                 observe({
                   req(num_vars()==1)
                   exists_variable <- !is.null(independent_variable$dist_id())
                   invalid_name <- validate_variable_name(input$sim_var_name,
                                                          names(simulated_data()))
                   if (!invalid_name & exists_variable) {
                     shinyjs::enable("gen_var")
                   } else {
                     shinyjs::disable("gen_var")
                   }
                 })
                 
                 # toggle gen_var button for independent variable generation ----
                 observe({
                   req(input$independ_dist)
                   exists_variable <- !is.null(independent_variable$dist_id())
                   invalid_name <- validate_variable_name(input$sim_var_name,
                                                          names(simulated_data()))
                   if (input$independ_dist == "Yes") {
                     if (!invalid_name & exists_variable) {
                       shinyjs::enable("gen_var")
                     } else {
                       shinyjs::disable("gen_var")
                     }
                   }
                 })
                 
                 acc <- reactiveVal()
                 
                 # toggle gen_var button for dependent variable generation ----
                 observe({
                   req(input$independ_dist)
                   invalid_name <- validate_variable_name(input$sim_var_name,
                                                          names(simulated_data()))
                   
                   null_mean <- is.null(location_scale$mean_params$calculated_mean())
                   null_sd <- is.null(location_scale$sd_params$calculated_sd())
                   null_error <- is.null(location_scale$error_params$simulated_error())

                   missing_params = any(c(null_mean,
                                          null_sd,
                                          null_error))
                   
                   if (input$independ_dist == "No") {
                     if (!invalid_name & !missing_params) {
                       shinyjs::enable("gen_var")
                     } else {
                       shinyjs::disable("gen_var")
                     }
                   }
                 })

                 
                 
                 observe({
                   if (num_vars()>1) {
                     shinyjs::show("independ_dist")
                   } else {
                     shinyjs::hide("independ_dist")
                   }
                 })
                 
                 observe({
                   cond_varname <- validate_variable_name(sim_var_name(), names(simulated_data()))
                   if (!cond_varname) {
                     shinyjs::enable("independ_dist")
                   } else {
                     shinyjs::disable("independ_dist")
                   }
                 })
                 
                 observe({
                   req(input$independ_dist)
                   shinyjs::hide("indepent_variable")
                   shinyjs::hide("loc_scale_column")
                   if (input$independ_dist=="No") {
                     shinyjs::hide("indepent_variable")
                     shinyjs::show("loc_scale_column")
                   } else {
                     shinyjs::show("indepent_variable")
                     shinyjs::hide("loc_scale_column")
                   }
                 })
                 
                 observe({
                   if (num_vars() == 1) {

                     sim_df <- data.frame(independent_variable$sampled_var())
                     names(sim_df) <- sim_var_name()
                     total <-  input$n_days*input$decision_pts

                     sim_df$decision_pt <- rep(rep(c(1:input$decision_pts), each=input$n_participants), input$n_days)
                     sim_df$day <- rep(rep(1:input$n_days, each=input$n_participants), each=input$decision_pts)
                     sim_df$time_pt <- rep(rep(1:total), each=input$n_participants)
                     sim_df$pid <- rep(1:input$n_participants, total)

                     simulated_data(sim_df)
                     
                     id = str_c("var-", num_vars(), collapse = "")
                     
                     insert_variable_UI(paste0("#", ns("variable-list")), where = "beforeEnd", id = ns(id),
                                        label = h3(independent_variable$name_latex()))

                   } 
                   else if (input$independ_dist == "Yes") {
                     sim_df <- simulated_data()
                     # sim_df[sim_var_name()] <- independent_variable$sampled_var()
                     sim_df <- cbind(independent_variable$sampled_var(), sim_df)
                     colnames(sim_df)[1] <- sim_var_name()
                     simulated_data(sim_df)
                     
                     id = str_c("var-", num_vars(), collapse = "")
                     
                     insert_variable_UI(paste0("#", ns("variable-list")), where = "beforeEnd", id = ns(id),
                                        label = h3(independent_variable$name_latex()))

                   } else {
                     sim_df <- simulated_data()

                     mu <- location_scale$mean_params$calculated_mean()
                     sigma <- location_scale$sd_params$calculated_sd()
                     error <- location_scale$error_params$simulated_error()
                     # sim_df[sim_var_name()] <- mu + sigma*error
                     
                     sim_df <- cbind(mu + sigma*error, sim_df)
                     colnames(sim_df)[1] <- sim_var_name()
                     simulated_data(sim_df)
                     
                     # Generating latex/ui component
                     mu_latex <- location_scale$mean_params$mean_latex()
                     sigma_latex <- location_scale$sd_params$sd_latex()
                     error_latex <- location_scale$error_params$error_latex()
                     
                     acc_id = str_c("accordion-", num_vars())
                     
                     acc_item <- tags$ul(
                       tags$li(h4(mu_latex)),
                       tags$li(h4(sigma_latex)),
                       tags$li(h4(error_latex))
                     )

                     tex_header = render_tex_inline(str_c(
                       tex_var_name(sim_var_name()),
                       " = \\mu(X) + \\sigma(X) \\cdot \\epsilon",
                       collapse = ""
                     ))
                     insert_accordion_list_item(paste0("#", ns("variable-list")), 
                                                where = "beforeEnd", 
                                                acc_id = ns(acc_id),
                                                label = h3(tex_header),
                                                acc_item
                     )
                     
                     runjs(run_accordion_js(ns(acc_id)))

                   }
                   
                   runjs('MathJax.Hub.Queue(["Typeset",MathJax.Hub]);') # Render new latex
                  
                   # Refresh text input for variable name
                   shinyjs::reset("sim_var_name")
                   shinyjs::disable("gen_var")
                   
                   shinyjs::reset("location_Scale")
                   
                   shinyjs::hide("indepent_variable")
                   shinyjs::hide("loc_scale_column")

                   updateRadioButtons(
                     session,
                     inputId = 'independ_dist',
                     label = "Are the new variable and previous variables\n independently distributed?",
                     choices = c("Yes", "No"),
                     selected = character(0)
                   )
                   
                   # Increment counter (indicates number of variables generated)
                   num_vars(num_vars() + 1)
                 }) %>%  
                   bindEvent(input$gen_var)
                 
                 output$sim_data <- DT::renderDataTable({
                   req(simulated_data())
                   datatable(simulated_data(),
                             selection = 'none',
                             options = list(dom="t",
                                            autoWidth = TRUE
                             ),
                             rownames = F)
                   
                 })
                 
                 dataDownload_Server("sim_data", df = simulated_data, file_name = "simulated_data.csv")
                
                 return(list(simulated_data, location_scale))
                 
               })
  
}

source("utils_server.R")
source("utils_ui.R")
source("utils_latex_render.R")
source("mod_calc_mean.R")
source("mod_calc_sd.R")
source("mod_error_dist.R")
source("mod_weighted_sum.R")
source("mod_sample_distribution.R")
source("mod_location_scale.R")
source("mod_downloadData.R")
source("mod_operation_warning.R")


ui <- fluidPage(
  useShinyjs(),tags$head(
    includeCSS("../www/accordion.css"), 
    includeCSS("../www/style.css"), 
    includeScript("../www/accordion.js") 
  ),
  mainPanel(actionButton("browser", "browser"),
            data_simulation_UI("data_simulation")
  )
)


server <- function(input, output, session) {

  result <- data_simulation_Server("data_simulation")
  
  observeEvent(input$browser,{
    browser()
  })
  
}

shinyApp(ui, server)