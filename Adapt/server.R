library(shiny)
library(shinyjs)
library(DT)
library(stringr)
library(reactable)
library(shinydashboardPlus)


library(listdtr)

# Local scripts
# source("R/data_sim_modalDialog.R")
source("R/utils_ui.R")
source("R/utils_server.R")
source("R/utils_latex_render.R")
source("R/utils_data_proc.R")

# Modules
source("R/mod_warm_start.R")
source("R/mod_weighted_sum.R")
source("R/mod_data_simulation.R")


# Define modal dialog
data_gen_modal <- modalDialog(
  data_simulation_UI("data_simulation"),
  title = "Simulate Dataset",
  footer = actionButton("simulate_data", "Finish Simulation"),
  size = "l" # made modal window large
)

# Define server logic required to draw a histogram
# remove shinyServer!
shinyServer(function(input, output, session) {
  
 # Reactive Values used throughout the app 
  data_for_sim <- reactiveVal()
  
  
  ### Initialize values for probability calculation ###
  prob_values <-  reactiveValues(
    
    agg_cnt = 0,
    probs = NULL,
    treatment = NULL,
    variable_choices = NULL,
    generated_variables = c(),
    data = NULL
    
  )
  
   observeEvent(input$browser,{
      browser()
   })


   observeEvent(input$upload_file, {
      
      shinyjs::toggle("file_info")
      
   })
   
   upload_data  <- reactive({
      req(input$file_info$name)
     
      ext <- tools::file_ext(input$file_info$datapath)
      validate(need(ext == "csv", "Please upload a csv file"))
         
      return(read.csv(input$file_info$datapath))
      
      })
   
      
    observeEvent(input$file_info, {
       data_for_sim(upload_data())
      })


# Warm Start ----
 warm_start <- warmStartServer("warmstrt")
 
 warm_start_act <- eventReactive(input$init_warmstrt, {
   req(prob_values$data)
   
   return(eval_warm_start(d_list = warm_start(), prob_values$data))
   
 })
 
 output$warmStartBar <- renderPlot({
   
   data.frame(actions = warm_start_act()) %>% 
     ggplot(aes(x = actions)) +
     geom_bar() +
     theme_classic()
   
 })

 #### Modal Dialog for data simulation #### ----
 
 simulated_data <- data_simulation_Server("data_simulation")
 
   observeEvent(input$simulate_data, {

      data <- simulated_data()
      
      total <- max(data$time_pt)
      
      # updateSelectInput(
      #    session = session,
      #    inputId = 'calc_vars',
      #    choices = names(data)
      # )
      # 
      # updateSliderInput(
      #   session = session,
      #   inputId = "view_stages", 
      #   min = 1,
      #   value = c(1,3),
      #   max=total,
      #   "Choose stages to view"
      #   )
      # 
      # updateSelectInput(
      #   session = session,
      #   'sel_covariates',
      #   label = "Choose Covariates",
      #   selected = character(0),
      #   choices = names(data)
      # )
      # 
      # updateSelectInput(
      #   session = session,
      #   'sel_outcome',
      #   label = "Choose Outcome",
      #   selected = character(0),
      #   choices = names(data)
      # )
      # 
      # updateSelectInput(
      #   session = session,
      #   'sel_action',
      #   label = "Specify actions",
      #   selected = character(0),
      #   choices = names(data)
      # )
      
      # Update data set used for dynamic treatment regime
      data_for_sim(data)
      
      # Show inputs that set up dynmaic treatment regime
      shinyjs::show("setup_dynr")
      
      removeModal()
      
   })
   
   
   
   # Model Init ----
   
   # For debugging
   observeEvent(input$browser,{
     browser()
   })

   # Transform feature and update list choices for select inputs
   feature_gen <-  feature_gen_server("var_transform", data = data_for_sim, gen = reactive(input$create_var))

   observe({
     name <- input$created_var_name
     if (name!="") {
       shinyjs::enable("create_var")
     } else {
       shinyjs::disable("create_var")
     }
   })

  observeEvent(input$create_var, {
    
     data = data_for_sim()
     data[input$created_var_name] <-  feature_gen()
     
     # updateSelectInput(
     #   session = session,
     #   'sel_covariates',
     #   label = "Choose Covariates",
     #   choices = names(data)
     # )
     # 
     # updateSelectInput(
     #   session = session,
     #   'sel_outcome',
     #   label = "Choose Outcome",
     #   choices = names(data)
     # )
     # 
     # updateSelectInput(
     #   session = session,
     #   'sel_action',
     #   label = "Specify actions",
     #   choices = names(data)
     # )
     # 
     # Update dataset so it includes newly created variable
     data_for_sim(data)
     shinyjs::reset(id = "created_var_name")
   })


   observe({
     input$reset
     data <- data_for_sim()
     if (is.null(data)) {
       total = 1
       choices = list()
     } else {
       total <- max(data$time_pt)
       choices = names(data)
     }
     updateSelectInput(
       session = session,
       inputId = 'calc_vars',
       choices = choices
     )
     
     updateSliderInput(
       session = session,
       inputId = "view_stages", 
       min = 1,
       value = c(1,3),
       max=total,
       "Choose stages to view"
     )
     
     updateSelectInput(
       session = session,
       'sel_covariates',
       label = "Choose Covariates",
       selected = character(0),
       choices = choices
     )
     
     updateSelectInput(
       session = session,
       'sel_outcome',
       label = "Choose Outcome",
       selected = character(0),
       choices = choices
     )
     
     updateSelectInput(
       session = session,
       'sel_action',
       label = "Specify actions",
       selected = character(0),
       choices = choices
     )
     
   })
   
   # get treatment regime 
   dyn_treat <- eventReactive(input$sim_DLs, {
     data       <- data_for_sim()
     covariates <- input$sel_covariates
     outcome    <- input$sel_outcome
     action     <- input$sel_action
     n_stages   <- max(data$time_pt)
     
     #
     data_dl <- format_data_dl(data, pid="pid", timeid="time_pt", outcome = outcome, covariates = c(covariates, action))
     y <- as.matrix(get_dl_outcome(data_dl, outcome))
     x <- as.matrix(get_dl_vars(data_dl, covariates))
     a <- as.matrix(get_dl_vars(data_dl, action))
     stages <- rep(1:n_stages, length(covariates))
     
     # ! need actions still 
     dtr <- listdtr(y = y, x = x, a = a , stage.x = stages)
     
     dtr
})
   
   output$dl_plot <- renderPlot({
     req(input$view_stages)
     stages = input$view_stages
     stage_intv <- c(stages[1]:stages[2])
     plot(dyn_treat(), stages = stage_intv)
   })
   
   observeEvent(input$dply_dlplot, {
     shinyjs::toggle("dl_display")
   })
    
   
   # Open modal dialog to simulate data ----
   observeEvent(input$data_simulation, {
         
      showModal(data_gen_modal)
      
   }, 
   ignoreInit = TRUE
   )
  
   ##### Calculations for Randomization Design ####
   data_randomized <- randomizationProb_Server("randomize",
                                      X = data_for_sim,
                                      reset = reactive(input$reset))
   
   observeEvent(input$reset, {
     data_for_sim(NULL)
   })
   
    }
)





# testing = T
# observe({
#   if (testing) {
#     df <- reactiveVal()
#     
#     shinyjs::show("setup_dynr")
#     
#     decision_pts <- 2
#     n_participants <- 50
#     n_days <- 4
#     total_pts <- n_days*decision_pts
#     total_obs <- n_participants*total_pts
#     
#     data <- data.frame(matrix(rnorm(total_obs*3),ncol=3))
#     data$action <- rbinom(n=nrow(data), size=1, p=.5)
#     data$outcome <- 2*data$action*data$X1 - data$X2
#     
#     data$decision_pt <- rep(rep(c(1:decision_pts), each=n_participants), n_days)
#     data$day         <- rep(rep(1:n_days, each=n_participants), each=decision_pts)
#     data$time_pt     <- rep(rep(1:total_pts), each=n_participants)
#     data$pid         <- rep(1:n_participants, total_pts)
#     
#     prob_values$data = data
#     # df(data)
# 
#     updateSelectInput(
#       session = session,
#       inputId = 'calc_vars',
#       choices = names(data)
#     )
#     
#     
#     updateSliderInput(
#       session = session,
#       inputId = "view_stages",
#       min     = 1,
#       value   = c(1,3),
#       max     = total_pts,
#       "Choose stages to view"
#     )
#     
#     updateSelectInput(
#       session = session,
#       'sel_covariates',
#       label = "Choose Covariates",
#       choices = names(data)
#     )
#     
#     updateSelectInput(
#       session = session,
#       'sel_outcome',
#       label = "Choose Outcome",
#       choices = names(data)
#     )
#     
#     updateSelectInput(
#       session = session,
#       'sel_action',
#       label = "Specify actions",
#       choices = names(data)
#     )
#     
#   }
#   
# })
# 


