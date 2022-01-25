library(shiny)
library(shinyjs)
library(DT)
library(stringr)
library(reactable)
library(shinydashboardPlus)


library(listdtr)

# Local scripts
source("R/data_sim_modalDialog.R")
source("R/utils_ui.R")
source("R/utils_server.R")
source("R/utils_latex_render.R")
source("R/utils_data_proc.R")

# Modules
source("R/mod_warm_start.R")
source("R/mod_warm_start.R")

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
  
   observeEvent(input$browser2,{
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
 
 #### Dynamic UI components ####
 
 
 #### Modal Dialog for data simulation ####

 #### Init reactive values for data simulation ####   
   sim_params <-  reactiveValues(
      
      num_vars = 1 ,
      sim_data = NULL,
      cond_dist_step = 0, # tells us what stage we are at when generating location scale model
                           # 0 = Not active
                           # 1 = Specifying mean
                           # 2 = Specifying variance
                           # 3 = Specifying error
      
      expression_tbl = tibble(
         Select = character(0),
         Expression = character(0),
         Name = character(0)
      ),
      sim_mean = NULL,
      log_sim_variance = NULL,
      sim_error = NULL
   )
 
 tex_params <- reactiveValues(
    var_names = c(),
    tex_list = c(), # latex using for data dictionary tab
    mean_tex = NULL,
    variance_tex  = NULL,
    error_tex  = NULL
 )
 
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
         need(!(sim_var_name %in% names(sim_params$sim_data)), "Name exists in data. Cannot have duplicate variable names.")
         )
      
      tags$div(h3(strong(paste0("Generating ", input$sim_var_name))))
   })
   
   # Title for stage of conditional distribution
   output$param_specification <- renderUI({
      
      switch(sim_params$cond_dist_step,
         "1" = tags$div(h4(strong("Specify the mean"))), 
         "2" = tags$div(h4(strong("Specify the log variance"))),
         "3" = tags$div(h4(strong("Specify the error distribution"))),
         "" # default case
      )
         
   })
   
   # Display required parameters for selected distribution
   output$dist_params <-  renderUI({
      
      get_dist_ui(tolower(input$data_dist))
      
   })
   
   #(gen_var) Generating new variable for simulated data set ----
   observeEvent(input$gen_var, {
      
      
      n_participants <- input$n_participants
      decision_pts <- input$decision_pts
      n_days <- input$n_days
      
      # get latex name for future use
      tex_params$tex_name <- tex_var_name(sim_var_name()) 
      
      total <- n_days*decision_pts
      ## If new variable is independent (or first variable) then draw samples
      if (input$independ_dist == "Yes" || sim_params$num_vars == 1) {
         
         expr_str <- switch(
            input$data_dist,
            "Gaussian"  = guass_tex(mean = input$guass_mu, sd = input$guass_sd),
            "Bernoulli" = bern_tex(p = input$bern_p),
            "Binomial" =  binomial_tex(size = input$bin_n, p = input$bin_p),
            "Gamma"     = gamma_tex(shape = input$gamma_s, rate = input$gamma_r)
         ) %>%
            c(tex_params$tex_name, " \\sim ", .) %>%
            str_c(collapse = "") %>%
            render_tex_inline()
         
         id = str_c("var-", sim_params$num_vars, collapse = "")
         
         insert_variable_UI("#variable-list", where = "beforeEnd", id = id,
                          label = h3(expr_str))
         
     
         
         if (input$constant_time=="dec_pt") {
            new_var <- switch(
               input$data_dist,
               "Gaussian"  = rnorm(total*n_participants,
                                   mean = input$guass_mu,
                                   sd = input$guass_sd), 
               "Bernoulli" =  as.integer(rbernoulli(total*n_participants,
                                                    p = input$bern_p)), 
               
               "Binomial"  = rbinom(total*n_participants,
                                    size = input$bin_n,
                                    p = input$bin_p),
               
               "Gamma"     = rgamma(total*n_participants,
                                    shape = input$gamma_s,
                                    rate = input$gamma_r)
            )
            
         } else if (input$constant_time=="daily") {
           
             new_var <- switch (
               input$data_dist,
               "Gaussian"  = rep(rnorm(n_participants*decision_pts,
                                       mean = input$guass_mu,
                                       sd = input$guass_sd),
                                 n_days), 
               "Bernoulli" = rep(as.integer(rbernoulli(n_participants*decision_pts,
                                                       p = input$bern_p)),
                                 n_days), 
               
               "Binomial"  = rep(rbinom(n_participants*decision_pts,
                                        size = input$bin_n,
                                        p = input$bin_p),
                                 n_days),
               
               "Gamma"     = rep(rgamma(n_participants*decision_pts,
                                        shape = input$gamma_s,
                                        rate = input$gamma_r),
                                 n_days)
             )
         } else if (input$constant_time=="trial") {
            new_var <- switch (
               input$data_dist,
               "Gaussian"  = rep(rnorm(n_participants,
                                   mean = input$guass_mu,
                                   sd = input$guass_sd),
                                 total), 
               "Bernoulli" = rep(as.integer(rbernoulli(n_participants,
                                                    p = input$bern_p)),
                                 total), 
               
               "Binomial"  = rep(rbinom(n_participants,
                                    size = input$bin_n,
                                    p = input$bin_p),
                                 total),
               
               "Gamma"     = rep(rgamma(n_participants,
                                    shape = input$gamma_s,
                                    rate = input$gamma_r),
                                 total)
            )
            
         }
         
      } else {
         
         # getting sd from log variance
         sd <- sqrt(exp(sim_params$log_sim_variance))
         
         new_var <- sim_params$sim_mean + sd*sim_params$sim_error
      
         acc_id = str_c("accordion-", sim_params$num_vars)
         ids <- c("mean", "variance", "error")
         
         acc_item <- map(ids, 
                         ~ tags$li(h4(tex_params[[str_c(., "_tex")]])
           
         )) %>%
            tags$ul()
         
         tex_header = render_tex_inline(str_c(
            tex_params$tex_name,
            " = \\mu(X) + \\sigma(X) \\cdot \\epsilon",
            collapse = ""
         ))
         insert_accordion_list_item("#variable-list", 
                                    where = "beforeEnd", 
                                    acc_id = acc_id,
                                    label = h3(tex_header),
                                    acc_item
                          )
         
         runjs(run_accordion_js(acc_id))
      }
      
      runjs('MathJax.Hub.Queue(["Typeset",MathJax.Hub]);') # Render new latex
      
      if (sim_params$num_vars == 1) {
         
         # sim_var_name = stringr::str_trim(input$sim_var_name)
         sim_params$sim_data <- data.frame(new_var)
         colnames(sim_params$sim_data) <- sim_var_name()
         
      } else {
         sim_params$sim_data[sim_var_name()] <- new_var
      }
      
      # Adding latex name to list for further use in expressions
      tex_params$var_names[sim_var_name()] <- tex_params$tex_name

      ### Initialize variables for constructing conditional distribution
      sim_params <- update_sim_params(sim_params, cond_dist_step = 1, sim_params$num_vars + 1, sim_data = sim_params$sim_data)

      
      updateRadioButtons(
         session,
         inputId = 'independ_dist',
         label = "Are the new variable and previous variables\n independently distributed?",
         choices = c("Yes", "No"),
         selected = character(0)
      )
      
      
      
      shinyjs::reset("sim_var_name")
      shinyjs::hide("loc_scale_column")
      shinyjs::hide("constant_time")
      shinyjs::enable("sim_var_name")
      shinyjs::disable("gen_var")
      
      ## Updates select input with current simulated data variables
      ## Allows user to build conditional distribution for a new variable 
      ## using these previously generated variables
      ## Only displayed if "independ_dist" == "No"
      
      updateConditionalVars(session = session, names = names(sim_params$sim_data))
     
      
   })
   
   output$sim_data <- DT::renderDataTable({
      req(sim_params$sim_data)
      datatable(sim_params$sim_data,
                selection = 'none',
                options = list(dom="t",
                               autoWidth = TRUE
                               ),
                rownames = F)
      
      })
 
   # Downloadable csv of simulated dataset ----
   output$downloadSimData <- downloadHandler(
      filename = function() {
         "simulated_data.csv"
      },
      content = function(file) {
         write.csv(sim_params$sim_data, file, row.names = FALSE)
      }
   )
   
   
   ### !! I belive all this observe stuff should be done in javascript
   
   # Logic to (en/dis)able action buttons in modal dialog ----
   
   source(file.path("R", "utils_enableLogic_modalDialog.R"))
   
   
   toggle_genVar_btn_init(input, sim_params)
   toggle_genVar_btn_IV(input, sim_params)
   toggle_genVar_btn_DV(input, sim_params)
   
   
   toggle_apply_op_btn(input, sim_params)
   
   toggle_mean_variance_btn(input, sim_params)
   
   toggle_err_btn(input, sim_params)
   
   toggle_indpendence_btn(input, sim_params)

   toggle_name_btn1(input)
   toggle_name_btn2(input)
   
   #### Dynamic Display of operation choices for mean/var specification #### 
   source(file.path("R", "utils_visibilityLogic_modalDialog.R"))
   
   visibility_init_simulation(sim_params)
   
   visibility_variabile_options(input)
   
   visibility_location_scale_steps(sim_params)
   
   visibility_operation_choices(input)
   
   visibility_weighted_sum(input)
   
   #### Mean/Var/Error calculation reactivity ####
   
   conditional_vars <- reactive({
      req(sim_params$param_data)
      sim_params$param_data %>% 
         select(sim_params$var_choices[input$conditional_vars])
   })
   
   output$var_creation_warning <- renderText({
      req(conditional_vars)
      if (input$unary_operation == "ln" & any(conditional_vars() <= 0)) {
         validate("Selected data must be postive real number for this transformation")
      } else if (input$unary_operation == "^" & !is.integer(input$power_val) & any(conditional_vars() < 0)) {
         validate("Taking root of negative number may produce NaNs")
      } 
   })
   
   ### Get weights if weighted sum chosen
   output$weighted_sum_inputs <- renderUI({
      req(input$multi_operation == "weighted sum")
      tags$div(
         h4(strong("Assign weights:")),
         purrr::map(input$conditional_vars,
                    ~ numericInput(inputId = paste('wgt_',.x) ,
                                   label = .x, 
                                   value = 1)
         )
      )
   }) 
# apply_operation ----
   observeEvent(input$apply_operation, {
      current_data <- conditional_vars()

      prefix = switch(sim_params$cond_dist_step, "1" = "mean", "2" = "var")

      new_var_name <- paste(
         paste0(prefix, "__", sep = ""),
         sim_params$var_cnt,
         sep = "")
      
      weights <- NULL
      pow <- NULL
      input_val = "NA"
      display_name <- paste("expression", sim_params$var_cnt, collapse = " ")

      if (length(input$conditional_vars) == 1) {

         operation_name = input$unary_operation

         ## If they choose to take power of variable
         if (operation_name=="^") {

            pow = input$power_val
            input_val = as.character(pow)

            new_var <- unary_operator(operation_name,
                                      current_data,
                                      pow)

         } else {

            new_var <- unary_operator(operation_name,
                                      current_data)
         }

      } else if (length(input$conditional_vars) > 1) {

         operation_name = input$multi_operation
         # Logic changes if they choose weighted sum
         if (input$multi_operation == "weighted sum") {

            weights <- sapply(input$conditional_vars, function(x){
               input[[paste('wgt_',x)]]})
            
            input_val = paste0(weights, collapse = ", ")

            new_var <- apply(current_data, 1,
                             function(x) {
                                sum(x*weights)
                             }
            )

         } else {
               new_var <- apply(current_data, 1,
                                function(x) {
                                   n_ary_operator(operation_name, x)
                                   }
                                )
               }
      }

      sim_params$var_choices[display_name] = new_var_name
      sim_params$param_data[new_var_name] = new_var
      
      # Generate expression for data dict
      tex_vars <- tex_params$var_names[input$conditional_vars]
      tex <- func_to_tex(operation_name, X = tex_vars, pow = pow, weights = weights)
      tex_params$var_names[display_name] <- tex
      tex_params$expr_list <- c(tex_params$expr_list, tex)
      

      updateConditionalVars(session=session, names = names(sim_params$var_choices))


      sim_params$expression_tbl <- sim_params$expression_tbl  %>%
         dplyr::bind_rows(
            tibble(
               Select = create_checkbox(sim_params$var_cnt),
               Expression = gen_MathJax_html(tex),
               Name = display_name
            )
         )
      
      # Need to think about how to get previous text expression involved 
      # sim_params$tex_expr <- expr_to_tex()

      sim_params$var_cnt <- sim_params$var_cnt + 1


      shinyjs::reset("multi_operation")
      shinyjs::reset("unary_operation")
      shinyjs::disable("apply_operation")
   })


    output$loc_scale_tbl <- DT::renderDataTable({
       datatable( sim_params$expression_tbl,
                 options = list(dom="t",
                                paging = FALSE,
                                ordering = FALSE), 
                 escape = F,
                 rownames = F,
                 selection = 'single')
    })
   
   
   # Conditional Mean ----
   ## Store variable with associated expression as mean
   observeEvent(input$calc_mean, {
      row_id <- as.numeric(input$expr_row) ## set in javascript code
      
      # We extract the variable display name from the table using row_id
      # Using this we get the column name from var_choices
      # With this we can extract the appropiate column for the mean
      
      col_id <- sim_params$var_choices[[sim_params$expression_tbl[row_id, "Name", drop=T]]]
      sim_mean <- sim_params$param_data[[col_id]]

      ### Initialize variables for constructing conditional distribution
      sim_params <- update_sim_params(sim_params, cond_dist_step = 2, 
                                      sim_params$num_vars, sim_data = sim_params$sim_data, mean=sim_mean)
     
      updateConditionalVars(session=session, names = names(sim_params$var_choices))
     
      tex_params$mean_tex <- tex_params$expr_list[row_id] %>% unname() %>% 
         str_c("\\mu(X) =", ., collapse = "") %>% 
         render_tex_inline()

      tex_params <-  reset_expr_params(tex_params,
         tex_params$var_names[names(sim_params$var_choices)]
         )
      
      shinyjs::reset("multi_operation")
      shinyjs::reset("unary_operation")
      
      shinyjs::toggleState("calc_mean")
      shinyjs::hide("calc_mean")
      shinyjs::show("calc_variance")
   })
   
   #### Conditional Variance ####
   # Store variable with associated expression as variance
   observeEvent(input$calc_variance, {
      row_id <- as.numeric(input$expr_row) ## set in javascript code
      
      # We extract the variable display name from the table using row_id
      # Using this we get the column name from var_choices
      # With this we can extract the appropriate column for the mean
      
      col_id <- sim_params$var_choices[[sim_params$expression_tbl[row_id, "Name", drop=T]]]
      log_sim_variance <- sim_params$param_data[[col_id]]

      ### Initialize variables for constructing conditional distribution
      sim_params <- update_sim_params(sim_params, cond_dist_step = 3,
                                      sim_params$num_vars,
                                      sim_data = sim_params$sim_data,
                                      mean = sim_params$sim_mean,
                                      log_variance = log_sim_variance)
      
      updateConditionalVars(session=session, names = names(sim_params$var_choices))
      
    
      tex_params$variance_tex <- tex_params$expr_list[row_id] %>% unname() %>% 
         str_c("\\sigma(X) = \\sqrt{e^{", ., "}}", collapse = "") %>% 
         render_tex_inline()
      
      tex_params <-  reset_expr_params(tex_params,
                                       tex_params$var_names[names(sim_params$var_choices)]
      )
      
      shinyjs::reset("multi_operation")
      shinyjs::reset("unary_operation")
  
      shinyjs::toggleState("calc_variance")
      shinyjs::hide("calc_variance")
   
      
      shinyjs::show("calc_error")
      shinyjs::show("error_dist")
      shinyjs::show("cond_err_dist")
   })

   #### Conditional Error ####
   # Display required parameters for selected distribution
    
    output$cond_err_dist <-  renderUI({
       
       req(input$error_dist)
       err_dist_ui(tolower(input$error_dist))
    
       })
    

   ## Sample noise from selected distribution
   observeEvent(input$calc_error, {
      n_total <- input$n_participants*input$decision_pts*input$n_days
      
      sim_params$sim_error <- switch (
         tolower(input$error_dist),
         "gaussian" = rnorm(n_total,
                            0,
                            sd = input$guass_err_sd),
         "double exponential" = nimble::rdexp(n_total,
                                              0,
                                              input$dexp_err_scale)
      )
 
      
      tex_params$error_tex <- switch(tolower(input$error_dist),
                                       "gaussian"  = guass_tex(mean = 0, sd = input$guass_err_sd),
                                       "double exponential" = laplace_tex(location = 0, scale = input$dexp_err_scale))  %>% 
         str_c("\\epsilon \\sim", ., collapse = "") %>% 
         render_tex_inline()
      
      shinyjs::hide("cond_err_dist")
      shinyjs::hide("error_dist")
      shinyjs::hide("calc_error")
   })

   observeEvent(input$simulate_data, {
      decision_pts <- input$decision_pts
      n_participants <- input$n_participants
      n_days <- input$n_days
      total <- n_days*decision_pts
      data <- sim_params$sim_data
      
      data$decision_pt <- rep(rep(c(1:decision_pts), each=n_participants), n_days)
      data$day <- rep(rep(1:n_days, each=n_participants), each=decision_pts)
      data$time_pt <- rep(rep(1:total), each=n_participants)
      data$pid <- rep(1:n_participants, total)
      
      updateSelectInput(
         session = session,
         inputId = 'calc_vars',
         choices = names(data)
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
        choices = names(data)
      )
      
      updateSelectInput(
        session = session,
        'sel_outcome',
        label = "Choose Outcome",
        choices = names(data)
      )
      
      updateSelectInput(
        session = session,
        'sel_action',
        label = "Specify actions",
        choices = names(data)
      )
      
      variable_choices <- names(data)
      names(variable_choices) <- variable_choices
      
      prob_values$probs = numeric(length = nrow(data))
      prob_values$treatment = numeric(length = nrow(data))
      prob_values$variable_choices = variable_choices
      prob_values$data = data
      
      ## Reset simulation parameters
      sim_params$num_vars = 1 
      sim_params$sim_data = NULL
      sim_params$cond_dist_step = 0
      
      shinyjs::reset("independ_dist")
      shinyjs::reset("sim_var_name")
      shinyjs::reset("data_choice")
      
      
      # Show inputs that set up dynmaic treatment regime
      shinyjs::show("setup_dynr")
      
      removeModal()
      
   })
   
   # Model Init ----
   
  # Module for feature generation
  # feature_gen <-  feature_gen_server("var_transform", data = reactive(prob_values$data))
  

   # For debugging
   observeEvent(input$browser,{
     browser()
   })

   # Transform feature and update list choices for select inputs
   feature_gen <-  feature_gen_server("var_transform", data = reactive(prob_values$data), gen = reactive(input$create_var))

  observeEvent(input$create_var, {
    
     data = prob_values$data
     data[input$created_var_name] <-  feature_gen()
      
     # df(data)

     updateSelectInput(
       session = session,
       'sel_covariates',
       label = "Choose Covariates",
       choices = names(data)
     )

     updateSelectInput(
       session = session,
       'sel_outcome',
       label = "Choose Outcome",
       choices = names(data)
     )

     updateSelectInput(
       session = session,
       'sel_action',
       label = "Specify actions",
       choices = names(data)
     )
     
     prob_values$data = data
     
   })


   
   
   # get treatment regime 
   dyn_treat <- eventReactive(input$sim_DLs, {
     data       <- prob_values$data 
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
    
   
   ## Open modal dialog to simulate data ##
   observeEvent(input$data_simulation, {
         
      showModal(data_gen_modal)
      
   }, 
   ignoreInit = TRUE)
  
   
   
   ##### Calculations for Randomization Design ####
   data_randomized <- randomizationProb_Server("randomize",
                                      X = data_for_sim,
                                      reset = reactive(input$reset))
   
   
   
   
   

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


