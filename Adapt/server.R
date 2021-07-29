library(shiny)
library(DT)
library(stringr)
library(reactable)

source("R/data_sim_modalDialog.R")
source("R/utils_ui.R")
source("R/utils_server.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

   ### Initialize values for probability calculation ###
   prob_values <-  reactiveValues(

      agg_cnt = 0,
      probs = NULL,
      treatment = NULL,
      variable_choices = NULL,
      generated_variables = c(),
      data = NULL

   )

   # For debugging
   observeEvent(input$browser,{
      browser()
   })

   observeEvent(input$browser2,{
      browser()
   })

   upload_data  <- reactive({
      req(input$file_info$name)
     
      ext <- tools::file_ext(input$file_info$datapath)
      validate(need(ext == "csv", "Please upload a csv file"))
         
      return(read.csv(input$file_info$datapath))
      
      })
   
      
    observeEvent(input$file_info, {
   
       data <- upload_data()
       updateSelectInput(
       session = session,
       inputId = 'calc_vars',
       choices = names(data)
       )

    
    variable_choices <- names(data)
    names(variable_choices) <- variable_choices

    prob_values$probs = numeric(length = nrow(data))
    prob_values$treatment = numeric(length = nrow(data))
    prob_values$variable_choices = variable_choices
    prob_values$data = data

 })

 output$vars <- renderText({
    names(prob_values$data)
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
         Variables = character(0),
         Expression = character(0),
         Arguments = character(0),
         Name = character(0)
      ),
      sim_mean = NULL,
      log_sim_variance = NULL,
      sim_error = NULL
   )
   
   # Display name of new variable
   output$var_title <- renderUI({
      req(input$sim_var_name)
      sim_var_name <- stringr::str_trim(input$sim_var_name) # Remove leading / trailing white space
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
         "1" = tags$div(h4(strong("Specify the Mean"))), 
         "2" = tags$div(h4(strong("Specify the Variance"))),
         "3" = tags$div(h4(strong("Specify the error distribution"))),
         "" # default case
      )
         
   })
   
   # Display required parameters for selected distribution
   output$dist_params <-  renderUI({
      
      get_dist_ui(tolower(input$data_dist))
      
   })
   
   #### ObserveEvent: Generating new variable for simulated data set ####
   observeEvent(input$gen_var, {
      n_participants <- input$n_participants
      decision_pts <- input$decision_pts
      n_days <- input$n_days
      
      total <- n_days*decision_pts
      
      ## If new variable is independent (or first variable) then draw samples
      if (input$independ_dist == "Yes" || sim_params$num_vars == 1) {
         if (input$time_varying=="Yes") {
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
         } else {
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
         sd <- sqrt(exp(sim_params$log_sim_variance))
         
         new_var <- sim_params$sim_mean + sd*sim_params$sim_error
      }
      
      if (sim_params$num_vars == 1) {
         
         sim_var_name = stringr::str_trim(input$sim_var_name)
         sim_params$sim_data <- data.frame(new_var)
         colnames(sim_params$sim_data) <- sim_var_name
         
      } else {
         
         sim_var_name = stringr::str_trim(input$sim_var_name)
         sim_params$sim_data[sim_var_name] <- new_var
      }
      
      #sim_params$num_vars = sim_params$num_vars  + 1
      
      ### Initialize variables for constructing conditional distribution
      sim_params <- update_sim_params(sim_params, cond_dist_step = 1, sim_params$num_vars + 1, sim_data = sim_params$sim_data)

      
      updateRadioButtons(
         session,
         inputId = 'independ_dist',
         label = "Are the new variable and previous variables\n independently distributed?",
         choices = c("Yes", "No"),
         selected = character(0)
      )
      
      # shinyjs::reset("independ_dist")
      shinyjs::reset("sim_var_name")
      shinyjs::hide("loc_scale_column")
      shinyjs::hide("time_varying")
      
      ## Updates select input with current simulated data variables
      ## Allows user to build conditional distribution for a new variable 
      ## using these previously generated variables
      ## Only displayed if "independ_dist" == "No"
      
      updateConditionalVars(session = session, names = names(sim_params$sim_data))
      
   })
   
   output$sim_data <- DT::renderDataTable({
      req(sim_params$sim_data)
      datatable(sim_params$sim_data,
                    options = list(dom="t"),
                    rownames = F)
      
      })
   
   
   ### !! I belive all this observe stuff should be done in javascript
   
   # Logic to (en/dis)able action buttons in modal dialog ----
   
   source(file.path("R", "utils_enableLogic_modalDialog.R"))
   # file.path("R", "utils_enableLogic_modalDialog.R")
   
   toggle_genVar_btn(input, sim_params)
   
   toggle_apply_op_btn(input, sim_params)
   
   toggle_mean_variance_btn(input, sim_params)
   
   toggle_err_btn(input, sim_params)
   
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

   observeEvent(input$apply_operation, {
      current_data <- conditional_vars()

      prefix = switch(sim_params$cond_dist_step, "1" = "mean", "2" = "var")

      new_var_name <- paste(
         paste0(prefix, "__", sep = ""),
         sim_params$var_cnt,
         sep = "")

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

            wghts <- sapply(input$conditional_vars, function(x){
               input[[paste('wgt_',x)]]})

            input_val = paste0(wghts, collapse = ", ")

            new_var <- apply(current_data, 1,
                             function(x) {
                                sum(x*wghts)
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

      updateConditionalVars(session=session, names = names(sim_params$var_choices))


      sim_params$expression_tbl <- sim_params$expression_tbl  %>%
         dplyr::bind_rows(
            tibble(
               Select = create_btn(sim_params$var_cnt),
               Variables = paste0(input$conditional_vars,
                                  collapse = ", "),
               Expression = operation_name,
               Arguments = input_val,
               Name = display_name
            )
         )



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
   
   
   #### Conditional Mean ####
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
      # With this we can extract the appropiate column for the mean
      
      col_id <- sim_params$var_choices[[sim_params$expression_tbl[row_id, "Name", drop=T]]]
      log_sim_variance <- sim_params$param_data[[col_id]]

      ### Initialize variables for constructing conditional distribution
      sim_params <- update_sim_params(sim_params, cond_dist_step = 3,
                                      sim_params$num_vars,
                                      sim_data = sim_params$sim_data,
                                      mean = sim_params$sim_mean,
                                      log_variance = log_sim_variance)
      
      updateConditionalVars(session=session, names = names(sim_params$var_choices))
      
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
      
      
      updateSelectInput(
         session = session,
         inputId = 'calc_vars',
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
      
      
      removeModal()
      
   })
   
   ## Open modal dialog to simulate data ##
   observeEvent(input$data_choice, {
      if (input$data_choice == "Simulate") {
         
         showModal(data_gen_modal)
         
      }
   }, 
   ignoreInit = TRUE)
  
   
   
   ##### Calculations for Randomization Design ####
   
   covariates <- reactive({
      
      prob_values$data %>% 
         select(prob_values$variable_choices[input$calc_vars])
      
   })
   
   # Here we use the input var
  var_wgts <- reactive({
    sapply(input$calc_vars, function(x){
      input[[paste('wgt_',x)]]
    })
  })
  
  
  
  # Request Input weights for weighted average #
   output$weighted_avg <- renderUI({
     tags$div(
       h4(strong("Assign weights:")),
       purrr::map(input$calc_vars,
                  ~ numericInput(inputId = paste('wgt_',.x) ,
                                 label = .x, 
                                 value = round(1/length(input$calc_vars), 3))
       )
     )
   })
     

   
   #### Data Processing ####
   
   #### Data Aggregation ####
   aggregations <- reactive({
     # if weighted average chosen
     if (input$data_agg == "weighted average") {
   
       apply(covariates(),
             MARGIN = 1,
             function(x) {
               data_agg[[input$data_agg]](x, var_wgts())
             }
       )
     }
      
     # Aggregate data according to selected transformation
     apply(covariates(),
           MARGIN = 1,
           data_agg[[input$data_agg]])
     
   })
   
   ### Get Probabilities ###
   probabilities <- reactive({
     
     # Apply selected transformation to calculate probability
     prob_maps[[input$prob_map]](aggregations())
     
   })
   

   # Update Inputs
   observeEvent(input$apply_aggs, {
      
      ### Update probabilities each time a sequence is applied ###
      prob_values$agg_cnt = prob_values$agg_cnt + 1 
      prob_values$probs = (prob_values$probs + probabilities()) / prob_values$agg_cnt 
      
      # Add new variable 
      new_var_name = paste0('z', prob_values$agg_cnt, '__')
      new_var_disp = paste0('Synthetic ', prob_values$agg_cnt)
      
      # Update data frame 
      prob_values$data[new_var_name] <- prob_values$probs
      
      # Add Construct Variable to input choices
      prob_values$variable_choices[new_var_disp] <- new_var_name
      
      # Save new variables to be removed on reset
      prob_values$generated_variables <- c(prob_values$generated_variables, new_var_name)
      
      insertUI("table tbody",
               where = "beforeEnd",
               ui = tags$tr(class = "agg-seq",
                            tags$td(align = "center", 
                                    style = "word-wrap: break-word;",
                                    paste0(input$calc_vars, collapse = ", ")),
                            tags$td(align = "center", input$data_agg),
                            tags$td(align = "center", input$prob_map),
                            tags$td(align = "center", new_var_disp)
               )
      )
      
      # Reset select inputs 
      updateSelectInput(
         session = session,
         inputId = 'calc_vars',
         choices = names(prob_values$variable_choices))
      
      updateSelectizeInput(
         session = session,
         inputId = 'data_agg',
         choices = list("sum",
                        "average",
                        "weighted average"),
         options = list(
            maxItems = 1,
            placeholder = "select data transformation",
            onInitialize = I('function() { this.setValue(0); }')
         ) 
      )
      
      updateSelectizeInput(
         session = session,
         inputId = 'prob_map',
         choices = list("expit", 
                        "tanh", 
                        "arctan"),
         options = list(maxItems = 1,
                        placeholder = "select probability generation",
                        onInitialize = I('function() { this.setValue(0); }')) 
      )
      
   })
   

   
   observeEvent(input$get_prob, {
     prob_values$treatment = rbernoulli(n = length(prob_values$probs),
                                        p = prob_values$probs)
     
   })
   
   
   # Reset environment ----
   observeEvent(input$reset, {
      
      prob_values$agg_cnt = 0
      prob_values$probs =  numeric(length = nrow(prob_values$data))
      
      # Remove newly generated variables from data set
      prob_values$data <- prob_values$data %>% 
         dplyr::select(-prob_values$generated_variables)
      
      prob_values$variable_choices <- names(prob_values$data)
      names(prob_values$variable_choices) <- prob_values$variable_choices
      prob_values$generated_variables <-  c()
      
      updateSelectInput(
         session = session,
         inputId = 'calc_vars',
         choices = names(prob_values$variable_choices))
      
      shinyjs::reset("data_choice")
      update_sim_params(sim_params, cond_dist_step = 0, num_vars = 1, sim_data = sim_params$sim_data)
 
   })
   
   ### Assign treatment to participants ###
   output$selected <-  renderTable({head(upload_data())})
   output$cnt <-  renderText({ prob_values$agg_cnt })

   output$prob_plot <-  renderPlot({
     req(input$apply_aggs)
     data.frame(p =  prob_values$probs) %>%
       ggplot(aes(x=p)) +
       geom_histogram() +
       labs(x = "Ranomization Probabilities", y = "Count",
            title = "Histogram of Randomiztion Probabilities") +
       theme_classic()
   })

   # #### Treatment assignment plots ####
   output$assignment_plot <-  renderPlot({
  
     req(input$get_prob)
     data.frame(treatment =  prob_values$treatment, time = prob_values$data$time_pt) %>%
        group_by(time) %>% 
        summarise(perc_treatment = mean(treatment, na.rm=T)) %>%
       ggplot(aes(x=time, y = perc_treatment)) +
       geom_line(color="red") +
       labs(x = "Decision point in trial", y = "(%) of participants given treatment",
            title = "Treatment assignment throughout trial") +
       theme_classic()
   })

    }
)
