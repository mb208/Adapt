library(shiny)
library(DT)
library(stringr)

#### Function to Apply different operators to input variable(s) ####


n_ary_operator <- function(operator, var) {
   ## Make sure there are at least 2 variables
   stopifnot(length(var) > 1)
   
   ## Make sure operator is in list of operators
   # stopifnot(operator %in% c("+", "-", "x", "/"))
   
   switch(operator,
          # "+" = reduce(var, `+`),
          # "-" = reduce(var, `-`),
          "multiply" = reduce(var, `*`),
          "divide" = reduce(var, `/`))
}

unary_operator <- function(operator, var, pow=1) {
   ## Coniditions for power to work
   stopifnot((operator == "^" & exists("pow")) || operator != "^")
   stopifnot((operator == "^" & is.numeric(pow)) || operator != "^")
   
   switch(operator,
          "exp" = exp(var),
          "ln" = log(var),
          "^" = var^pow,
          "None" = I(var)
   )
}


#### List mapping input arguments to functions ####

expit <- function(x) {
   exp(x) / (1 + exp(x))
}

arctan <- function(x) {
   (1 / pi) * atan(x) + 1 / 2
}

adj_tanh <- function(x) {
   .5 * (tanh(x) + 1)
}


data_agg <- list("sum" = sum,
                 "average" = mean,
                 "weighted average" = weighted.mean)

prob_maps <- list("expit" = expit,
                  "arctan" = arctan,
                  "tanh" = adj_tanh)



#### UI utilities ####

#### Creating list of UIs for distribution inputs ####
get_dist_ui <- function(dist) {
   #### Returns Shiny Input 
   switch(
      dist,
      "gaussian" = tags$div(
         h4(strong("Gaussian Parameterization")),
         numericInput("guass_mu", "Mean", value = 0),
         numericInput("guass_sd", "SD", value = 1)
      ),
      
      "bernoulli" = tags$div(
         h4(strong("Bernoulli Parameterization")),
         numericInput("bern_p", "Probability", value = .5)
      ),
      
      "binomial" = tags$div(
         h4(strong("Binomial Parameterization")),
         numericInput("bin_n", "Size", value = 5),
         numericInput("bin_p", "Probability", value = .5)
      ),
      "gamma"   =  tags$div(
         h4(strong("Gamma Parameterization")),
         numericInput("gamma_s", "Shape", value = 1),
         numericInput("gamma_r", "Rate", value =  1)
      )
      
   )
}


err_dist_ui <- function(dist) {
   
   switch (dist,
           "gaussian" = tags$div(
              h4(strong("Gaussian Error")),
              numericInput("guass_err_sd", 
                           "SD",
                           value = 1)
              ),
           "double exponential" = tags$div(
              h4(strong("Double Exponential Error")),
              numericInput("dexp_err_scale",
                           "Scale",
                           value = 1))
           )
}

create_btn <- function(x) {
   paste0(
      '<button class="btn btn-default action-button btn-info action_button" id="select_',
      x, 
      '" type="button" onclick=get_id(this.id)></button>'
   )
}
                           


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
   
   upload_data  <- reactive({
      req(input$file_info$name)
     
      ext <- tools::file_ext(input$file_info$datapath)
      validate(need(ext == "csv", "Please upload a csv file"))
         
      return(read.csv(input$file_info$datapath))
      
      })
   
   
# Set initial data values    
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
      
      expression_tbl = tibble(Select = character(0), 
                              Variables = character(0), 
                              Expression = character(0), 
                              Arguments = character(0), 
                              Name = character(0))
   )
   
   #### Model Dialog UI ####
   data_gen_modal <- modalDialog(
      fluidPage(
         shinyjs::useShinyjs(),
                
         column(3, 
          fluidRow(
               column(5,  numericInput("n_participants",
                                       "Number of Participants",
                                       value = 20)
                      )
                ),
          textInput("sim_var_name", "Enter name for variable:"),
            uiOutput("var_title"),
            shinyjs::hidden(
               radioButtons(
                  "independ_dist",
                  label = "Are the new variable and previous variables\n independently distributed?",
                  choices = c("Yes", "No")
               )),
            selectizeInput(
               inputId = 'data_dist',
               label = "Select Distribution",
               list("Gaussian",
                    "Bernoulli",
                    "Binomial",
                    "Gamma"),
               options = list(maxItems = 1,
                              placeholder = "select distribution",
                              onInitialize = I('function() { this.setValue(0); }'))
               ),
            uiOutput("dist_params"),
            actionButton("gen_var", "Generate")
         ),
         column(9,
                fluidRow(column(5,
                                shinyjs::hidden(
                                   tags$div(id = "cond_dist_title",
                                            h3(strong("Constructing Conditional Distribution")))
                                )
                )),
                column(2,
                       shinyjs::hidden(
                          tags$div(id="loc_scale_column",
                                 uiOutput("param_specification"),  # Are we specifying the mean, var, or error
                                 tags$div(id = "cond_operations",
                                          selectInput(
                                             inputId = 'conditional_vars',
                                             label = "Choose Variables",
                                             list(),
                                             multiple=TRUE, 
                                             selectize=TRUE
                                          ),
                                          shinyjs::hidden(selectizeInput(
                                             inputId = 'multi_operation',
                                             label = "Apply operation to chosen variables",
                                             list(
                                                "weighted sum",
                                                "multiply",
                                                "divide"),
                                             options = list(maxItems = 1,
                                                            placeholder = "select operation",
                                                            onInitialize = I('function() { this.setValue(0); }'))
                                          )),
                                          shinyjs::hidden(
                                             uiOutput("weighted_sum_inputs")
                                          ),
                                          shinyjs::hidden(selectizeInput(
                                             inputId = 'unary_operation',
                                             label = "Apply operation to chosen variable",
                                             c("exp","ln", "^", "None"),
                                             options = list(maxItems = 1,
                                                            placeholder = "select operation",
                                                            onInitialize = I('function() { this.setValue(0); }'))
                                          )),
                                          shinyjs::hidden(
                                             numericInput("power_val", "Exponent Value:", value = 1)
                                             ),
                                          actionButton("apply_operation", "Apply Operations")
                                          )
                                 ,
                                 shinyjs::hidden(
                                    selectizeInput(
                                       inputId = 'error_dist',
                                       label = "Select Error Distribution",
                                       list("Gaussian",
                                            "Double Exponential"),
                                       options = list(maxItems = 1,
                                                      placeholder = "select distribution",
                                                      onInitialize = I('function() { this.setValue(0); }'))
                                    ),
                                    uiOutput("cond_err_dist")),
                                 shinyjs::hidden(actionButton("calc_error", "Generate Error")),
                                 br(),
                                 br(),
                                 br()
                       )))
                       ,
                column(3,
                      # tableOutput("sim_data"),
                       textOutput("var_creation_warning")
                ),
                column(4,
                       # shinyjs::hidden(
                       #    tags$table(
                       #       id = "loc-scale-table",
                       #       border = 2,
                       #       tags$thead(tags$tr(
                       #          tags$th(
                       #             colspan = 5,
                       #             height = 50,
                       #             width = 600,
                       #             "Mean Generation",
                       #             style = 'text-align: center'
                       #          )
                       #       )),
                       #       tags$tbody(
                       #          id = "loc-scale-body",
                       #          tags$tr(
                       #             id = "loc-scale-row1",
                       #             tags$td(align = "center", strong("Select")),
                       #             tags$td(align = "center", strong("Variables")),
                       #             tags$td(align = "center", strong("Operation")),
                       #             tags$td(align = "center", strong("Weights")),
                       #             tags$td(align = "center", strong("Variable Name"))
                       #          )
                       #       )
                       # )
                       # ),
                       
                        shinyjs::hidden(
                           div(id = "loc_scale",
                               DT::dataTableOutput(outputId = "loc_scale_tbl"),
                               tags$script(
                               HTML("function get_id(clicked_id) {
                               Shiny.setInputValue('expr_row', 
                                 clicked_id.split('_')[1], 
                                 {priority: 'event'});
                                    }"))
                               )
                        ),
                       shinyjs::hidden(actionButton("calc_mean", "Generate Mean")),
                       shinyjs::hidden(actionButton("calc_variance", "Generate Variance"))

                       )
                )
         )
      ,
      title = "Simulate Dataset", 
      footer = actionButton("simulate_data", "Finish Simulation")
      ,
      size = "l" # made modal window large


                )
   
   # Display name of new variable
   output$var_title <- renderUI({
      req(input$sim_var_name)
      sim_name <- stringr::str_trim(input$sim_var_name) # Remove leading / trailing white space
      validate(
         need(!stringr::str_detect(sim_name, "^\\d"), "Name cannot start with digit."),
         need(!stringr::str_detect(sim_name, "^_"), "Name cannot start with '_'."),
         need(!stringr::str_detect(sim_name, "[[:space:]]"), "Name cannot contain spaces (replace with _)."),
         need(!stringr::str_detect(sim_name, "[^_[:^punct:]]"), "Name cannot contain punctuation."),
         need(!stringr::str_detect(sim_name, "[A-Z]"), "Name should be lower case."),
         need(!(sim_name %in% names(sim_params$sim_data)), "Name exists in data. Cannot have duplicate variable names.")
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
      
      ## If new variable is independent (or first variable) then draw samples
      if (input$independ_dist == "Yes" || sim_params$num_vars == 1) {
         new_var <- switch (
            input$data_dist,
            "Gaussian"  = rnorm(
               n_participants,
               mean = input$guass_mu,
               sd = input$guass_sd
            ),
            "Bernoulli" =  as.integer(rbernoulli(n_participants,
                                         p = input$bern_p)),
            
            "Binomial"  = rbinom(n_participants,
                                 size = input$bin_n,
                                 p = input$bin_p),
            
            "Gamma"     = rgamma(
               n_participants,
               shape = input$gamma_s,
               rate = input$gamma_r
            )
         )
      } else {
         sd <- sqrt(exp(sim_params$sim_variance))
         
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
      
      sim_params$num_vars = sim_params$num_vars  + 1
      
      ### Initialize variables for constructing conditional distribution
      sim_params$cond_dist_step <- 1
      sim_params$param_data <- sim_params$sim_data # Used for calculating conditional mean / var
      sim_params$var_cnt <- 1
      sim_params$var_choices <- names(sim_params$sim_data)
      names(sim_params$var_choices) <- sim_params$var_choices
      sim_params$expression_tbl = tibble(Select = character(0),
                                         Variables = character(0),
                                         Expression = character(0),
                                         Arguments = character(0),
                                         Name = character(0))
      
      sim_params$sim_mean <- NULL
      sim_params$sim_variance <- NULL
      sim_params$sim_error <- NULL
      
      shinyjs::reset("independ_dist")
      shinyjs::reset("sim_var_name")
      
      ## Updates select input with current simulated data variables
      ## Allows user to build conditional distribution for a new variable 
      ## using these previously generated variables
      ## Only displayed if "independ_dist" == "No"
      
      updateSelectInput(
         session,
         inputId = 'conditional_vars',
         label = "Choose Variables",
         names(sim_params$sim_data)
      )
      
   })
   
   output$sim_data <- renderTable({
      req(sim_params$sim_data)
      head(sim_params$sim_data)
      
      })
   
   
   ### !! I belive all this observe stuff should be done in javascript
   observe({
      # When number of variables in greater then 1
      # Give option to choose if new variable is dependent on previous
      
      if (sim_params$num_vars > 1 ) {
         shinyjs::disable("n_participants")
         shinyjs::show("independ_dist")
         shinyjs::hide("data_dist")
         shinyjs::hide("dist_params")
      }
      
   })
   
   #### Dynamic Display of operation choices for mean/var specification #### 
   
   observe({
      req(input$independ_dist)
      if (sim_params$num_vars > 1) {
         if (input$independ_dist == "Yes") {
            shinyjs::show("data_dist")
            shinyjs::show("dist_params")
            
            shinyjs::hide("cond_dist_title")
            shinyjs::hide("loc_scale_column")
            # shinyjs::hide("loc-scale-table")
            # shinyjs::hide("loc_scale_tbl")
            shinyjs::hide("loc_scale")
            shinyjs::hide("calc_mean")
         } else {
            shinyjs::hide("data_dist")
            shinyjs::hide("dist_params")
            
            shinyjs::show("cond_dist_title")
            shinyjs::show("loc_scale_column")
            # shinyjs::show("loc-scale-table")
            # shinyjs::show("loc_scale_tbl")
            shinyjs::show("calc_mean")
            shinyjs::show("loc_scale")
         }
      }
         
   })
   
   # logic for showing equation panels
   observe({
      if(sim_params$cond_dist_step==3) {
         shinyjs::hide("cond_operations")
      } else if (any(sim_params$cond_dist_step == c(1,2))) {
         shinyjs::show("cond_operations")
      }
   })
   
   observe({
      req(input$independ_dist)
      req(input$conditional_vars)
    
         if (input$conditional_vars[1] == "" || is.null(input$conditional_vars)) {
            shinyjs::hide("unary_operation")
            shinyjs::hide("multi_operation")
            shinyjs::hide("power_val")
         } else if (length(input$conditional_vars) == 1) {
            shinyjs::show("unary_operation")
            shinyjs::hide("multi_operation")
            # Need to specify because we don't want this to capture case where no variables are selected
         } else {
            shinyjs::hide("unary_operation")
            shinyjs::show("multi_operation")
         }
      
         if (input$unary_operation == "^") {
            shinyjs::show("power_val")
         } else {
            shinyjs::hide("power_val")
         }
      
   })
   
   observe({
      req(input$multi_operation)
      if (input$multi_operation == "weighted sum") {
         shinyjs::show("weighted_sum_inputs")
      } else {
         shinyjs::hide("weighted_sum_inputs")
      }
      
   })
   
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
                                   value = round(1/length(input$conditional_vars), 3))
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
      
      updateSelectInput(
         session,
         inputId = 'conditional_vars',
         label = "Choose Variables",
         names(sim_params$var_choices)
      )
      
      # check_input <- paste0('<input type="checkbox"/>')
      # insertUI("#loc-scale-body",
      #          where = "beforeEnd",
      #          ui = tags$tr(class = "loc-scale-build",
      #                       tags$td(HTML(check_input)),
      #                       tags$td(align = "center", 
      #                               style = "word-wrap: break-word;",
      #                               paste0(input$conditional_vars,
      #                                      collapse = ", ")),
      #                       tags$td(align = "center", operation_name),
      #                       tags$td(align = "center", input_val),
      #                       tags$td(align = "center", display_name)
      #          )
      # )
      
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
   })
   
    output$loc_scale_tbl <- DT::renderDataTable({
       datatable(sim_params$expression_tbl,
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
      sim_params$sim_mean <- sim_params$param_data[[col_id]]
      
      ### Initialize variables for constructing conditional distribution
      sim_params$cond_dist_step = 2
      sim_params$param_data <- sim_params$sim_data # Used for calculating conditional mean / var
      sim_params$var_cnt <- 1
      sim_params$var_choices <- names(sim_params$sim_data)
      names(sim_params$var_choices) <- sim_params$var_choices
      
      sim_params$expression_tbl = tibble(Select = character(0), 
                                         Variables = character(0), 
                                         Expression = character(0), 
                                         Arguments = character(0), 
                                         Name = character(0))
      
      
      updateSelectInput(
         session,
         inputId = 'conditional_vars',
         label = "Choose Variables",
         names(sim_params$var_choices)
      )
      
      shinyjs::reset("multi_operation")
      shinyjs::reset("unary_operation")
      
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
      sim_params$sim_variance <- sim_params$param_data[[col_id]]

      ### Initialize variables for constructing conditional distribution
      sim_params$cond_dist_step = 3
      sim_params$param_data <- sim_params$sim_data # Used for calculating conditional mean / var
      sim_params$var_cnt <- 1
      sim_params$var_choices <- names(sim_params$sim_data)
      names(sim_params$var_choices) <- sim_params$var_choices

      sim_params$expression_tbl = tibble(Select = character(0),
                                         Variables = character(0),
                                         Expression = character(0),
                                         Arguments = character(0),
                                         Name = character(0))
      
      updateSelectInput(
         session,
         inputId = 'conditional_vars',
         label = "Choose Variables",
         names(sim_params$var_choices)
      )
   
      shinyjs::reset("multi_operation")
      shinyjs::reset("unary_operation")
  
      shinyjs::hide("calc_variance")
      shinyjs::hide("loc_scale")
      
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
      sim_params$sim_error <- switch (
         tolower(input$error_dist),
         "gaussian" = rnorm(input$n_participants,
                            0,
                            sd = input$guass_err_sd),
         "double exponential" = nimble::rdexp(input$n_participants,
                                              0,
                                              input$dexp_err_scale)
      )
      
      
      shinyjs::hide("cond_err_dist")
      shinyjs::hide("error_dist")
      shinyjs::hide("calc_error")
   })

   observeEvent(input$simulate_data, {
      data <- sim_params$sim_data
      
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
      
      removeModal()
      
   })
   
   ## Open modal dialog to simulate data ##
   observeEvent(input$data_choice, {
      if (input$data_choice == "Simulate") {
         
         showModal(data_gen_modal)
         
      }
   }, ignoreInit = TRUE)
  
   
   
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
     data.frame(treatment =  prob_values$treatment) %>%
       mutate(teatment = factor(ifelse(treatment,"yes", "no"))) %>%
       ggplot(aes(x=treatment)) +
       geom_bar() +
       labs(x = "Assigned Treatment", y = "Count",
            title = "Histogram of Treatement Assignment") +
       theme_classic()
   })

    }
)
