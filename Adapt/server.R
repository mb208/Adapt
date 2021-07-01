library(shiny)



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
   
   sim_params <-  reactiveValues(
      
      num_vars = 1 ,
      sim_data = NULL

   )
   
   
   data_gen_modal <- modalDialog(
      fluidPage(
         shinyjs::useShinyjs(),
                fluidRow(column(1,
                                numericInput("n_participants", 
                                             "Number of Participants",
                                             value = 20))),
         column(3, 
            uiOutput("var_title"),
            shinyjs::hidden(
               radioButtons(
                  "independ_dist",
                  label = "Are the new variable and previous variables\n independently distributed?",
                  choices = c("Yes", "No")
               )),
            shinyjs::hidden(
               selectizeInput(
               inputId = 'conditional_vars',
               label = "Choose Variables",
               list(),
               options = list(maxItems = 1,
                              placeholder = "select variables",
                              onInitialize = I('function() { this.setValue(0); }'))
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
         tableOutput("sim_data")
         )
      ,
      title = "Simulate Dataset"
      ,
      size = "l" # made modal window large


                )
   
   # Display name of new variable
   output$var_title <- renderUI({
      tags$div(h3(strong(paste("Generating X", sim_params$num_vars, sep = ""))))
   })
   
   
   # Display required parameters for selected distribution
   output$dist_params <-  renderUI({
      
      get_dist_ui(tolower(input$data_dist))
   })
   
   
   observeEvent(input$gen_var, {
      
      
      
      n_participants <- input$n_participants
      
      ## If new variable is indpendpent (or first variable) then draw samples
      if (input$independ_dist == "Yes") {
         new_var <- switch (input$data_dist,
            "Gaussian" = rnorm(n_participants,
                               mean = input$guass_mu,
                               sd = input$guass_sd) ,
            "Bernoulli" = rbernoulli(n_participants, p = input$bern_p),
            
            "Binomial" = rbinom(n_participants, size = input$bin_n, 
                                p = input$bin_p),
            
            "Gamma" = rgamma(n_participants,
                             shape = input$gamma_s, 
                             rate = input$gamma_r) 
            )
      }
      
      if (sim_params$num_vars == 1) {
         
         sim_params$sim_data <- data.frame("X1" = new_var)
      } else {
         
         sim_var_name = paste("X", sim_params$num_vars, sep = "")
         sim_params$sim_data[sim_var_name] <- new_var
      }
      
      sim_params$num_vars = sim_params$num_vars  + 1
      
      updateRadioButtons(
         session,
         "independ_dist",
         label = "Are the new variable and previous variables\n independently distributed?",
         choices = c("Yes", "No"),
         selected = character(0)
      )
      
   })
   
   output$sim_data <- renderTable({head(sim_params$sim_data)})
   
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
   
   observeEvent(input$independ_dist, {
       if (sim_params$num_vars > 1) {  
         if (input$independ_dist == "Yes") {
            shinyjs::show("data_dist")
            shinyjs::show("dist_params")
         } else {
            shinyjs::hide("data_dist")
            shinyjs::hide("dist_params")
         }
          }
   }, ignoreInit = TRUE)  
   

   observeEvent(input$data_choice, {
      if (input$data_choice == "Simulate") {
         
         showModal(data_gen_modal)
         
      }
   }, ignoreInit = TRUE)
  
   covariates <- reactive({
    
       prob_values$data %>% 
        select(prob_values$variable_choices[input$calc_vars])
    
  })
  
   
   ##### Calculations for Randomization Design ####
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
      new_var_disp = paste0('Z', prob_values$agg_cnt)
      
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
   output$cnt <-  renderText({ prob_values$agg_cnt})

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
