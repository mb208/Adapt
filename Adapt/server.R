library(shiny)





data <- read.csv("./data/test_data.csv")

### Non reactive functions ###

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
   
   ###! Will change when data is user specified
   variable_choices <- names(data)
   names(variable_choices) <- variable_choices
   
   ### Initialize values for probability calculation ###
   prob_values <-  reactiveValues(
      
      agg_cnt = 0, 
      probs = numeric(length = nrow(data)), 
      treatment = numeric(length = nrow(data)),
      variable_choices = variable_choices,
      generated_variables = c(), 
      data = data
      
   )
   
  
  #### Dynamic UI components ####
  
  covariates <- reactive({
    
     prob_values$data %>% 
        select(unname(prob_values$variable_choices[input$calc_vars]))
    
  })
  
   # Here we use the input va
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
        print(var_wgts())
        print(head(covariates()))
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
      prob_values$probs =  numeric(length = nrow(data))
      
      # Remove newly generated variables from data set
      prob_values$data <- prob_values$data %>% 
         dplyr::select(-generated_variables)
      
      prob_values$variable_choices <- names(prob_values$data)
      names(prob_values$variable_choices) <- prob_values$variable_choices
      prob_values$generated_variables <-  0
      
      updateSelectInput(
         session = session,
         inputId = 'calc_vars',
         choices = names(prob_values$variable_choices))
 
   })
   
   ### Assign treatment to participants ###
   output$selected <-  renderText({names(covariates())})
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
