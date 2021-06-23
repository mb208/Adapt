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
  
  #### Dynamic UI components ####
  
  covariates <- reactive({
    
    data %>% select(input$calc_vars)
    
  })
  
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
     

  # Fill box with data transformation sequence   
   observeEvent(input$apply_aggs, {
     value <- paste0(input$calc_vars, collapse = " , ")
     value <- paste0(c(value, input$data_agg, input$prob_map),  collapse = " ; ")
     
     # Fill box with sequence
     # insertUI(
     #   selector = ".box-header",
     #   where = "afterEnd",
     #   tags$p(class="data-agg", value)
     # )
     
     # Reset select inputs 
     updateSelectInput(
       session = session,
       inputId = 'calc_vars',
       choices = names(data))
     
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
   
   
   #### Data Processing ####
   
   ### Data Aggregation ###
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
   
   ### Initialize values for probability calculation ###
   prob_values <-  reactiveValues(
     
     agg_cnt = 0, 
     probs = numeric(length = nrow(data)), 
     treatment = numeric(length = nrow(data))
     
   )
   
   
   ### Update probabilities each time a sequence is applied ###
   observeEvent(input$apply_aggs, {
     
     prob_values$agg_cnt = prob_values$agg_cnt + 1 
     prob_values$probs = (prob_values$probs + probabilities()) / prob_values$agg_cnt 
     
   })
   
   observeEvent(input$get_prob, {
     prob_values$treatment = rbernoulli(n = length(prob_values$probs),
                                        p = prob_values$probs)
     
   })
   
   observeEvent(input$reset, {
      
      prob_values$agg_cnt = 0
      prob_values$probs =  numeric(length = nrow(data))
 
   })
   
   ### Assign treatment to participants ###
   output$selected <-  renderText({ prob_values$probs})
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
