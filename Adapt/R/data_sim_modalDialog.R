source("R/utils_ui.R")

#### Model Dialog UI ####
data_gen_modal <- modalDialog(
  fluidPage(
    shinyjs::useShinyjs(),
           fluidRow(
             column(3,
                    numericInput("n_participants",
                                 "Number of Participants",
                                 value = 20)),
             column(3,
                    numericInput("decision_pts",
                                 "Decision points per day",
                                 value = 5)),
             column(3,
                    numericInput("n_days",
                                 "Length of trial (in days)",
                                 value = 40),
                    actionButton("browser2", "browser")
                    )
             # ,
             # actionButton("end_design", "End Variable Creation")
             ),
           hr(),
           fluidRow(
             
             column(3,
                    textInput("sim_var_name", "Enter name for variable:"),
                    uiOutput("var_title"),
                    shinyjs::hidden(
                      radioButtons(
                        "independ_dist",
                        label = "Are the new variable and previous variables\n independently distributed?",
                        choices = c("Yes", "No")
                      )),
                    radioButtons(
                      "time_varying",
                      label = "Does the variable vary with time?",
                      choices = c("Yes", "No")
                    ),
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
                   actionButton("gen_var", "Generate"),
                   tags$script("$('#gen_var').prop('disabled', true);")
                    ),
             shinyjs::hidden(
               column(9,
                      id = "loc_scale_column",
                      fluidRow(column(5,
                                      tags$div(id = "cond_dist_title",
                                               h3(strong("Constructing Conditional Distribution")))
                      )),
                      column(2,
                             tags$div(
                               uiOutput("param_specification"),  # Are we specifying the mean, var, or error
                               tags$div(id = "cond_operations",
                                        selectInput(
                                          inputId = 'conditional_vars',
                                          label = "Choose Variables",
                                          list(),
                                          multiple=TRUE, 
                                          selectize=TRUE
                                        ),
                                        selectizeInput(
                                          inputId = 'multi_operation',
                                          label = "Apply operation to chosen variables",
                                          list(
                                            "weighted sum",
                                            "multiply",
                                            "divide"),
                                          options = list(maxItems = 1,
                                                         placeholder = "select operation",
                                                         onInitialize = I('function() { this.setValue(0); }'))
                                        ),
                                        uiOutput("weighted_sum_inputs")
                                        ,
                                        selectizeInput(
                                          inputId = 'unary_operation',
                                          label = "Apply operation to chosen variable",
                                          c("exp","ln", "^", "None"),
                                          options = list(maxItems = 1,
                                                         placeholder = "select operation",
                                                         onInitialize = I('function() { this.setValue(0); }'))
                                        ),
                                        numericInput("power_val", "Exponent Value:", value = 1),
                                        textOutput("var_creation_warning"),
                                        actionButton("apply_operation", "Apply Operations")
                               )
                               ,
                               tags$div(id = "error_calc",
                                        selectizeInput(
                                          inputId = 'error_dist',
                                          label = "Select Error Distribution",
                                          list("Gaussian",
                                               "Double Exponential"),
                                          options = list(maxItems = 1,
                                                         placeholder = "select distribution",
                                                         onInitialize = I('function() { this.setValue(0); }'))
                                        ),
                                        uiOutput("cond_err_dist"),
                                        actionButton("calc_error", "Generate Error")),
                               set_html_breaks(3),
                             ))
                      ,
                      column(1), 
                      column(6,
                             id = "loc_scale",
                             div(
                               DT::dataTableOutput(outputId = "loc_scale_tbl"),
                               tags$script(
                                 HTML("function get_id(clicked_id) {
                            Shiny.setInputValue('expr_row', 
                              clicked_id.split('_')[1], 
                              {priority: 'event'});
                                 }"))
                             )
                             ,
                             shinyjs::hidden(actionButton("calc_mean", "Generate Mean")),
                             shinyjs::hidden(actionButton("calc_variance", "Generate Variance"))
                             
                      )
               ) # End of loc_scale_column / closing of shinyjs hidden
             )
                    
           )
  )
  ,
  title = "Simulate Dataset", 
  footer = actionButton("simulate_data", "Finish Simulation")
  ,
  size = "l" # made modal window large
  
  
)