library(shiny)
fluidRow(
  column(
    3,
    textInput("sim_var_name", "Enter name for variable:"),
    uiOutput("var_title"),
    shinyjs::hidden(
      radioButtons(
        "independ_dist",
        label = "Are the new variable and previous variables\n independently distributed?",
        choices = c( "Not selected" = "", "Yes", "No")
      )
    ),
    selectizeInput(
      "constant_time",
      label = "At what interval does the variable change?",
      choices = c("Each decision point" = "dec_pt", 
                  "Daily" = "daily",
                  "Constant over entire trial" = "trial")
    ),
    selectizeInput(
      inputId = 'data_dist',
      label = "Select Distribution",
      list("Gaussian",
           "Bernoulli",
           "Binomial",
           "Gamma"),
      options = list(
        maxItems = 1,
        placeholder = "select distribution",
        onInitialize = I('function() { this.setValue(0); }')
      )
    ),
    uiOutput("dist_params"),
    actionButton("gen_var", "Generate"),
    tags$script("$('#gen_var').prop('disabled', true);")
  ),
  shinyjs::hidden(
    column(
      9,
      id = "loc_scale_column",
      fluidRow(column(5,
                      tags$div(id = "cond_dist_title",
                               h3(
                                 strong("Constructing Conditional Distribution")
                               )))),
      column(
        2,
        tags$div(
          uiOutput("param_specification"),
          # Are we specifying the mean, var, or error
          tags$div(
            id = "cond_operations",
            selectInput(
              inputId = 'conditional_vars',
              label = "Choose Variables",
              list(),
              multiple = TRUE,
              selectize = TRUE
            ),
            selectizeInput(
              inputId = 'multi_operation',
              label = "Apply operation to chosen variables",
              list("weighted sum",
                   "multiply",
                   "divide"),
              options = list(
                maxItems = 1,
                placeholder = "select operation",
                onInitialize = I('function() { this.setValue(0); }')
              )
            ),
            uiOutput("weighted_sum_inputs")
            ,
            selectizeInput(
              inputId = 'unary_operation',
              label = "Apply operation to chosen variable",
              c("exp", "ln", "^", "None"),
              options = list(
                maxItems = 1,
                placeholder = "select operation",
                onInitialize = I('function() { this.setValue(0); }')
              )
            ),
            numericInput("power_val", "Exponent Value:", value = 1),
            textOutput("var_creation_warning"),
            actionButton("apply_operation", "Apply Operations"),
            tags$script("$('#apply_operation').prop('disabled', true);")
          )
          ,
          tags$div(
            id = "error_calc",
            selectizeInput(
              inputId = 'error_dist',
              label = "Select Error Distribution",
              list("Gaussian",
                   "Double Exponential"),
              options = list(
                maxItems = 1,
                placeholder = "select distribution",
                onInitialize = I('function() { this.setValue(0); }')
              )
            ),
            uiOutput("cond_err_dist"),
            actionButton("calc_error", "Generate Error")
          ),
          set_html_breaks(3),
        )
      )
      ,
      column(1),
      column(
        6,
        id = "loc_scale",
        div(
          DT::dataTableOutput(outputId = "loc_scale_tbl"),
          includeScript("www/dataTableUtils.js")
        )
        ,
        shinyjs::hidden(actionButton("calc_mean", "Generate Mean")),
        shinyjs::hidden(actionButton("calc_variance", "Generate Variance")),
        tags$script("$('#calc_mean').prop('disabled', true);"),
        tags$script("$('#calc_variance').prop('disabled', true);")
        
      )
    ) # End of loc_scale_column / closing of shinyjs hidden
  )
)
