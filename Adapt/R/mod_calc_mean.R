library(shiny)
library(shinyjs)
library(tidyverse)
library(DT)

# source("R/utils_server.R")
# source("R/mod_weighted_sum.R")
# source("R/utils_ui.R")
# source("R/utils_latex_render.R")

calc_mean_ui <- function(id) {
  ns <- NS(id)
  tagList(h4(strong("Specify the mean")),
          column(
            2,
            tags$div(
              selectInput(
              inputId = ns('select_vars'),
              label = "Choose Variables",
              list(),
              multiple = TRUE,
              selectize = TRUE
            ),
            selectizeInput(
              inputId = ns('multi_operation'),
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
            wgt_sum_ui(ns("weights")),
            selectizeInput(
              inputId = ns('unary_operation'),
              label = "Apply operation to chosen variable",
              c("exp", "ln", "^", "None"),
              options = list(
                maxItems = 1,
                placeholder = "select operation",
                onInitialize = I('function() { this.setValue(0); }')
              )
            ),
            hidden(numericInput(ns("power_val"), "Exponent Value:", value = 1)),
            textOutput(ns("var_creation_warning")),
            disabled(actionButton(ns("apply_operation"), "Apply Operations"))
          )
          ),
          column(1),
          column(
            6,
            id = "loc_scale",
            div(
              withMathJax(DT::dataTableOutput(outputId = ns("loc_scale_tbl"))),
              includeScript("../www/dataTableUtils.js")
            )
            ,
            actionButton(ns("calc_mean"), "Generate Mean")
            
  )
  )

}

calc_mean_server <- function(id, data, expr_row){
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 # Initialize expression table for representing expressions used to calculate mean ----
                 expression_tbl <- reactiveVal(tibble(
                   Select = character(0),
                   Expression = character(0),
                   Name = character(0)
                 ))

                 calculated_mean <- reactiveVal()
                 mean_latex <- reactiveVal()

                 var_cnt <- reactiveVal(1)

                 var_choices <- reactiveVal()

                 observe({
                   # Use `isolate()` here to break dependency with data(). So this operation is only down at onset.
                   isolate({
                     var_choices__ <- names(data())
                     names(var_choices__) <- var_choices__
                     var_choices(var_choices__)
                   })
                 })

                 tex_var_names <- reactiveVal()

                 observe({
                   # Use `isolate()` here to break dependency with data(). So this operation is only down at onset.
                   isolate({
                     tex_var_names__ <- names(data())
                     names(tex_var_names__) <- tex_var_names__
                     tex_var_names(tex_var_names__)
                   })
                 })

                 expr_list <- reactiveVal(c())

                 # create data set that will be modified throughout process
                 curr_data <- reactiveVal()
                 observe(isolate(curr_data(data())))

                 observe({
                   updateSelectizeInput(session = session,
                                        inputId = "select_vars",
                                        choices = names(var_choices()))
                 })

               #! Note: Not sure if isolate is the way to go here. If we see that the
               # variable choices etc are not updating with new iterations of calc mean
               # then this is the place to start.

                # call weights from weighted sum UI
                 weights <- wgt_sum_server(
                   "weights",
                   var_names = reactive(input$select_vars),
                   multi_operation = reactive(input$multi_operation)
                 )

                 observeEvent(input$apply_operation, {
                   curr_data__ <- curr_data()
                   vars <- curr_data__ %>%
                     dplyr::select(var_choices()[input$select_vars])

                   new_var_name <- paste(
                     paste0("mean", "__", sep = ""),
                     var_cnt(),
                     sep = "")

                   weights <- NULL
                   pow <- NULL

                   display_name <- paste("expression", var_cnt(), collapse = " ")

                   if (length(input$select_vars) == 1) {

                     operation_name = input$unary_operation

                     ## If they choose to take power of variable
                     if (operation_name=="^") {

                       pow = input$power_val

                       new_var <- unary_operator(operation_name,
                                                 vars,
                                                 pow)

                     } else {

                       new_var <- unary_operator(operation_name,
                                                 vars)
                     }

                   } else if (length(input$select_vars) > 1) {

                     operation_name = input$multi_operation
                     # Logic changes if they choose weighted sum
                     if (input$multi_operation == "weighted sum") {
                       new_var <- apply(vars, 1,
                                        function(x) {
                                          sum(x*weights())
                                        }
                       )

                     } else {
                       new_var <- apply(vars, 1,
                                        function(x) {
                                          n_ary_operator(operation_name, x)
                                        }
                       )
                     }
                   }

                   # Update variable display names
                   var_choices__ = var_choices()
                   var_choices__[display_name] = new_var_name
                   var_choices(var_choices__)

                   # Update data set
                   curr_data__[new_var_name] = new_var
                   curr_data(curr_data__)

                   # Generate expression for data dict
                   tex_var_names__ <- tex_var_names()
                   tex_vars <- tex_var_names__[input$select_vars]

                   tex <- func_to_tex(operation_name, X = tex_vars, pow = pow, weights = weights())


                   tex_var_names__[display_name] <- tex
                   tex_var_names(tex_var_names__)

                   expr_list(c(expr_list(), tex))

                   expression_tbl <- expression_tbl()  %>%
                     dplyr::bind_rows(
                       tibble(
                         Select = create_checkbox(var_cnt()),
                         Expression = gen_MathJax_html(tex),
                         Name = display_name
                       )
                     ) %>%
                     expression_tbl()

                   # Need to think about how to get previous text expression involved
                   # sim_params$tex_expr <- expr_to_tex()

                   var_cnt(var_cnt() + 1)



                   shinyjs::reset("multi_operation")
                   shinyjs::reset("unary_operation")
                   shinyjs::disable("apply_operation")
                 })


                 output$loc_scale_tbl <- DT::renderDataTable({
                   datatable(expression_tbl(),
                              options = list(dom="t",
                                             paging = FALSE,
                                             ordering = FALSE),
                              escape = F,
                              rownames = F,
                              selection = 'single')
                 })


                 # logic for displaying operation choices ----
                 observe({
                   var_selected <- input$select_vars
                   unary <- input$unary_operation == ""
                   multi <- input$multi_operation == ""

                   if (is.null(var_selected)) {
                     shinyjs::disable("apply_operation")
                   } else if (length(var_selected) == 1 & !unary) {
                     shinyjs::enable("apply_operation")
                   } else if (length(var_selected) > 1 & !multi) {
                     shinyjs::enable("apply_operation")
                   } else {
                     shinyjs::disable("apply_operation")
                   }
                 })

                 observe({
                   if (is.null(input$select_vars)) {
                     shinyjs::hide("multi_operation")
                     shinyjs::hide("unary_operation")
                     shinyjs::reset("multi_operation")
                     shinyjs::reset("unary_operation")
                   }
                   else if (length(input$select_vars) == 1) {
                     shinyjs::show("unary_operation")
                     shinyjs::hide("multi_operation")
                     shinyjs::reset("multi_operation")
                     # Need to specify because we don't want this to capture case where no variables are selected
                   } else {
                     shinyjs::hide("unary_operation")
                     shinyjs::show("multi_operation")
                     shinyjs::reset("unary_operation")
                   }
                 })

                 observe({
                   if (input$unary_operation == "^") {
                     shinyjs::show("power_val")
                   } else {
                     shinyjs::hide("power_val")
                   }
                 })

                 # observeEvent(input$expr_row, {
                 observe({
                   if (is.null(expr_row())) {
                     shinyjs::disable("calc_mean")
                   } else {
                     shinyjs::enable("calc_mean")
                   }
                   
                   })

                 observeEvent(input$calc_mean, {
                   # row_id <- as.numeric(input$expr_row) ## set in javascript code
                   row_id <- as.numeric(expr_row()) ## set in javascript code

                   # We extract the variable display name from the table using row_id
                   # Using this we get the column name from var_choices
                   # With this we can extract the appropriate column for the mean

                   col_id <- var_choices()[[expression_tbl()[row_id, "Name", drop=T]]]
                   sim_mean <- curr_data()[[col_id]]
                   calculated_mean(sim_mean)

                   ### Initialize variables for constructing conditional distribution
                   # Preivously used to reset parameters that were also used for variance calculations
                   # sim_params <- update_sim_params(sim_params, cond_dist_step = 2,
                   #                                 sim_params$num_vars, sim_data = sim_params$sim_data, mean=sim_mean)


                   # updateConditionalVars(session=session, names = names(sim_params$var_choices))
                   mean_tex <- expr_list()[row_id] %>% unname() %>%
                     str_c("\\mu(X) = ", ., collapse = "") %>%
                     render_tex_inline()

                   # Preivously used to reset parameters that were also used for variance latex
                   # tex_params <-  reset_expr_params(tex_params,
                   #                                  tex_params$var_names[names(sim_params$var_choices)]
                   # )


                   mean_latex(mean_tex)

                   shinyjs::reset("multi_operation")
                   shinyjs::reset("unary_operation")

                 })

               return(list(calculated_mean = calculated_mean,
                           mean_latex = mean_latex,
                           expr_row = expr_row,
                           expr_list = expr_list))

                 }
  )
}


source("utils_server.R")
source("utils_ui.R")
source("utils_latex_render.R")
source("mod_weighted_sum.R")

ui <- fluidPage(
  useShinyjs(),
  withMathJax(),
  mainPanel(actionButton("browser", "browser"),
            calc_mean_ui("calc_mean")
  )
)


server <- function(input, output, session) {

  data <-data.frame(matrix(rnorm(300),ncol=3))

  result <- calc_mean_server("calc_mean", reactive(data), expr_row = reactive(input$expr_row))




  observeEvent(input$browser,{
    browser()
  })

}

shinyApp(ui, server)