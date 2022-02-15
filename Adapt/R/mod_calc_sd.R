library(shiny)
library(shinyjs)
library(tidyverse)
library(DT)

# source("R/utils_server.R")
# source("R/mod_weighted_sum.R")
# source("R/mod_operation_warning.R")
# source("R/utils_ui.R")
# source("R/utils_latex_render.R")


calc_sd_UI <- function(id) {
  ns <- NS(id)
  tagList(h4(strong("Specify the log variance")),
          column(
            3,
            tags$div(
              h5("Click the button below if the variance does not depend on the data."),
              actionButton(ns("indep_var"), "Idependent Variance"),
              set_html_breaks(2),
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
              c("exp", "ln", "^", "lag", "None"),
              options = list(
                maxItems = 1,
                placeholder = "select operation",
                onInitialize = I('function() { this.setValue(0); }')
              )
            ),
            set_html_breaks(2),
            hidden(numericInput(ns("power_val"), "Exponent Value:", value = 1)),
            hidden(tags$div(id = ns("lag_inputs"), style = "display:inline-block",
                            fluidRow(
                              column(6, numericInput(ns("lag_val"), "Lag Value", value = 1)),
                              column(6,  selectizeInput(
                                inputId = ns('lag_unit'),
                                label = "Lag Time Unit",
                                c("decision point" = "time_pt", "day" = "day"),
                                options = list(maxItems = 1))
                              ))
            )
            ),
            operation_warning_UI(ns("operation_warning")),
            disabled(actionButton(ns("apply_operation"), "Apply Operations"))
          )
          ),
          column(1),
          column(
            6,
            id = "loc_scale",
            div(
              h4("Click on a row to select an expression for the standard deviation"),
              withMathJax(DT::dataTableOutput(outputId = ns("loc_scale_tbl"))),
              # tags$script(HTML(
              #   str_interp("function set_row_id(clicked_id) {
              #       var el = document.getElementById(clicked_id);
              #       if (el.checked) {
              #           Shiny.setInputValue('${id}-expr_row',
              #                           clicked_id.split('_')[1],
              #                           {priority: 'event'});
              #       } else {
              #           Shiny.setInputValue('${id}-expr_row',
              #                           null,
              #                           {priority: 'event'});
              #       }
              #     
              #     };
              #   function ckChange(el) {
              #       var ckName = document.getElementsByName(el.name);
              #       for (var i = 0, c; c = ckName[i]; i++) {
              #         c.disabled = !(!el.checked || c === el);
              #       }
              #     }
              #     
              #     function checkboxProperties(el) {
              #       set_row_id(el.id) ;
              #       ckChange(el) ;
              #     }")
              # ))
            )
            ,
            set_html_breaks(1),
            actionButton(ns("calc_var"), "Generate Variance")
            
  )
  )

}

calc_sd_Server <- function(id, data){
  moduleServer(id,
               function(input, output, session) {
                 
                 calculated_sd <- reactiveVal()
                 sd_latex <- reactiveVal()
                 
                 
                 expression_tbl <- reactiveVal()
                 var_cnt <- reactiveVal()
                 
                 var_choices <- reactiveVal()
                 
                 tex_var_names <- reactiveVal()
                 
                 expr_list <- reactiveVal()
                 
                 # create data set that will be modified throughout process
                 curr_data <- reactiveVal()
                 observe({
                   # Init parameters for sd calculation
                   
                   curr_data(data())
                   
                   # Initialize expression table for representing expressions used to calculate mean
                   expression_tbl(tibble(
                     Select = character(0),
                     Expression = character(0),
                     Name = character(0)
                   ))
                   
                   var_cnt(1)
                   
                   var_choices__ <- names(data())
                   names(var_choices__) <- var_choices__
                   var_choices(var_choices__)
                   
                   tex_var_names__ <- map_chr(names(data()), ~ tex_var_name(.))
                   names(tex_var_names__) <- var_choices__ # Latex name is indexed by dataset column name 
                   tex_var_names(tex_var_names__)
                   
                   expr_list(c())
                   
                   calculated_sd(NULL)
                   sd_latex(NULL)
                   
                 })
                 

                 observe({
                   updateSelectizeInput(session = session,
                                        inputId = "select_vars",
                                        choices = names(var_choices()))
                 })

                # call weights from weighted sum UI
                 weights <- wgt_sum_server(
                   "weights",
                   var_names = reactive(input$select_vars),
                   multi_operation = reactive(input$multi_operation)
                 )
                 
                 # Return warning if operation/data combination returns NAs
                 operation_warning_Server("operation_warning", data = reactive(curr_data() %>%
                                                                                 dplyr::select(var_choices()[input$select_vars])),
                                          unary_operation = reactive(input$unary_operation),
                                          exponent  = reactive(input$power_val))

                 observeEvent(input$apply_operation, {
                   curr_data__ <- curr_data()
                   varnames <- input$select_vars
                   vars <- curr_data__ %>%
                     dplyr::select(var_choices()[varnames])

                   new_var_name <- paste(
                     paste0("mean", "__", sep = ""),
                     var_cnt(),
                     sep = "")

                   weights <- NULL
                   pow <- NULL
                   lag_n <- NULL
                   lag_unit <- NULL

                   display_name <- paste("expression", var_cnt(), collapse = " ")

                   if (length(varnames) == 1) {

                     operation_name = input$unary_operation

                     ## If they choose to take power of variable
                     if (operation_name=="^") {

                       pow = input$power_val

                       new_var <- unary_operator(operation_name,
                                                 vars,
                                                 pow)

                     }  else if (operation_name=="lag") {
                       
                       lag_n = input$lag_val
                       lag_unit <- input$lag_unit
                       
                       n_day = max(curr_data__$decision_pt)
                       
                       if (lag_unit=="day") {
                         n <- lag_n*n_day
                       } else {
                         lag_unit <- "dec"
                         n <- lag_n
                       }
                       
                       new_var <- curr_data__ %>% 
                         group_by(pid) %>% 
                         mutate(lag = dplyr::lag(!!as.name(varnames), n)) %>% 
                         pull(lag)
                       
                       
                     }  else {

                       new_var <- unary_operator(operation_name,
                                                 vars)
                     }

                   } else if (length(varnames) > 1) {

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
                   tex_vars <- tex_var_names__[varnames]

                   tex <- func_to_tex(operation_name, X = tex_vars, pow = pow,
                                      n = lag_n, unit = lag_unit, weights = weights())


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
                   req(data())
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
                   req(data())
                   if (is.null(input$select_vars)) {
                     shinyjs::hide("multi_operation")
                     shinyjs::hide("unary_operation")
                     reset_unary_operation(session, 'unary_operation') # User built functions to reset these inputs
                     reset_multi_operation(session, 'multi_operation')
                   }
                   else if (length(input$select_vars) == 1) {
                     shinyjs::show("unary_operation")
                     shinyjs::hide("multi_operation")
                     reset_multi_operation(session, 'multi_operation')
                     # Need to specify because we don't want this to capture case where no variables are selected
                   } else {
                     shinyjs::hide("unary_operation")
                     shinyjs::show("multi_operation")
                     reset_unary_operation(session, 'unary_operation')
                   }
                 })

                 observe({
                   req(data())
                   if (input$unary_operation == "^") {
                     shinyjs::show("power_val")
                   } else {
                     shinyjs::hide("power_val")
                   }
                 })
                 
                 observe({
                   req(data())
                   if (input$unary_operation == "lag") {
                     shinyjs::show("lag_val")
                   } else {
                     shinyjs::hide("lag_val")
                   }
                 })
                 

                 # observeEvent(input$expr_row, {
                 observe({
                   req(data())
                   if (is.null(input$loc_scale_tbl_rows_selected)) {
                     shinyjs::disable("calc_var")
                   } else {
                     shinyjs::enable("calc_var")
                   }
                   })
           
                
                 observeEvent(input$indep_var, {
                   calculated_sd(rep(1, dim(curr_data())[1]))
                   sd_tex = render_tex_inline("\\sigma(X) = 1")
                   sd_latex(sd_tex)
                 })
                 
                 observeEvent(input$calc_var, {
                   # row_id <- as.numeric(input$expr_row) ## set in javascript code
                   row_id <- as.numeric(input$loc_scale_tbl_rows_selected)

                   # We extract the variable display name from the table using row_id
                   # Using this we get the column name from var_choices
                   # With this we can extract the appropriate column for the mean
                   
                   col_id <- var_choices()[[expression_tbl()[row_id, "Name", drop=T]]]
                   sim_var<- curr_data()[[col_id]]
                   calculated_sd(sqrt(exp(sim_var))) # getting sd from log variance
                   
                   

                   ### Initialize variables for constructing conditional distribution
                   # Preivously used to reset parameters that were also used for variance calculations
                   # sim_params <- update_sim_params(sim_params, cond_dist_step = 2,
                   #                                 sim_params$num_vars, sim_data = sim_params$sim_data, mean=sim_mean)


                   # updateConditionalVars(session=session, names = names(sim_params$var_choices))
                   
              
                   # Preivously used to reset parameters that were also used for variance latex
                   # tex_params <-  reset_expr_params(tex_params,
                   #                                  tex_params$var_names[names(sim_params$var_choices)]
                   # )

                   sd_tex <- expr_list()[row_id] %>% unname() %>%
                     str_c("\\sigma(X) = \\sqrt{e^{", ., "}}", collapse = "") %>%
                     render_tex_inline()
                   
                   sd_latex(sd_tex)
                   
                   
                  
                   # shinyjs::reset("multi_operation")
                   # shinyjs::reset("unary_operation")

                 })

               return(list(calculated_sd = calculated_sd,
                           sd_latex = sd_latex,
                           expr_list = expr_list))

                 }
  )
}


# source("utils_server.R")
# source("utils_ui.R")
# source("utils_latex_render.R")
# source("mod_weighted_sum.R")
# source("mod_operation_warning.R")
# 
# ui <- fluidPage(
#   useShinyjs(),
#   withMathJax(),
#   mainPanel(actionButton("browser", "browser"),
#             calc_sd_UI("calc_sd")
#   )
# )
# 
# 
# server <- function(input, output, session) {
# 
#   data <-data.frame(matrix(rnorm(300),ncol=3))
# 
#   result <- calc_sd_Server("calc_sd", reactive(data))
# 
#   observeEvent(input$browser,{
#     browser()
#   })
# 
# }
# 
# shinyApp(ui, server)