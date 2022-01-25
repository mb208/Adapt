library(shiny)
library(shinyjs)
library(tidyverse)
library(shinydashboard)


# library("R/utils_server.R")

randomizationProb_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3,
             
             h4('Choose Variables'), 
             selectInput(ns('calc_vars'), 
                         label = "",
                         list(), 
                         multiple=TRUE, 
                         selectize=TRUE),
             dataProc_UI(ns("data_proc")),
             prob_map_UI(ns("prob_map")),
             shinyjs::disabled(actionButton(ns("apply_aggs"),label = "Apply Sequence"))
      ), # end of column 1
      column(1),
      column(6,
             fluidRow(box(title="Probability generation ", style="width:400px;",
                          tags$table(border = 2, 
                                     tags$thead(
                                       tags$tr(
                                         tags$th(colspan = 4, height = 50, width = 500, 
                                                 "Data Transformation Steps", style = 'text-align: center')
                                       )
                                     ), 
                                     tags$tbody(
                                       tags$tr(id = "agg-table-body",
                                               tags$td(align = "center", strong("Variables")),
                                               tags$td(align = "center", strong("1st Method")),
                                               tags$td(align = "center", strong("2nd Method")),
                                               tags$td(align = "center", strong("Variable Name"))
                                       )
                                     )
                          )
                          ,
                          set_html_breaks(2),
                          shinyjs::disabled(actionButton(ns("get_prob"),label = "Assign Treatment"))
                          
             )
             )
      )
      
    ),
    fluidRow(
      column(4),
      column(4,
             tags$div(id = "prb-plt", 
                      class = "asn-plt",
                      plotOutput(ns("prob_plot"), width = "auto"),
                      tags$script(HTML(paste0("$('#", ns("apply_aggs"), "').click(function(){
                                            $('#prb-plt').css('visibility', 'visible')  ;
                                        })")
                      ))
                      
             )),
      column(4,
             tags$div(id = "treat-plt",
                      class = "asn-plt",
                      plotOutput(ns("assignment_plot"), width = "auto"),
                      tags$script(HTML(paste0("$('#", ns("get_prob"), "').click(function(){
                                        $('#treat-plt').css('visibility', 'visible')  ;
                                    })")
                      ))
             )
             )
      )
    )
  
}


randomizationProb_Server <- function(id, X, reset=NULL) {
  moduleServer(id,
               function(input, output, session){
                 ns <- session$ns
                 
                 var_cnt <- reactiveVal()

                 var_choices <- reactiveVal()

                 r_probs <- reactiveVal()
                 curr_data <- reactiveVal()
                 return_data <- reactiveVal()
                 treatment <- reactiveVal()

                 observe({
                   curr_data(X())
                   return_data(X())
                   
                   var_cnt(0)
                   r_probs(0)

                   var_choices__ <- names(X())
                   names(var_choices__) <- var_choices__
                   var_choices(var_choices__)
                 })

                observe({
                  updateSelectInput(
                    session = session,
                    inputId = 'calc_vars',
                    choices = names(var_choices()))
                })


                 selected_vars <- reactive({
                   req(input$calc_vars)
                   curr_data() %>% dplyr::select(var_choices()[input$calc_vars])
                 })


                 agg <- dataProc_Server("data_proc", X = selected_vars)

                 curr_probs <- prob_map_Server("prob_map", X = agg$data)

                 observe({
                   agg_name <- agg$agg_name()
                   prob_map <- curr_probs$prob_map()
                   if (agg_name != '' & prob_map != '') {
                     shinyjs::enable("apply_aggs")
                   } else {
                     shinyjs::disable("apply_aggs")
                   }
                 })

                 observe({
                   if (var_cnt() > 0) {
                     shinyjs::enable("get_prob")
                   } else {
                     shinyjs::disable("get_prob")
                   }
                 })

                 
                 # Update Inputs
                 observeEvent(input$apply_aggs, {

                   var_cnt(var_cnt() + 1)

                   # Update probabilities
                   r_probs((r_probs() + curr_probs$probs())/var_cnt())
                   
                   # Add randomization probabilities to data set that is returned.
                   return_data__ <- return_data()
                   return_data__$ran_prob <- r_probs()
                   return_data(return_data__)

                   # Add new variable
                   new_var_name = paste0('z', var_cnt(), '__')
                   new_var_disp = paste0('Synthetic ',var_cnt())

                   # Update date
                   curr_data__ <- curr_data()
                   curr_data__[new_var_name] <- r_probs()
                   curr_data(curr_data__)

                   # Update display names
                   var_choices__ <- var_choices()
                   var_choices__[new_var_disp] <- new_var_name
                   
                   var_choices(var_choices__)
                   
                   insertUI("table tbody",
                            where = "beforeEnd",
                            ui = tags$tr(class = "agg-seq",
                                         tags$td(align = "center", 
                                                 style = "word-wrap: break-word;",
                                                 paste0(input$calc_vars, collapse = ", ")),
                                         tags$td(align = "center", agg$agg_name()),
                                         tags$td(align = "center", curr_probs$prob_map()),
                                         tags$td(align = "center", new_var_disp)
                            )
                   )
                   }
                   )
                 
                 observeEvent(input$get_prob, {
                   treatment(rbernoulli(n = length(r_probs()),
                                        p = r_probs()))
                 })
                 
                 ### Assign treatment to participants ###
                 output$selected <-  renderTable({head(upload_data())})
                 output$cnt <-  renderText({ prob_values$agg_cnt })
                 
                 output$prob_plot <-  renderPlot({
                   req(input$apply_aggs)
                   data.frame(p =  r_probs()) %>%
                     ggplot(aes(x=p)) +
                     geom_histogram() +
                     labs(x = "Ranomization Probabilities", y = "Count",
                          title = "Histogram of Randomiztion Probabilities") +
                     theme_classic()
                 })
                 
                 # #### Treatment assignment plots ####
                 output$assignment_plot <-  renderPlot({
                   
                   req(input$get_prob)
                   data.frame(treatment = treatment(), time = curr_data()$time_pt) %>%
                     group_by(time) %>% 
                     summarise(perc_treatment = mean(treatment, na.rm=T)) %>%
                     ggplot(aes(x=time, y = perc_treatment)) +
                     geom_line(color="red") +
                     labs(x = "Decision point in trial", y = "(%) of participants given treatment",
                          title = "Treatment assignment throughout trial") +
                     theme_classic()
                 })
                 
                 return(list(agg = agg$agg_name,
                             prob_map=curr_probs$prob_map,
                             var_cnt = var_cnt,
                             curr_data = curr_data,
                             var_choices = var_choices
                             ))
                 
                 
               })
}

source("utils_server.R")
source("utils_ui.R")

source("mod_weighted_sum.R")
source("mod_dataProc_randomizationProb.R")
source("mod_probMap_randomizationProb.R")


ui <- fluidPage(
  useShinyjs(),
  mainPanel(actionButton("browser", "browser"),
            randomizationProb_UI("randomize")

  )

)


server <- function(input, output, session) {

  data <- cbind(data.frame(matrix(rnorm(300),ncol=3)), time_pt = c(1:100))
  result <- randomizationProb_Server("randomize",
                            X = reactive(data))
  observeEvent(input$browser,{
    browser()
  })

}

shinyApp(ui, server)