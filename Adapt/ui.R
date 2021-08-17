#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinythemes)

source("R/utils_ui.R")



# Define UI for application that draws a histogram
shinyUI(fluidPage(
    shinyjs::useShinyjs(),
    theme = shinytheme("sandstone"),
    tags$head(
        tags$style(HTML('

                        .modal-lg {
                        width: 1500px;
                        max-width: 1500px;

                        }
                        
                        .modal-body { 
                        min-height:700px;
                        }
                      ')),
        includeCSS("www/accordion.css"), 
        includeCSS("www/accordion.js"), 
        tags$script("MathJax = {
                      tex: {
                        inlineMath: [['$', '$']]
                      },
                      svg: {
                        fontCache: 'global'
                      }
                    };"), 
        tags$script(type="text/javascript", id="MathJax-script",
                    "async src"="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js"),
        tags$script(type="text/javascript", src = "www/accordion.js")
    ),
    # Application title
    titlePanel("Randomization Design"),
    actionButton("browser", "browser"),
    tags$script("$('#browser').hide();"),
    column(2,
        tags$div(
            h3(strong("Data Source")),
            h4("Generate data source by uploading an an existing file from your computer or simulating a data set from scratch"),
            actionButton("upload_file", "Upload Data"),
            actionButton("data_simulation", "Simulate"),
            br(),
         shinyjs::hidden(fileInput("file_info", 
                   "Upload File",
                   accept = ".csv")),
            h4('Choose Variables'), 
            selectInput('calc_vars', 
                    label = "",
                    list(), 
                    multiple=TRUE, 
                    selectize=TRUE)
        ),
        h4('Choose Data Transformation'),
        selectizeInput('data_agg', 
                             label = "",
                             list("sum",
                                  "average",
                                  "weighted average"),
                             options = list(maxItems = 1,
                                            placeholder = "select data transformation",
                                            onInitialize = I('function() { this.setValue(0); }')) 
                ),
        h4('Choose Probability Mapping'),
        selectizeInput('prob_map',
                       label = "",
                       list("expit", 
                            "tanh", 
                            "arctan"),
                       options = list(maxItems = 1,
                                      placeholder = "select probability generation",
                                      onInitialize = I('function() { this.setValue(0); }')) 
        ),
        actionButton("apply_aggs",label = "Apply Sequence")
        ), # end of column 1
    column(1, 
           offset = 0,
           style='padding:20px;',
           conditionalPanel(condition = "input.data_agg == 'weighted average'",
                            uiOutput("weighted_avg")
                            )
           ),
    column(5,
           fluidRow(box(title="Probability generation ",
                        tags$table(border = 2, 
                                   tags$thead(
                                       tags$tr(
                                           tags$th(colspan = 4, height = 50, width = 600, 
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
                        actionButton("get_prob",label = "Assign Treatment") 
                        # , 
                        # tableOutput("selected")
                        )
                    )
           ),
    column(3,
           tags$div(id = "prb-plt", 
                    class = "asn-plt",
                    plotOutput("prob_plot", width = "auto"),
                    tags$script(HTML("$('#apply_aggs').click(function(){
                                            $('#prb-plt').css('visibility', 'visible')  ;
                                        }) "
                    ))
                    ),
           tags$div(id = "treat-plt", 
                    class = "asn-plt",
                    plotOutput("assignment_plot", width = "auto"),
                    tags$script(HTML("$('#get_prob').click(function(){
                                        $('#treat-plt').css('visibility', 'visible')  ;
                                    })"
                    ))
                    ),
           actionButton("reset",label = "Reset"),
           tags$script(HTML("$('#reset').click(function(){
                                $('table tbody').find('.agg-seq').remove() ;
                                
                             }) ;
                            
                            $('#reset').click(function(){
                            $('.asn-plt').css('visibility', 'hidden') ;
                            });
                            "
           ))
           ),
    column(1)
    )
)



