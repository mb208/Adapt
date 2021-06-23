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
source("R/custom_ui.R")

data <- read.csv("./data/test_data.csv")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme = shinytheme("sandstone"),
    tags$head(
        tags$link(rel = "stylesheet", type="text/css", href = "style.css"),
    ),
    # Application title
    titlePanel("Randomization probabilities"),
    column(2,
        tags$div(
            h4('Choose Variables'), 
            selectInput('calc_vars', 
                    label = "",
                    names(data), 
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
        ),
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
                                           tags$th(colspan = 3, height = 50, width = 600, 
                                                   "")
                                       )
                                   ), 
                                   tags$tbody(
                                       tags$tr(id = "agg-table-body",
                                               tags$td(align = "center", strong("Variables")),
                                               tags$td(align = "center", strong("1st Method")),
                                               tags$td(align = "center", strong("2nd Method"))
                                       )
                                   )
                        )
                        ,
                        tags$script(HTML("$('#apply_aggs').click(function(){ 
                             let X    = $('#calc_vars').val() ;
                             let aggf = $('#data_agg').val();
                             let pmap = $('#prob_map').val();
                             let html = `<tr class='agg-seq'><td align='center' style='word-wrap: break-word'>${X}</td><td align='center'>${aggf}</td><td align='center'>${pmap}</td></tr>`;
                             $('table tbody').append(html)})
                        "
                        )),
                        br(),
                        br(),
                        actionButton("get_prob",label = "Assign Treatment")
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
           # plotOutput("prob_plot", width = "auto"),
           # plotOutput("assignment_plot"),
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



