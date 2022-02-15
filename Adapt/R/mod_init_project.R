library(shiny)
library(shinyjs)
library(tidyverse)
library(shinydashboard)

library(RMariaDB)
library(DBI)

# Personal files 
# source("R/utils_ui.R")

# Modules 
# source("R/mod_connect_mariadb.R")

init_project_UI <- function(id) {
  ns <- NS(id)
  tagList(
    column(3,
           set_html_breaks(1),
           textInput("proj_name", h3("Enter Project Name:"))
           ),
    column(2),
    column(9, 
           set_html_breaks(1),
           fluidRow(
             box(title = "", style="width:800px;", solidHeader = T,
                 status = "primary", color = "aqua",
                 column(4,
                        con_mariadb_UI(ns("mariaDB_conn"))),
                 column(1),
                 column(3,
                        set_html_breaks(3),
                        selectInput(
                          ns('table'),
                          label =  h5(strong("Select Table:")),
                          list(),
                          selectize = TRUE
                          )
                        )
                 ),
             ),
           fluidRow(
             column(5,
                    h4(strong("Specify Inputs for Project")),
                    selectInput(
                      ns('sel_covariates'),
                      label = "Choose Covariates",
                      list(),
                      multiple = TRUE,
                      selectize = TRUE
                    ),
                    selectInput(ns('sel_outcome'),
                                label = "Choose Outcome",
                                list(),
                                multiple = FALSE),
                    selectInput(ns('sel_action'),
                                label = "Specify actions",
                                list(),
                                multiple = FALSE)),
             column(3,
                    h4(strong("Specify Decision Points:"))
             ))
           )
  )
}

init_project_Server <- function(id) {
  moduleServer(id,
               function(input, output, session){
                 connection <- con_mariadb_Server("mariaDB_conn")
                 
                 observe({
                   req(connection())
                   tables = dbListTables(connection())
                   dbname = RMariaDB::dbGetInfo(connection())$dbname
                   updateSelectizeInput(session = session,
                                        label =  str_interp("Select Table from ${dbname}:") ,
                                        inputId = "table", 
                                        selected = character(0),
                                        choices = tables)
                   
                 })
                 
                 observe({
                   req(input$table != "")
                   fields <- dbListFields(connection(), "participants")
                   
                   updateSelectInput(
                     session = session,
                     'sel_covariates',
                     label = "Choose Covariates",
                     selected = character(0),
                     choices = fields
                   )
                   
                   updateSelectInput(
                     session = session,
                     'sel_outcome',
                     label = "Choose Outcome",
                     selected = character(0),
                     choices = fields
                   )
                   
                   updateSelectInput(
                     session = session,
                     'sel_action',
                     label = "Specify actions",
                     selected = character(0),
                     choices = fields
                   )
                 })
                 
                 return(connection)
               })
}


# Demo ----
source("utils_ui.R")

# Modules 
source("mod_connect_mariadb.R")

ui <- fluidPage(
   useShinyjs(),
  mainPanel(actionButton("browser", "browser"),
            init_project_UI("init_proj")
  )
)

server <- function(input, output, session) {
  result <- init_project_Server("init_proj")
  observeEvent(input$browser,{
    browser()
  })
  
}

shinyApp(ui, server)
