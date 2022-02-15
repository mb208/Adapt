library(shiny)
library(shinyjs)
library(tidyverse)
library(shinydashboard)

library(RMariaDB)
library(DBI)

# Personal files 
# source("R/utils_ui.R")

# library(DBI)
# con_sql <- dbConnect(RMariaDB::MariaDB(), user = "root", password = "Lalito", dbname = "adaptV0") 
# dbListTables(con_sql)
# 
# dbListFields(con_sql, "participants")


con_mariadb_UI <- function(id) {
  ns <- NS(id)
  tagList(
          h3(strong("Enter MariaDB credentials")),
          textInput(ns("user"), "User:"),
          textInput(ns("password"), "Password:"),
          textInput(ns("dbname"), "Database:"),
          textInput(ns("host"), "Host:"),
          set_html_breaks(1),
          shinyjs::disabled(actionButton(ns("conn"), "Connect"))
          )
}


con_mariadb_Server <- function(id) {
  moduleServer(id,
               function(input, output, session){
                 
                 connection <- reactiveVal()
                 
                 observe({
                  creds <- c("user", "password", "dbname")
                  credentialFilled <-
                    vapply(creds,
                           function(x) {
                             !is.null(input[[x]]) && input[[x]] != ""
                           },
                           logical(1))
                  credentialFilled <- all(credentialFilled)
                  shinyjs::toggleState(id = "conn", condition = credentialFilled)
                 })
                 
                 observe({
                   con_sql <- dbConnect(RMariaDB::MariaDB(), 
                                        user = input$user, 
                                        password = input$password,
                                        dbname = input$dbname,
                                        host = input$host) 
                   connection(con_sql)
                 }) %>% 
                   bindEvent(input$conn)
                 
                 return(connection)
                 
               })
}


source("utils_ui.R")

ui <- fluidPage(
  useShinyjs(),
  mainPanel(actionButton("browser", "browser"),
            con_mariadb_UI("mariaDB_conn")
  )
)

server <- function(input, output, session) {
    result <- con_mariadb_Server("mariaDB_conn")
    observeEvent(input$browser,{
      browser()
    })

  }

  shinyApp(ui, server)