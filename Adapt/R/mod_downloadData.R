library(shiny)


dataDownload_UI <- function(id, label = "Download Data") {
  ns <- NS(id)
  tagList(
    downloadButton(ns("download"), label)
  )
}


dataDownload_Server <- function(id, df, file_name) {
  moduleServer(id,
               function(input, output, session) {
                 output$download <- downloadHandler(
                   filename = function() {
                     file_name
                   },
                   content = function(file) {
                     write.csv(df(), file, row.names = FALSE)
                   }
                 )
               })
}





# Example 
# ui <- fluidPage(
#   mainPanel(actionButton("browser", "browser"),
#             dataDownload_UI("sim_data")
#   )
# )
# 
# 
# server <- function(input, output, session) {
#   data <-data.frame(matrix(rnorm(300),ncol=3))
#   dataDownload_Server("sim_data", df = data, file_name = "simulated_data.csv")
#   
#   observeEvent(input$browser,{
#     browser()
#   })
#   
# }
# 
# shinyApp(ui, server)