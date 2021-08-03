library(reactable)
library(shiny)



data_dict <- function(output, summary_tables) {
  output$data_dict <- renderReactable({
    reactable(summary_tables$data_dict,
              searchable = F,
              showSortable = F
              # ,
              #
              # details = function(index) {
              #   if (summary_tables$data_dict$Independent[index] == "No") {
              #     reactable()
              #   }
              #
              # }
    )           
  })
  }