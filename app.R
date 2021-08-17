# app.R

# Run shiny app
#options(shiny.reactlog = TRUE)
# addResourcePath(
#   # will be accessible at /files
#   prefix = "files", 
#   # path to the assets directory
#   directoryPath = "www"
# )

shiny::shinyAppDir("Adapt")
