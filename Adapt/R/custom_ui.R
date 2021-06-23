library(shiny)


draggable_card <- function(id, title, text, draggable = TRUE) {
  
  # validate args
  stopifnot(!is.null(id))
  stopifnot(!is.null(title))
  stopifnot(!is.null(text))
  stopifnot(is.logical(draggable))
  
  # create <svg> icon
  fill_color <- "#09BC8A"
  line_color <- "#ffffff"
  # build parent element: <div class="card">
  el <- tags$div(
    id = paste0("card-", id),
    class = "card",
    draggable = tolower(draggable),
    `data-value` = title,
    tags$h2(class = "card-title", title),
    tags$p(class = "card-message", text)
  )
  
  # remove <svg> element if draggable = FALSE
  if (!draggable) el$children[[2]] <- NULL
  
  # return
  return(el)
}

variableSelect <- function(inputId, label, choices, selected = NULL,
                      multiple = FALSE, size = NULL) {
  selectTag <- shiny::selectizeInput(
    inputId,
    label,
    choices,
    selected,
    multiple,
    size
  )
  
  # Modify tag
  selectTag$attribs$class <- NULL
  # Clean extra label class
  selectTag$children[[1]]$attribs$class <- NULL
  # Remove extra outer div
  selectTag$children[[2]] <- selectTag$children[[2]]$children[[1]]
  
  # Add good class for variable binding
  selectTag$children[[2]]$attribs$class <- if (is.null(size)) {
    "var-dropdown"
  } else {
    "var-list"
  }
  
  selectTag
}
