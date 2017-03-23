# TODO: see if there's a way to return a reactive list instead of reactive items

#' @export
dome <- function() {
  ui <- fluidPage(
    selectInput("dataset", "Select dataset", c("iris", "mtcars")),
    uiOutput("mapcolumns")
  )
  
  server <- function(input, output, session) {
    mydata <- reactive({
      if (input$dataset == "iris") {
        iris
      } else {
        mtcars
      }
    })
    
    output$mapcolumns <- renderUI({
      mapcolumnsUI(data = mydata(),
                   names = c("dates", "name"),
                   multiple = c(T, F),
                   labels = c("Please provide the dates of onset",
                              "Which column corresponds to the name?")
      )
    })
    
    mynames <- mapcolumnsServer(
      data = mydata(),
      names = c("dates", "name"),
      multiple = c(T, F))
    
    observe({
      cat(str(mynames$dates()))
    })
    observe({
      cat(str(mynames$name()))
    })
      
  }
  
  runApp(shinyApp(ui = ui, server = server))
}

#' @export
#' @import shiny
mapcolumnsUI <- function(id = "mapcolumns", data, names, multiple = TRUE,
                         labels = NULL) {
  ns <- NS(id)
  
  if (missing(data)) {
    stop("'data' argument is required")
  }
  if (missing(names)) {
    stop("'names' argument is required")
  }

  multiple <- rep(multiple, length.out = length(names))

  if (is.null(labels)) {
    labels <- paste0("select ", names, " variable", ifelse(multiple, "s", ""))
  }
  if (length(labels) != length(names)) {
    stop("The length of the 'labels' vector must match the length of the 'names' vector")
  }
    
  div(
    lapply(seq_along(names), function(idx) {
      group_name <- names[idx]
      multi <- multiple[idx]
      label <- labels[idx]
      selectInput(
        ns(paste0("map_col_", group_name)),
        label, 
        colnames(data),
        multiple = multi
      )
    })
  )
}

#' @export
#' @import shiny
mapcolumnsServer <- function(id = "mapcolumns",
                             data, names, multiple = TRUE) {
  callModule(mapcolumnsServerHelper, id, data = data, names = names)
}

mapcolumnsServerHelper <- function(input, output, session, id,
                                   data, names) {
  return(
    setNames(
      lapply(names, function(name) {
        reactive(input[[paste0("map_col_", name)]])
      }),
      names
    )
  )
}