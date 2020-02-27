library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(6, actionButton('addFilter', 'Add filter')),
        offset = 6
      ),
      tags$hr(),
      tags$div(id = 'placeholderAddRemFilt'),
      tags$div(id = 'placeholderFilter'),
      width = 4 # sidebar
    ),
    mainPanel(
      tableOutput("data")
    )
  )
)

server <- function(input, output,session) {
  filter <- character(0)
  
  makeReactiveBinding("aggregFilterObserver")
  aggregFilterObserver <- list()
  
  observeEvent(input$addFilter, {
    add <- input$addFilter
    filterId <- paste0('Filter_', add)
    colfilterId <- paste0('Col_Filter_', add)
    rowfilterId <- paste0('Row_Filter_', add)
    removeFilterId <- paste0('Remove_Filter_', add)
    headers <- names(mtcars)
    insertUI(
      selector = '#placeholderFilter',
      ui = tags$div(id = filterId,
                    actionButton(removeFilterId, label = "Remove filter", style = "float: right;"),
                    selectInput(colfilterId, label = "Some Filter", choices = as.list(headers), selected = 1),
                    checkboxGroupInput(rowfilterId, label = "Select variable values",
                                       choices = NULL, selected = NULL, width = 4000)
      )
    )
    
    observeEvent(input[[colfilterId]], {
      
      col <- input[[colfilterId]]
      values <- as.list(unique(mtcars[col]))[[1]]
      
      updateCheckboxGroupInput(session, rowfilterId , label = "Select variable    values", 
                               choices = values, selected = values, inline = TRUE)
      
      aggregFilterObserver[[filterId]]$col <<- col
      aggregFilterObserver[[filterId]]$rows <<- NULL
    })
    
    observeEvent(input[[rowfilterId]], {
      
      rows <- input[[rowfilterId]]
      
      aggregFilterObserver[[filterId]]$rows <<- rows
      
    })
    
    observeEvent(input[[removeFilterId]], {
      removeUI(selector = paste0('#', filterId))
      
      aggregFilterObserver[[filterId]] <<- NULL
      
    })
  })
  
  output$data <- renderTable({
    
    dataSet <- mtcars
    
    invisible(lapply(aggregFilterObserver, function(filter){
      
      dataSet <<- dataSet[which(!(dataSet[[filter$col]] %in% filter$rows)), ]
      
    }))
    
    dataSet
  })
}

shinyApp(ui = ui, server = server)