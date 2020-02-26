library(shiny)

ui <- fluidPage(
  fluidRow(
    column(6,
           textInput("mainDesc1", label = "Main Description", value = "", width = '100%')
    ),
    column(1,
           actionButton(inputId = 'insertBtn', label = "More")
    ),
    column(1,
           actionButton(inputId = 'removeBtn', label = "Less")
    )
  ),
  tags$div(id = 'placeholder'),
  fluidRow(column(12, verbatimTextOutput("value", placeholder = T)))
)

server <- function(input, output) {
  ## keep track of elements inserted and not yet removed
  vals <- reactiveValues(btn = 0)
  
  observeEvent(input$insertBtn, {
    vals$btn <- vals$btn + 1
    insertUI(
      selector = '#placeholder',
      ## wrap element in a div with id for ease of removal
      ui = tags$div(
        id = paste0('line', vals$btn),
        tags$p(fluidRow(
          column(6,
                 textInput(paste("mainDesc", vals$btn + 1, sep = ""), label = paste("Line", vals$btn), value = "", width = '100%')
          )
        )
        )
      )
    )
  })
  
  observeEvent(input$removeBtn, {
    removeUI(
      ## pass in appropriate div id
      selector = paste0('#line', vals$btn)
    )
    vals$btn <- vals$btn - 1
  })
  
  output$value <- renderText({ 
    msg <- c(input[["mainDesc1"]])
    if (vals$btn > 0) {
      for (i in 1:vals$btn) {
        msg <- c(msg, input[[paste0("mainDesc", i + 1)]])
      }
      msg <- paste(msg, collapse = "\n")  
    }
  })
}

shinyApp(ui = ui, server = server)