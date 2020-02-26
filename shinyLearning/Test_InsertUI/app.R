#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "red",
    dashboardHeader(title = "Personal Network"),
    
    dashboardSidebar(width = 75,
        sidebarMenu(
            menuItem("", tabName = "section-1", icon = icon("user-circle", "fa-3x")),
            menuItem("",tabName = "section-2", icon = icon("route", "fa-3x")),
            menuItem("",tabName = "section-3", icon = icon("comment", "fa-3x"))
        )
    ),
        
    dashboardBody(
        tabItems(
            tabItem(tabName = "section-1",
                    fluidRow(
                        box(title = "Getting user information", status="primary", solidHeader = TRUE, collapsible = TRUE, #background = "black",
                            textInput("q1", HTML("<strong>Question 1:</strong> <br> What is your favorite color?")),
                            textInput("q2", HTML("<strong>Question 2:</strong> <br> Have you ever been to the moon?")),
                            textInput("q3", "Question 3"),
                            textInput("q4", "Question 4")
                            ),
                        box(plotOutput("distPlot")),
                        box(sliderInput("bins",
                                        "Number of bins:",
                                        min = 5,
                                        max = 10,
                                        value = 5))
                    )),
            tabItem(tabName = "section-2",
                    h2("This is where a user would select how many people they identify"),
                    actionButton('insertBtn', 'Insert'),
                    tags$div(id = 'placeholder')   ),
            tabItem(tabName = "section-3",
                    h2("This is where a user give more information about their network"))
        )
        
        
        
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent(input$insertBtn, {
        insertUI(
            selector = '#placeholder',
            ui = textInput('txt3', 'Seeks advice from: ')
        )
    })
    
    observeEvent(input$removeBtn, {
        removeUI(
            selector = 'div:has(> #txt1)'
        )
    })
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
