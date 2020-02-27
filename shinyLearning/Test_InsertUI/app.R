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
library(shinyjs)
library(shinyalert)
library(shinydashboardPlus)
# library(shinythemes)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- dashboardPagePlus(skin = "red",
                    
                    dashboardHeader(title = "Personal Network"), 
                    
                    dashboardSidebar(width = 75,
                                     conditionalPanel("input.next1 != 12 && input.next2 != 54", {
                                         sidebarMenu(id = "sidebarmenu",
                                                     menuItem("", tabName = "section-1", icon = icon("user-circle", "fa-3x"))
                                         )
                                     }),

                                     conditionalPanel("input.next1 == 12 && input.next2 != 54", {
                                         sidebarMenu(id = "sidebarmenu",
                                                     menuItem("", tabName = "section-1", icon = icon("user-circle", "fa-3x")),
                                                     menuItem("",tabName = "section-2", icon = icon("people-carry", "fa-3x"))
                                         )
                                     }),

                                     conditionalPanel("input.next1 == 12 && input.next2 == 54", {
                                         sidebarMenu(id = "sidebarmenu",
                                                     menuItem("", tabName = "section-1", icon = icon("user-circle", "fa-3x")),
                                                     menuItem("",tabName = "section-2", icon = icon("people-carry", "fa-3x")),
                                                     menuItem("",tabName = "section-3", icon = icon("comments", "fa-3x"))
                                         )
                                     })

                    ),
                    
                    # dashboardSidebar(width = 75,
                    #                      sidebarMenu(id = "sidebarmenu",
                    #                                  menuItem("", tabName = "section-1", icon = icon("user-circle", "fa-3x")),
                    #                                  menuItem("",tabName = "section-2", icon = icon("people-carry", "fa-3x")),
                    #                                  menuItem("",tabName = "section-3", icon = icon("comments", "fa-3x"))
                    #                      )
                    # ),
                    
                    dashboardBody(
                        
                        tabItems(
                            tabItem(tabName = "section-1",
                                    fluidRow(
                                        box(title = "Getting user information", status="primary", solidHeader = TRUE, collapsible = TRUE, #background = "black",
                                            selectizeInput("q1", HTML("<strong>Question 1:</strong> <br> What is your gender?"),
                                                        list('Gender' = list("Male", "Female")),
                                                        options = list(
                                                            placeholder = 'Please select an option below',
                                                            onInitialize = I('function() { this.setValue(""); }')
                                                            )
                                                        ),
                                            textInput("q2", HTML("<strong>Question 2:</strong> <br> Have you ever been to the moon?")),
                                            selectizeInput("q3", HTML("<strong>Question 3:</strong> <br> What is your age?"),
                                                           list('Adolescent' = list("[3-12]", "[13-19]"),
                                                               'Young Adults' = list("[20-29]", "[30-39]"),
                                                                'Middle Age' = list("[40-49]", "[50-59]"),
                                                               'Elder' = list("[60-69]", "[70-79]", "[80+]")
                                                                ),
                                                           options = list(
                                                               placeholder = 'Please select an option below',
                                                               onInitialize = I('function() { this.setValue(""); }')
                                                           )
                                            ),
                                            textInput("q4", "Question 4", placeholder = "Sample hint text")
                                        ),
                                        
                                        box(title = "Move to next",
                                            textInput("next1", "Enter Password to move to enable next tab", placeholder = "The password is: 12"))
                                    )
                                    
                            ),
                            tabItem(tabName = "section-2",
                                    h2("This is where a user would select how many people they identify"),
                                    
                                    box(title = "Select the amount of people you regularly seek advice from:",
                                        sliderInput("amt", "", min = 1, max=10, value = 1),
                                        br(),
                                        h5("Names of people below"),
                                        uiOutput("person")
                                        # ,textOutput("influence")
                                    ),
                                    # box(
                                    #     actionButton('insertBtn', 'Insert'),
                                    #     tags$div(id = 'placeholder')
                                    #     ),
                                    
                                    # actionButton('insertBtn', 'Insert'),
                                    # tags$div(id = 'placeholder'),
                                    box(title = "Move to next",
                                        textInput("next2", "Enter Password to move to enable next tab", placeholder = "The password is: 54"))
                            ),
                            tabItem(tabName = "section-3",
                                    h2("This is where a user give more information about their network"),
                                    
                                    box(title = "Enter names below",
                                        # uiOutput("person"),
                                        textOutput("influence")),
                                    
                                    box(plotOutput("distPlot")),
                                    box(sliderInput("bins", "Number of bins:", min = 5,  max = 10, value = 5)),
                                    box(actionButton("downloadData", "Generate Report"))
                            )
                        )
                        
                    ),
                    footer = dashboardFooter(
                        left_text = HTML(paste0(
                            "<small>&copy;  p.walker"
                        )),
                        right_text = HTML(paste0(
                            "<script>",
                            "var today = new Date();",
                            "var yyyy = today.getFullYear();",
                            "</script>",
                            "<i class='fas fa-user-astronaut fa-lg'></i>",
                            " a pickles product - <script>document.write(yyyy);</script>",
                            "<table style='margin-left:auto; margin-right:auto;'>",
                            "</table>"))
                    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # observeEvent(input$insertBtn, {
    #     insertUI(
    #         selector = '#placeholder',
    #         ui = textInput('txt3', 'Seeks advice from: ')
    #     )
    # })
    
    col_names <- reactive(paste0("person", seq_len(input$amt)))
    
    output$person <- renderUI({
        map(col_names(), ~ textInput(.x, NULL, placeholder = "Enter the name of the person you seek advice from"))
    })
    
    output$influence <- renderText({
        map_chr(col_names(), ~ input[[.x]])
    })
    
    
    # observeEvent(input$removeBtn, {
    #     removeUI(
    #         selector = 'div:has(> #txt1)'
    #     )
    # })
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    # Want to save off the survey information when the user is done (ie like maybe right before show them the results?)

    # writeValues <- reactive({
    #     file <- paste("data-", Sys.Date(), ".csv", sep="")
    #     inputsList <- names(reactiveValuesToList(input))
    #     exportVars <- paste0(inputsList, ",", sapply(inputsList, function(inpt) input[[inpt]]))
    #     write(exportVars, file)
    # })    
    
    
    observeEvent(input$downloadData, {
        file <- paste("data-", Sys.Date(), ".csv", sep="")
        inputsList <- names(reactiveValuesToList(input))
        exportVars <- paste0(inputsList, ",", sapply(inputsList, function(inpt) input[[inpt]]))
        write(exportVars, file)
    })
    

        
        # downloadHandler(
        # 
        # filename = function() {
        #     paste("data-", Sys.Date(), ".csv", sep="")
        # },
        # content = function(file) {
        #     inputsList <- names(reactiveValuesToList(input))
        #     exportVars <- paste0(inputsList, ",", sapply(inputsList, function(inpt) input[[inpt]]))
        #     write(exportVars, file)
        # })  
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)