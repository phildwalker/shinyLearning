library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyalert)
library(shinydashboardPlus)
# library(shinythemes)
library(tidyverse)

source("mod_inputs.R")
source("mod_save_load.R")
source("head_foot.R")
source("pw_module.R")



# Set up questionnaire interface ----
ui <- 
  dashboardPagePlus(
    skin = "red",
    sidebar_fullCollapse = TRUE,
    
    dashboardHeader(title = "Personal Network"),
    
    dashboardSidebar(
      useShinyjs(),
      sidebarMenu(id = "sidebarmenu", inactiveLink,
                  menuItem("Your Info",tabName = "section-1", icon = icon("user-circle") ),
                  menuItem( "Initialize Network",tabName = "section-2",icon = icon("people-carry") ),
                  menuItem("Description",tabName = "section-3",icon = icon("comments")),
                  menuItem( "Results", tabName = "section-4", icon = icon("external-link-alt"))
      )
    ),
    
    dashboardBody(useShinyjs(), useShinyalert(),
                  tabItems(
                    tabItem(tabName = "section-1",
                            # actionButton("submit", "Submit"),
                            box(
                              title = "Sample Demographics", status="primary", solidHeader = TRUE, collapsible = TRUE,
                              select_gender(id="self_gender"),
                              select_age(id="self_age"),
                              select_SES(id = "self_SES"),
                              select_ED(id = "self_ED")
                                ),
                            fluidPage(
                              Password_UI(id = "PWD_sec1")
                            )
                    ),
                    
                    tabItem(tabName = "section-2",
                            h2("Section 2 tab content"),
                            h4("This is where a user would select how many people they identify"),
                            box(
                              title = "Select the amount of people you regularly seek advice from:",
                              sliderInput("amt", "", min = 1, max=10, value = 1),
                              br(),
                              text_influ(value = 1),
                              text_influ(value = 2)
                            ),
                            fluidPage(
                              Password_UI(id = "PWD_sec2")
                            )
                    ),
                    
                    tabItem(tabName = "section-3",
                            h2("This is where a user give more information about their network"),
                            br(),
                            actionButton("downloadData", "Show my Results"),
                            fluidPage(
                              Password_UI(id = "PWD_sec3")
                            )
                            ),
                    
                    tabItem(tabName = "section-4",
                            h2("Show the homophily of your network"),
                            h4("How similar is your network?"),
                            
                            fluidRow(
                            )
                    )
                  )),
    footer = dashboardFooter(left_text = leftText,
                             right_text = rightText)
  )
    

# Reactive functions ----
server = function(input, output, session) {
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$sidebarmenu, {
    saveData(input)

    # thank the user
    n_responses <- length(list.files(outputDir))
    response <- paste0("Thank you for completing the survey! You are respondant ",
                       n_responses, ".")
    showNotification(response, duration = 0, type = "message")
  })
  

  #Disable menuitem when the app loads
  sections <- list("section-2", "section-3", "section-4")
  
  addCssClass(selector = "a[data-value='section-2']", class = "inactiveLink")
  addCssClass(selector = "a[data-value='section-3']", class = "inactiveLink")
  addCssClass(selector = "a[data-value='section-4']", class = "inactiveLink")
  
  # Set passwords for each screen
  entPW <- callModule(Password, id = "PWD_sec1")
  callModule(mod_server, id = "PWD_sec1", password = 100, 
             section = "a[data-value='section-2']", enteredPW=entPW, moveTo="section-2", parent=session)
  
  entPW2 <- callModule(Password, id = "PWD_sec2")
  callModule(mod_server, id = "PWD_sec2", password = 200, 
             section = "a[data-value='section-3']", enteredPW=entPW2, moveTo="section-3", parent=session)
  
  entPW3 <- callModule(Password, id = "PWD_sec3")
  callModule(mod_server, id = "PWD_sec3", password = 300, 
             section = "a[data-value='section-4']", enteredPW=entPW3, moveTo="section-4", parent=session)
  
  
  
}

shinyApp(ui, server)