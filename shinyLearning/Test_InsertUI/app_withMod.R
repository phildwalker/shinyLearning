# Rebuilding the app but simpler to test a couple of features
# Sun Mar 01 10:25:32 2020 ------------------------------

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyalert)
library(shinydashboardPlus)
# library(shinythemes)
library(tidyverse)

# load module functions
source("module.R")
source("pw_module.R")
source("head_foot.R")


app_ui <-
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
                      fluidPage(
                        DemographUI(id = "DemographUI_self", BOXtitle = "Getting user information")
                        ),
                      fluidPage(
                        Password_UI(id = "Password_UI_sec1")
                      )
                    ),
                    
                    tabItem(tabName = "section-2",
                            h2("Section 2 tab content"),
                            h4("This is where a user would select how many people they identify"),
                            fluidPage(
                              BuildNetwork_UI(id = "SelectBuild")
                            ),
                            fluidPage(
                              Password_UI(id = "Password_UI_sec2")
                            )),
                    
                    tabItem(tabName = "section-3",
                            h2("This is where a user give more information about their network"),
                            br(),
                            actionButton("downloadData", "Show my Results"),
                            fluidPage(
                              Password_UI(id = "Password_UI_sec3")
                            )),
                    
                    tabItem(tabName = "section-4",
                            h2("Show the homophily of your network"),
                            h4("How similar is your network?"))
                  )),
    footer = dashboardFooter(left_text = leftText,
                             right_text = rightText)
  )

app_server <- function(input, output, session) {
  #Disable menuitem when the app loads
  sections <- list("section-2", "section-3", "section-4")
  
  addCssClass(selector = "a[data-value='section-2']", class = "inactiveLink")
  addCssClass(selector = "a[data-value='section-3']", class = "inactiveLink")
  addCssClass(selector = "a[data-value='section-4']", class = "inactiveLink")

  # Set passwords for each screen
  entPW <- callModule(Password, id = "Password_UI_sec1")
  callModule(mod_server, id = "Password_UI_sec1", password = 100, section = "a[data-value='section-2']", enteredPW=entPW)
  
  entPW2 <- callModule(Password, id = "Password_UI_sec2")
  callModule(mod_server, id = "Password_UI_sec2", password = 200, section = "a[data-value='section-3']", enteredPW=entPW2)
  
  entPW3 <- callModule(Password, id = "Password_UI_sec3")
  callModule(mod_server, id = "Password_UI_sec3", password = 300, section = "a[data-value='section-4']", enteredPW=entPW3)

  callModule(BuildNetwork, id = "SelectBuild")  

}

shinyApp(app_ui, app_server)