# module to show the results of the participant's personal network
library(dplyr)
library(scales)

ResultsUI <- function(id, BOXtitle) {
  ns <- NS(id)
  
  # fluidPage(
  #   fluidRow(
  #     box(title = BOXtitle, status="primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, #background = "black",
  #                infoBox("Age", paste0(30, "%"), icon = icon("align-left"), fill=TRUE, color="olive", width=12 ),
  #                infoBox("Gender", paste0(90, "%"), icon = icon("angle-double-up"), fill=TRUE, color="olive", width=12 ),
  #                infoBox("Race", paste0(50, "%"), icon = icon("bars"), fill=TRUE, color="olive", width=12 ),
  #                infoBox("Social Status", paste0(66, "%"), icon = icon("bookmark"), fill=TRUE, color="olive" , width=12)
  #         )
  # 
  #     ),
  #   fluidRow(
  #     plotOutput(outputId = "RadarPlot")
  #   )
  # )
  
  fluidRow(
    box(
      title = BOXtitle, status="primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, #background = "black",
      infoBox("Age", paste0(30, "%"), icon = icon("align-left"), fill=TRUE, color="olive", width=12 ),
      infoBox("Gender", paste0(90, "%"), icon = icon("angle-double-up"), fill=TRUE, color="olive", width=12 ),
      infoBox("Race", paste0(50, "%"), icon = icon("bars"), fill=TRUE, color="olive", width=12 ),
      infoBox("Social Status", paste0(66, "%"), icon = icon("bookmark"), fill=TRUE, color="olive" , width=12)
    ),
    box(title = BOXtitle, status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
      plotOutput(outputId = "RadarPlot"),
      h4("some sample text to see if anything is rendering")
    )
  )
  
  
  
  
  
}

ResultTextUI <- function(id, BOXtitle) {
  ns <- NS(id)
  box(
    h4("Based of these results your network is very similar to you")
  ) 
}


plotRadar <- function(){
  library(dplyr)
  library(scales)
  
  mtcars %>% 
    tibble::as_tibble(rownames = "group") %>% 
    mutate_at(vars(-group), rescale) %>% 
    tail(1) %>% 
    select(1:10) %>% 
    ggradar::ggradar()
}



mod_result <- function(input, output, session, data) {
  ns <- session$ns
  
  module_data <- reactive({
    dplyr::mtcars %>% 
      tibble::as_tibble(rownames = "group") %>% 
      mutate_at(vars(-group), rescale) %>% 
      tail(1) %>% 
      select(1:10) 
  })
  
  output$RadarPlot <- renderPlot({
    plotRadar()
  })
  
}

