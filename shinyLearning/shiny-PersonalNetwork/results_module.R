# module to show the results of the participant's personal network

ResultsUI <- function(id, BOXtitle) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      box(title = BOXtitle, status="primary", solidHeader = TRUE, collapsible = TRUE, #background = "black",
          # fluidRow(
            infoBox("Age", paste0(30, "%"), icon = icon("align-left"), fill=TRUE, color="olive", width=12 ),
          # ),
          # fluidRow(
            infoBox("Gender", paste0(90, "%"), icon = icon("angle-double-up"), fill=TRUE, color="olive", width=12 ),
          # ),
          # fluidRow(
            infoBox("Race", paste0(50, "%"), icon = icon("bars"), fill=TRUE, color="olive", width=12 ),
          # ),
          # fluidRow(
            infoBox("Social Status", paste0(66, "%"), icon = icon("bookmark"), fill=TRUE, color="olive" , width=12)
          # )
      )
    )
  )
}

ResultTextUI <- function(id, BOXtitle) {
  ns <- NS(id)
  box(
    h4("Based of these results your network is very similar to you")
  ) 
}
