# Some of the modules that are going to be used for the application
# Sun Mar 01 10:30:49 2020 ------------------------------

Placeholder_list <- list(
  placeholder = 'Please select an option below',
  onInitialize = I('function() { this.setValue(""); }')
)

AgeList <- list(
  'Adolescent' = list("[3-12]", "[13-19]"),
  'Young Adults' = list("[20-29]", "[30-39]"), 
  'Middle Age' = list("[40-49]", "[50-59]"),
  'Elder' = list("[60-69]", "[70-79]", "[80+]")
)

SexList <-  list(
  'Gender' = list("Male", "Female")
  )



DemographUI <- function(id, BOXtitle) {
  ns <- NS(id)

  # fluidPage(
    fluidRow(
      box(title = BOXtitle, status="primary", solidHeader = TRUE, collapsible = TRUE, #background = "black",
          selectizeInput(inputId = ns("q1"), 
                         label= HTML("<strong>Question 1:</strong> <br> What is your gender?"),
                         choices=SexList,
                         options = Placeholder_list
          ),
          textInput(inputId = ns("q2"), 
                    label=HTML("<strong>Question 2:</strong> <br> Have you ever been to the moon?")),
          selectizeInput(inputId = ns("q3"), 
                         label=HTML("<strong>Question 3:</strong> <br> What is your age?"),
                         choices=AgeList,
                         options = Placeholder_list
          ),
          textInput(inputId = ns("q4"), 
                    label= "Question 4", placeholder = "Sample hint text")
      )
    )
  # )
  
}


BuildNetwork_UI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      box(title = "Select the amount of people you regularly seek advice from:", solidHeader = TRUE, 
          sliderInput(inputId = ns("amt"), 
                      label = " ", min = 5, max=10, value = 5)
      )),
    fluidRow(
      box(title= "Names of people below", solidHeader = TRUE, 
          uiOutput(ns("person"))
      )
    )
  )
  
}


BuildNetwork <- function(input,output,session){

  col_names <- reactive(paste0("person", seq_len(input$amt)))
  
  output$person <- renderUI({
    map(col_names(), ~ textInput(.x, NULL, value = " ", placeholder = "Enter the name of the person you seek advice from"))
  })
  
  # output$cols <- renderUI({
  #   input$tableName
  #   cols <- vector("list", input$ncols)
  #   for (i in seq_len(input$ncols)) {
  #     textInput(paste0("colName", i), "Influence name", placeholder = "Enter the person's name to help you remember")
  #   }
  #   cols
  # })
  
  observeEvent(input$amt, {
    tokenStrShow <- 1:input$amt
    tokenStrHide <- if (input$amt == 10) {
      NULL
    } else {
      (input$amt+1):10
    }
    
    # print(input$amt)
    # print(tokenStrShow)
    # print(paste0("Hiding Ids:",tokenStrHide))
    
    # tokenStr <- strsplit(input$accessToken, "")[[1]]
    # tokenLen <- length(tokenStr)
    # 
    # 
    # for (i in tokenStrShow){
    #   show(selector= paste0("ul li:eq(",i - 1,")"))
    # }
    # for (i in tokenStrHide){
    #   hide(selector= paste0("ul li:eq(",i - 1,")"))
    # }
    
  })
  
}



InfluencersUI <- function(id){
  ns <- NS(id)
  
  fluidRow(
    box(
    textInput("influ1", "Influence name 1", placeholder = "The first person you go to for advice"),
    textInput("influ2", "Influence name 2", placeholder = "The second person you go to for advice"),
    textInput("influ3", "Influence name 3", placeholder = "The third person you go to for advice"),
    textInput("influ4", "Influence name 4", placeholder = "The fourth person you go to for advice"),
    textInput("influ5", "Influence name 5", placeholder = "The fifth person you go to for advice"),
    textInput("influ6", "Influence name 6", placeholder = "The sixth person you go to for advice"),
    textInput("influ7", "Influence name 7", placeholder = "The seventh person you go to for advice"),
    textInput("influ8", "Influence name 8", placeholder = "The eighth person you go to for advice"),
    textInput("influ9", "Influence name 9", placeholder = "The ninth person you go to for advice"),
    textInput("influ10", "Influence name 10", placeholder = "The tenth person you go to for advice")
    )

  )
  
}


ShowInfluencers <- function(input,output,session){
  
  observe({
    input$influ1
    # this will run on app load and then again every time "mytext" changes
    # print(1:10) 
  })
  
}








