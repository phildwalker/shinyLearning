#----------------------------------
# UI and server for password input and enabled the next button

#----------------------------------
Password_UI <- function(id) {
  ns <- NS(id)
  useShinyalert()
  
  # fluidPage(
    fluidRow(
      box(title = "Enter the password given to you by the instuctor:",
          textInput(inputId = ns("pwd"), label="", placeholder = "Sample hint text"), #"pwd1", label= paste0("Enter PW (",pwd,")"), 
          actionButton(ns("btn"), "Submit Password"),
          textOutput(ns("pwdAnswer")),
          textOutput(ns("passwordENTERED")),
          textOutput(ns("hello"))
      )
      )
  # )
}



Password<-function(input,output,session){
  pwd<-reactive({input$pwd})
  output$passwordENTERED<-renderText({
    # req(input$print)
    paste0("Typed in password ",input$pwd)
  })
  return(pwd)
}



mod_server <- function(input, output, session, enteredPW, password, section, moveTo, parent) {
  ns <- session$ns
  
  output$pwdAnswer <- renderText({
    paste0("The pwd to enter is: ",password)
  })
  
  observeEvent(input$btn, {
    if (enteredPW() == password) {
      # print("Oh boy the button click worked!")
      removeCssClass(selector = section, class = "inactiveLink") #  "a[data-value='section-2']" // cat(dQuote(paste0("a[data-value='",section,"']")))
      
      updateTabItems(session = parent, "sidebarmenu",
                     selected = moveTo)
      
    }
    if (enteredPW() != password) {
      print("Oh no, it didn't work!")
      shinyalert("Oops!", "That wasn't the right password.", type = "error")
    }
    
  })
  
}