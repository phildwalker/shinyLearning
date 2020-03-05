# module for input information

outputDir <- "responses"

# Define the fields we want to save from the form
fields <- c("self_age", "self_gender", "self_SES","self_ED", "amt",
            "influ1_age", "influ1_gender", "influ1_SES","influ1_ED",
            "influ2_age", "influ2_gender", "influ2_SES","influ2_ED",
            "influ3_age", "influ3_gender", "influ3_SES","influ3_ED",
            "influ4_age", "influ4_gender", "influ4_SES","influ4_ED",
            "influ5_age", "influ5_gender", "influ5_SES","influ5_ED",
            "influ6_age", "influ6_gender", "influ6_SES","influ6_ED",
            "influ7_age", "influ7_gender", "influ7_SES","influ7_ED",
            "influ8_age", "influ8_gender", "influ8_SES","influ8_ED",
            "influ9_age", "influ9_gender", "influ9_SES","influ9_ED",
            "influ10_age", "influ10_gender", "influ10_SES","influ10_ED")


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

SESList <-  list(
  'Income Bracket' = list("High", "Middle", "Low")
)

EduList <-  list(
  'Highest Education' = list("High School", "Some College", "Undergrad", "Masters", "PhD")
)


select_gender <- function(id="q1_gender"){
  selectizeInput(inputId = id, 
                 label= HTML("<strong>Question 1:</strong> <br> What is your gender?"),
                 choices=SexList,
                 options = Placeholder_list
  )
}
  

select_age <- function(id = "q3_age"){
  selectizeInput(inputId = id, 
                             label=HTML("<strong>Question 3:</strong> <br> What is your age?"),
                             choices=AgeList,
                             options = Placeholder_list
                             )
}
 
select_SES <- function(id = "q2_SES"){
  selectizeInput(inputId = id, 
                 label=HTML("<strong>Question 3:</strong> <br> What is your SES?"),
                 choices=SESList,
                 options = Placeholder_list
  )
} 

select_ED <- function(id = "q4_ED"){
  selectizeInput(inputId = id, 
                 label=HTML("<strong>Question 3:</strong> <br> What is your Education?"),
                 choices=EduList,
                 options = Placeholder_list
  )
} 

text_sample <- textInput(inputId = "q4_text", 
                         label= "Question 4", placeholder = "Sample hint text")

text_influ <- function(id = "influ", personNum){
  textInput(paste0(id,personNum), paste0("Influence name ",personNum), placeholder = "Type the name of the person") #, value = " "
}


fullBox <- function(linkNM){
  
  boxTitle <- (paste0(linkNM,"_nm"))
  
  div(id = paste0("outer-",linkNM),
    box(id = paste0("box",linkNM),
        title = textOutput(boxTitle), status="primary", solidHeader = TRUE, collapsible = TRUE, collapsed=TRUE,
        select_gender(id=paste0(linkNM,"_gender")),
        select_age(id=paste0(linkNM,"_age")),
        select_SES(id = paste0(linkNM,"_SES")),
        select_ED(id = paste0(linkNM,"_ED"))
    )
  )
}
  
 
  
  

help_demo <- helpText("You can write help text in your form this way")








