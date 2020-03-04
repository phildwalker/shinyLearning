# module for input information

outputDir <- "responses"

# Define the fields we want to save from the form
fields <- c("self_age", 
            "self_gender",
            "self_SES",
            "self_ED")


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

text_influ <- function(id = "influ", value){
  textInput(paste0(id,value), paste0("Influence name ",value), placeholder = "Type the name of the person")
}




help_demo <- helpText("You can write help text in your form this way")