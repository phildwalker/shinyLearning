# save and load results


saveData <- function(input) {
  # put variables in a data frame
  data <- data.frame(matrix(nrow=1,ncol=0))
  for (x in fields) {
    var <- input[[x]]
    if (length(var) > 1 ) {
      # handles lists from checkboxGroup and multiple Select
      data[[x]] <- list(var)
    } else {
      # all other data types
      data[[x]] <- var
    }
  }
  data$submit_time <- date()
  
  # Create a unique file name
  fileName <- sprintf(
    "%s_%s.rds",
    as.integer(Sys.Date()),
    digest::digest(data)
  )
  
  # fileName <- session$token
  
  # Write the file to the local system
  saveRDS(
    object = data,
    file = file.path(outputDir, fileName)
  )
}

loadData <- function() {
  # read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  
  if (length(files) == 0) {
    # create empty data frame with correct columns
    field_list <- c(fields, "submit_time")
    data <- data.frame(matrix(ncol = length(field_list), nrow = 0))
    names(data) <- field_list
  } else {
    data <- lapply(files, function(x) readRDS(x)) 
    
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
  }
  
  data
}



# resetForm <- function(session) {
#   updateTextInput(session, "text_demo", value = "")
#   updateSelectInput(session, "select_demo", selected=character(0))
#   # updateRadioButtons(session, "radio_demo", selected = "yes")
#   # updateCheckboxInput(session, "checkbox_demo", value = FALSE)
#   # updateCheckboxGroupInput(session, "cbgroup_demo", selected=character(0))
#   # updateTextAreaInput(session, "textarea_demo", value = "")
#   # updateTextInput(session, "password_demo", value = "")
#   updateSliderInput(session, "slider_demo", value = 50)
#   # updateDateInput(session, "date_demo", value = NA)
#   # updateDateRangeInput(session, "daterange_demo", start = NA, end = NA)
#   updateNumericInput(session, "number_demo", value = 0)
# }
