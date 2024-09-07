#' Validate email input
validate_email <- function(x, empty.valid) {
  # code from Felix Schönbrodt
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case = TRUE)
}

#' Validate all responses
validate_report <- function(answers = NULL, sectionsList = NULL, headList = NULL){
  
  # Check the head section first
  if(!is.null(headList)){
    completeHead <- sapply(headList, function(question){
      if (isTRUE(question$Mandatory)) {
        return(!is.null(answers[[question$Name]]) && gsub(" ", "", answers[[question$Name]]) != "")
      }
      return(TRUE)  # Skip validation if not mandatory
    })
    
    # If any mandatory head question is not complete, return FALSE
    if (!all(completeHead)) {
      # print("Head validation failed")
      return(FALSE)
    }
  }
  
  # Now validate the sections
  completeSection <- vector("logical", length(sectionsList))
  
  for (sec_idx in seq_along(sectionsList)) {
    section <- sectionsList[[sec_idx]]
    section_complete <- TRUE 
    
    for (i in seq_along(section$Questions)) {
      result <- validate_question(section$Questions[[i]], answers)
      if (!result$complete) {
        section_complete <- FALSE
        break  # Early exit if any question is incomplete
      }
    }
    
    completeSection[sec_idx] <- section_complete
  }
  
  # Return TRUE if all sections are complete
  if(!all(completeSection)){
    # print("Section validation failed")
    return(FALSE)
  }
  
  return(TRUE)
}

#' Validate responses to questions
#' Validate responses to questions
validate_question <- function(question, answers){
  error_message <- NULL
  
  # If it's not mandatory to respond, skip to another question
  if (!is.null(question$Mandatory) && !question$Mandatory) {
    return(list(complete = TRUE, error_message = error_message))
  }
  
  # If the question is a comment or some guidance text, skip
  if (question$Type == "text") {
    return(list(complete = TRUE, error_message = error_message))
  }
  
  # Check if the question is supposed to be shown
  if (!is.null(question$Depends)) {
    shown <- eval(parse(text = gsub(".ind_", "answers$ind_", question$Depends)))
    
    # Depending on the predicate questions, the statement may return NA/logical(0)
    shown <- ifelse(length(shown) == 0 || is.na(shown), FALSE, shown)
    
    # If the question is not shown, mark as complete
    if (!shown) {
      return(list(complete = TRUE, error_message = error_message))
    }
  }
  
  # Validate mandatory response
  if (is.null(answers[[question$Name]]) || gsub(" ", "", answers[[question$Name]]) == "") {
    error_message <- "This question needs to be answered."
    return(list(complete = FALSE, error_message = error_message))
  }
  
  # Validate additional constraints (minChar, maxChar)
  if (!is.null(question$Validation)) {
    answer_value <- answers[[question$Name]]
    
    if (!is.null(question$Validation$minChar) && nchar(answer_value) < question$Validation$minChar) {
      error_message <- paste("Please enter at least", question$Validation$minChar, "characters.")
      return(list(complete = FALSE, error_message = error_message))
    }
    
    if (!is.null(question$Validation$maxChar) && nchar(answer_value) > question$Validation$maxChar) {
      error_message <- paste("Please enter no more than", question$Validation$maxChar, "characters.")
      return(list(complete = FALSE, error_message = error_message))
    }
  }
  
  return(list(complete = TRUE, error_message = error_message))
}
