#' Validate email input
isValidEmail <- function(x, empty.valid) {
  # code from Felix Schönbrodt
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case = TRUE)
}

#' Validate all responses
validate_report <- function(answers = NULL, sectionsList = NULL){
  # Check questions and return TRUE if filled in sufficiently
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
  
  return(completeSection)
}

#' Validate responses to questions
validate_question <- function(question, answers){
  error_message <- NULL
  
  # If it's not mandatory to respond, skip to another question
  if(!is.null(question$Mandatory) && !question$Mandatory){
    return(list(complete = TRUE, error_message = error_message))
  }
  
  # If the question is a comment or some guidance text skip
  if (question$Type == "text") {
    return(list(complete = TRUE, error_message = error_message))
  }
  
  # Check whether the question is supposed to be even shown
  # If not, skip to another question
  if(!is.null(question$Depends)){
    shown <- gsub(".ind_", "answers$ind_", question$Depends)
    shown <- eval(parse(text = shown))
    
    # Depending on the status of the predicate questions, the above logical statement can result in logical(0) or NA,
    # which is caused by predicate questions not being answered yet
    # In that case, the question is not to be shown
    shown <- ifelse(length(shown) == 0 || is.na(shown), FALSE, shown)
    
    # if the question is not shown, we do not require any answers, and thus the question is complete regardless of the answer
    if(!shown){
      return(list(complete = TRUE, error_message = error_message))
    }
  }
  
  # The current question is supposed to be shown, and so it needs to be in answers; otherwise, the question is not completed
  if (is.null(answers[[question$Name]]) || gsub(" ", "", answers[[question$Name]]) == "") {
    error_message <- "This question needs to be answered."
    return(list(complete = FALSE, error_message = error_message))
  }
  
  # Check for additional validation rules in the JSON
  if (!is.null(question$Validation)) {
    if (!is.null(question$Validation$minChar)) {
      answer_value <- answers[[question$Name]]
      if (nchar(answer_value) < question$Validation$minChar) {
        error_message <- paste("Please enter at least", question$Validation$minChar, "characters.")
        return(list(complete = FALSE, error_message = error_message))
      }
    }
    
    if (!is.null(question$Validation$maxChar)) {
      answer_value <- answers[[question$Name]]
      if (nchar(answer_value) > question$Validation$maxChar) {
        error_message <- paste("Please enter no more than", question$Validation$maxChar, "characters.")
        return(list(complete = FALSE, error_message = error_message))
      }
    }
  }
  
  # If all checks out (question is shown and answered), then it is complete
  return(list(complete = TRUE, error_message = error_message))
}