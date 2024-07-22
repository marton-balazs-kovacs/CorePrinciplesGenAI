#' Functions to create the HTML structure of the shiny app dynamically
#'
#'These functions, first, read the header and the questions sections content and structure
#'from the appropriate internal datafile depending on whether a long or short checklist
#'is being built. Second, they create the HTML DOM tree dynamically when they called. To
#'save computing power we use them only once on app start.
#'
#' @section Warning: The dynamically created HTML element ids should be namespaced
#'   in order to reach their inputs from the server side of the modules.
#' 
renderSection <- function(section, id = NULL, answers = NULL){
  fluidRow(
    # render all fields within this section
    lapply(section$Questions, customField, id = id, answers = answers),
    
    # a break line after each section
    fluidRow(hr())
  )
  
}

customField <- function(ind, id = NULL, answers = NULL){
  # if the input is not a question, it is assumed that it is some guidance text in between the items
  if(ind$Type == "text"){
    # Get styling of labels
    if(!is.null(ind$Style)) {
      style <- ind$Style
    } else {
      style <- "font-weight: bold;"
    }
    # the guidance text can itself be conditional
    if(is.null(ind$Depends)){
      fluidRow(column(1),
               column(10, br(), tags$span(HTML(ind$Label), style = style)),
               column(1))
    } else {
      # Add module id
      depends <- dep_ns(ind$Depends, id = id)
      
      conditionalPanel(condition = depends,
                       fluidRow(column(1), 
                                column(10, br(), tags$span(HTML(ind$Label), style = style)),
                                column(1))
                       )
    }
  } else { # render questions
    customButton(ind, id = id, answers = answers)
  }
}

customButton <- function(ind, id = NULL, answers = NULL){
  # Get styling of labels
  if(!is.null(ind$Style)) {
    style <- ind$Style
  } else {
    style <- "font-weight: bold;"
  }
  
  # Always display unconditional items
  if (is.null(ind$Depends)) {
    ind$Depends <- "true"
  }
  # or display depending on the state of the input
  else {
    # Add module id
    ind$Depends <- dep_ns(ind$Depends, id = id)
  }
  
  fluidPage( # wrapping into another fluid page makes a slight indentation of the questions from the text fields
    conditionalPanel(
      condition = ind$Depends,
      fluidRow(
        # Left offset margin
        column(1),
        # Question label
        column(8,
               br(),
               tags$span(with_i18n(ind$Label, ind$Label), style = "font-weight: bold;")
        ),
              #a(ind$href, href = ind$href, target = "_blank"),
              #ind$LabelEnd), # this makes the buttons appear horizontally aligned
              # do not add ns here as it would duplicate ns tag in consequent calls
        # Answer buttons
        column(1,
               switchButtons(ind, id = id, answers = answers)
        ),
        # Icon for opening textbox for additional explanation
        # We decided to provide the option to add explanation by a toggle button
        # We currently add explanation as a separate response dependent on the button which is not in questions.json
        # This is a makeshift solution should be replaced by nested dependency structure (by rows and button element e.g.) if app gets picked up
        # in that case json-schema should be provided for the app so it is more generalizable for other tasks in the future
        # Also sections should be optional as well
        # if (!is.null(ind$Toggle) && ind$Toggle){
        # column(1,
        # 
        #          actionButton(
        #            inputId = paste0(id, "-", ind$Name, "_button"),
        #            label = "",
        #            icon = icon("far fa-pen-to-square", lib = "font-awesome", class = "dependency-icon")
        #          )
        # 
        #        )
        # },
        # Icon to show whether the question is answered
        # Only for mandatory questions
        column(1,
               br(),
               if ( ind$Mandatory ){
                 # Adds exclamation circle next to the item
                 tags$div(
                   class = "toggle-icon",
                   id = shiny::NS(id, paste0("div", ind$Name, "Checker")),
                   title = "This question needs to be answered.",
                   tags$i(
                     id = shiny::NS(id,  paste0(ind$Name, "Checker")),
                     class = 'fa fa-exclamation-circle')
                   ) |> with_i18n("This question needs to be answered.", attribute = "title")
               }
        ),
        # Right offset margin
        column(1)
        )
      )
    )
}


switchButtons <- function(ind, id = NULL, answers = NULL){
  # Add module id
  ind$Name <- shiny::NS(id, ind$Name)
  
  answerList <- CARE:::checklist$answerList
  # if the AnswerType is specified in the answerList object (from .json), the button options should be rendered from 
  # those options
  # otherwise, the AnswerType is passed directly to the options
  if(ind$AnswerType %in% names(answerList)){
    answerOptions <- answerList[[ind$AnswerType]]
  } else{ 
    answerOptions <- ind$AnswerType
  }
  
  # preserve selected values if translation was called
  answered <- ind$Name %in% names(answers)
  if(answered){
    selected <- answers[[ind$Name]]
  } else{
    selected <- NULL
  }
  
  # switch between different input types
  switch (ind$Type,
    "select"    = pickerInputTranslatable (inputId = ind$Name, choices = answerOptions),
    "radio"     = radioButtonTranslatable  (inputId = ind$Name, choices = answerOptions),
    "textInput" = textInput                (inputId = ind$Name, label = with_i18n(ind$Label, ind$Label)),
    "textArea"  = textAreaInputTranslatable(inputId = ind$Name, placeholder = answerOptions, rows = 2)
  )
}


getItemList <- function(sectionsList, all = TRUE, id = NULL){
  # Get list of question ids
  items <- unlist(
    sapply(sectionsList,
           function(section) {
             sapply(section$Questions, function(item) item$Name)
             }
           )
    )
  
  # Add module id
  items <- shiny::NS(id, items)
  
  if(all){
    return(items)
  } else {
    return(items[grep("ind", items)])
  }
}