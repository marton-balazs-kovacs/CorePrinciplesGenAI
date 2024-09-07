#' sections UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sections_ui <- function(id, checklist){
  ns <- NS(id)
  
  sectionsList <- checklist$sectionsList
  
  sectionsHTML <- lapply(sectionsList, renderSection, id = id)
  names(sectionsHTML) <- NULL
  
  tagList(
    sectionsHTML
  )
}
    
#' sections Server Functions
#'
#' @noRd 
mod_sections_server <- function(id, checklist){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    sectionsList <- checklist$sectionsList
    
    # Changing icons when item is answered: on the fly validation
    observe({
      # Get item ids and add module ids to them
      items <- get_item_list(
        sectionsList, 
        # Return only module ids with required input from the user (question)
        all = FALSE)

      for(item in items){
        # Get the full definition of a given item
        question <- get_question(item = item, sectionsList = sectionsList)
        
        # Apply custom validation logic (based on JSON rules)
        validation_result <- validate_question(question = question, answers = reactiveValuesToList(input))
        
        # Send message of validation results to UI
        session$sendCustomMessage(
          type = "toggleChecker",
          message = list(
            id = ns(paste0(item, "Checker")),
            val = input[[item]],
            divId = ns(paste0("div", item, "Checker")),
            complete = validation_result$complete,
            error_message = validation_result$error_message
            )
        )
      }
    })
    
    # Store the answers to the inputs in a list
    answers <- reactive({
      reactiveValuesToList(input)
    })
    
    # Return answers
    return(answers)
  })
}
    
## To be copied in the UI
# mod_sections_ui("sections")
    
## To be copied in the server
# mod_sections_server("sections")
