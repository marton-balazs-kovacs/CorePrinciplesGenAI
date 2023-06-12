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
    
    # changing icons when item is answered
    # change id to fit module
    observe({
      items <- getItemList(sectionsList, all = FALSE) # loop only on items

      for(item in items){
        session$sendCustomMessage(
          type = "toggleChecker",
          message = list(
            id = ns(paste0(item, "Checker")),
            val = input[[item]],
            divId = ns(paste0("div", item, "Checker"))
            )
        )
      }

    })
    
    # stores the answers in a list
    answers <- reactive({
      reactiveValuesToList(input)
    })
    
    # return answers
    return(answers)
  })
}
    
## To be copied in the UI
# mod_sections_ui("sections")
    
## To be copied in the server
# mod_sections_server("sections")
