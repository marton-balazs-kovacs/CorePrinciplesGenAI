#' intro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_intro_ui <- function(id) {
  ns <- NS(id)
  tagList(wellPanel(withTags({
    div(
      id = "intro-modal-content",
      p(
        "The aim of this checklist is to provide a framework for researchers, publishers, and institutions to prepare and assess a responsible use of AI in research."
      ) |> with_i18n(NULL),
      h3("Instruction") |> with_i18n(NULL),
      p(
        "Please fill out the checklist on behalf of your co-authors. It's important that before using any AI tools in your research, that you:"
      ) |> with_i18n(NULL),
      ol(
        li(
          "Become and remain well-informed about the regulations of AI usage that are relevant to your study, including data security, transparency, and originality;"
        ) |> with_i18n(NULL),
        li(
          "Gather sufficient knowledge and experience such that you are able to use your AI tool professionally;"
        ) |> with_i18n(NULL),
        li(
          "Set quality criteria and quality control procedures for your AI-assisted outcomes;"
        ) |> with_i18n(NULL),
        li(
          "Consult your co-authors and ensure they are aware of and comply with the requirements for responsible AI usage listed in this checklist."
        ) |> with_i18n(NULL),
        li(
          "Fill out all the questions. If your answer is ‘no’ you are required to provide sufficient explanation to complete the checklist. After filling out the checklist, you can generate the report."
        ) |> with_i18n(NULL),
      ),
      br(),
      span(
        "A detailed explanation of the checklist is available in the publication (REF)."
      ) |> with_i18n(NULL)
    )
  })))
}
    
#' intro Server Functions
#'
#' @noRd 
mod_intro_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
  })
}
    
## To be copied in the UI
# mod_intro_ui("intro")
    
## To be copied in the server
# mod_intro_server("intro")
