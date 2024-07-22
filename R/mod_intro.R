#' intro UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_intro_ui <- function(id){
  ns <- NS(id)
  tagList(
    absolutePanel(
      shinyWidgets::actionBttn(inputId = ns("trigger"), label = with_i18n("About", "About"), icon = icon("circle-info")),
      top = "3%", left = "2%",
      # fixed = TRUE,
      width = "auto"
    )
  )
}
    
#' intro Server Functions
#'
#' @noRd 
mod_intro_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Content of the intro modal
    intro_modal <- function() {
      modalDialog(
        withTags({
          div(id = "intro-modal-content",
            h3("Criteria for AI-usage in Research") |> with_i18n(NULL),
            br(),
            h4("A Comprehensive Guideline & Checklist ") |> with_i18n(NULL),
            br(),
            p("The aim of this checklist is to provide a framework for researchers, publishers, and institutions to prepare and assess a responsible use of AI in research.") |> with_i18n(NULL),
            br(),
            h3("Instruction") |> with_i18n(NULL),
            p("Please fill out the checklist on behalf of your co-authors. It's important that before using any AI tools in your research, that you:") |> with_i18n(NULL),
            ol(
              li("Become and remain well-informed about the regulations of AI usage that are relevant to your study, including data security, transparency, and originality;") |> with_i18n(NULL),
              li("Gather sufficient knowledge and experience such that you are able to use your AI tool professionally;") |> with_i18n(NULL),
              li("Set quality criteria and quality control procedures for your AI-assisted outcomes;") |> with_i18n(NULL),
              li("Consult your co-authors and ensure they are aware of and comply with the requirements for responsible AI usage listed in this checklist.") |> with_i18n(NULL),
              li("Fill out all the questions. If your answer is ‘no’ you are required to provide sufficient explanation to complete the checklist. After filling out the checklist, you can generate the report.") |> with_i18n(NULL),
            ),
            br(),
            span("A detailed explanation of the checklist is available in the publication (REF).") |> with_i18n(NULL),
            # hr(),
            # p(with_i18n("Feedback and recommendations for an update of the checklist can be provided here:", NULL)),
            # br(),
            # a(
            #   img(src = "www/GitHub-Mark-32px.png"),
            #   href = "https://github.com/BalazsAczel/TransparencyChecklist",
            #   target = "_blank"
            # )
          )
        }),
        easyClose = TRUE,
        footer = NULL
      )
    }
    
    # Open intro modal
    observeEvent(input$trigger, {
      showModal(intro_modal())
      localize("#intro-modal-content")
    })
    
    # # Link language button in main app and in the about window
    # observe({
    #   updateSelectInput(session, "intro_language", selected = language_code())
    # })
    # 
    # # Return the language set in the intro modal so the other language select input can be updated
    # return(reactive(input$intro_language))
  })
}
    
## To be copied in the UI
# mod_intro_ui("intro")
    
## To be copied in the server
# mod_intro_server("intro")
