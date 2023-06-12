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
            h3("What is CARE?") |> with_i18n(NULL),
            br(),
            h3("How to use it?") |> with_i18n(NULL),
            ul(
              li("The checklist refers to a single study of interest.") |> with_i18n(NULL),
              li("Please respond to each checklist item. If necessary, you can provide an explanation at the end of each section.") |> with_i18n(NULL),
              li("When the question refers to your manuscript, this includes all supplementary materials and appendices that are relevant to the study of interest.") |> with_i18n(NULL),
              li("After all the questions have been answered, you can generate a report for your study by pressing the button labeled GENERATE REPORT at the bottom of the page.") |> with_i18n(NULL),
              li("Save your report on your computer. Note that after you download your report, your responses on the checklist will not be saved by our webpage.") |> with_i18n(NULL),
              li("Upload your report to a public repository.") |> with_i18n(NULL),
            ),
            br(),
            span("You can cite the CARE as follows:") |> with_i18n(NULL),
            hr(),
            p(with_i18n("Feedback and recommendations for an update of the checklist can be provided here:", NULL)),
            br(),
            a(
              img(src = "www/GitHub-Mark-32px.png"),
              href = "https://github.com/BalazsAczel/TransparencyChecklist",
              target = "_blank"
            )
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
