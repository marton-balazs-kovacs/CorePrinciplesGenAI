#' Translatable Widgets
#'
#' @title Translatable Widgets for Shiny
#' @description These functions provide customized UI elements (picker input, radio buttons, text area)
#' with built-in internationalization (i18n) support.
#' @name translatable_widgets
#' @param inputId The input ID.
#' @param choices Named vector or list of choices.
#' @param placeholder Placeholder text.
#' @param rows Number of rows in the textarea.
#' @return HTML tags (UI elements).
#' @export
pickerInputTranslatable <- function(inputId, choices) {
  options <- list()
  for(i in seq_along(choices)) {
    options[[i]] <- tags$option(value = choices[i], names(choices[i])) |> with_i18n(names(choices[i]))
  }
  
  div(
    class = "form-group shiny-input-container",
    tags$label(
      class = "control-label",
      `for` = inputId
    ),
    tags$select(
      id = inputId,
      class = "form-control",
      HTML('<option value="" disabled selected data-i18n="Please select an option">Please select an option</option>'),
      options
    )
  )
}

#' @rdname translatable_widgets
#' @export
radioButtonTranslatable <- function(inputId, choices) {
  div(id = inputId, class = "form-group shiny-input-radiogroup shiny-input-container", role = "radiogroup", `aria-labelledby` = sprintf("%s-label", inputId),
      tags$label(class = "control-label", id = sprintf("%s-label", inputId), `for` = inputId),
      radioButtonTranslatableOptions(choices, inputId))
}

radioButtonTranslatableOptions <- function(choices, inputId) {
  options <- list()
  for(i in seq_along(choices)) {
    options[[i]] <- radioButtonTranslatableOption(choices[i], inputId)
  }
  
  div(class = "shiny-options-group", options)
}

#' @rdname translatable_widgets
#' @export
radioButtonTranslatableOption <- function(choice, inputId) {
  div(class = "radio-inline",
      tags$input(type = "radio", name = inputId, value = choice),  
      # |> with_i18n(
      #   names(choice),
      #   attribute = "value"
      #   ),
      tags$label(
        names(choice),
        `for` = names(choice)
      ) |> 
        with_i18n(names(choice))
      
  )
}

#' @rdname translatable_widgets
#' @export
textAreaInputTranslatable <- function(inputId, placeholder, rows) {
  div(class = "form-group shiny-input-container",
      style = "flex-grow: 1;",
      tags$label(class = "control-label", id = inputId, `for` = inputId),
      tags$textarea(class = "form-control", id = inputId, placeholder = placeholder, rows = rows) |> with_i18n(placeholder, attribute = "placeholder")
  )
}