#' Axis Limits UI
#'
#' @param id Module ID.
#' @param title Box title.
#' @param min_label Minimum-axis input label.
#' @param max_label Maximum-axis input label.
#' @param reset_label Reset button label.
#'
#' @return Shiny UI.
#' @keywords internal
mod_axis_limits_ui <- function(
  id,
  title = "Y axis",
  min_label = "Min",
  max_label = "Max",
  reset_label = "Auto"
) {
  ns <- shiny::NS(id)

  shiny::div(
    id = ns("axis_limits_scope"),
    class = "general-axis-limits",
    shiny::div(
      class = "general-axis-limits-title",
      title
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::numericInput(
          inputId = ns("axis_min"),
          label = min_label,
          value = NA,
          step = 1
        )
      ),
      shiny::column(
        width = 6,
        shiny::numericInput(
          inputId = ns("axis_max"),
          label = max_label,
          value = NA,
          step = 1
        )
      )
    ),
    shiny::actionButton(
      inputId = ns("reset_axis"),
      label = reset_label,
      class = "btn btn-light general-axis-limits-reset"
    )
  )
}

#' Axis Limits Server
#'
#' @param id Module ID.
#'
#' @return Reactive list with `min` and `max`.
#' @keywords internal
mod_axis_limits_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$reset_axis, {
      shiny::updateNumericInput(session, "axis_min", value = NA)
      shiny::updateNumericInput(session, "axis_max", value = NA)
    })

    shiny::reactive({
      list(
        min = input$axis_min %||% NA_real_,
        max = input$axis_max %||% NA_real_
      )
    })
  })
}
