#' Wrap Shiny output in a namespaced container (button excluded)
#'
#' @param id Module namespace id
#' @param output_ui_fn A function like `plotlyOutput` or `DTOutput` (un‐namespaced)
#'   that takes one argument (the output id) and returns UI markup.
#' @return UI output tag (without button)
#' @export
pinModuleUI <- function(id, output_ui_fn) {
  ns <- NS(id)
  output_ui_fn(ns("out"))
}
#' Server logic to capture & store a pinned view
#'
#' @param id Module namespace id
#' @param render_fn A function `(output, session, out_id) -> NULL` that assigns
#'   the appropriate `render*()` to `output[[out_id]]`
#' @param type A short character string identifying this view (e.g. "scatter")
#' @param params_r A reactive expression returning a named list of inputs
#' @param title_r A reactive expression returning a single string title
#' @param pins The reactiveVal object storing all pins (pass from parent server)
#' @return Invisibly `NULL`; side‐effect is updating a global `pins` store
#' @export
pinModuleServer <- function(
  id,
  render_fn,
  type,
  params_r,
  title_r = reactive(type),
  pins
) {
  moduleServer(id, function(input, output, session) {
    render_fn(output, session, "out")
    observeEvent(input$pin, {
      new_pin <- list(
        id = id,
        type = type,
        params = params_r(),
        title = title_r(),
        caption = Sys.Date()
      )
      pins(append(pins(), list(new_pin)))
    })
    invisible(NULL)
  })
}
