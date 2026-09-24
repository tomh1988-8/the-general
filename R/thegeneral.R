#' Run The General Shiny Application
#'
#' @param debug Enable debug mode with additional logging
#' @return A Shiny application object
#' @export
the_general_app <- function(debug = FALSE) {
  options(the_general.debug = isTRUE(debug))

  app_init(debug = debug)

  logger::log_info(glue::glue(
    "Launching The General | debug={isTRUE(debug)}"
  ))

  ui <- app_ui

  shiny::shinyApp(
    ui = ui,
    server = app_server,
    onStart = function() {
      logger::log_debug("Shiny onStart: ensuring assets and data")
      ensure_assets()
      ensure_data()
      logger::log_debug("Shiny onStart: assets and data ready")
    },
    options = list(launch.browser = TRUE)
  )
}
