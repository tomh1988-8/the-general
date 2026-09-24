# R/app_server.R

#' Header navigation server logic
#' @noRd
general_header_nav_server <- function(
  input,
  output,
  session,
  menu_id = "tabs"
) {
  observeEvent(
    input$header_nav_tab,
    {
      selected_tab <- input$header_nav_tab

      if (is.null(selected_tab) || !nzchar(selected_tab)) {
        return(invisible(NULL))
      }

      bs4Dash::updateTabItems(
        session = session,
        inputId = menu_id,
        selected = selected_tab
      )
    },
    ignoreInit = TRUE
  )
}
