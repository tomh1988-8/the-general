# app_ui.R

#' @export
app_ui <- function(request) {
  css_path <- system.file(
    "app/www/css/styles.css",
    package = "thegeneral",
    mustWork = FALSE
  )

  if (!nzchar(css_path) && file.exists("inst/app/www/css/styles.css")) {
    css_path <- normalizePath(
      "inst/app/www/css/styles.css",
      winslash = "/",
      mustWork = TRUE
    )
  }

  css_version <- if (nzchar(css_path) && file.exists(css_path)) {
    as.integer(file.info(css_path)$mtime)
  } else {
    as.integer(Sys.time())
  }

  bs4Dash::dashboardPage(
    title = "The General",
    help = NULL,
    dark = NULL,

    header = bs4Dash::dashboardHeader(
      skin = "dark",
      status = "info",
      sidebarIcon = shiny::icon("bars"),
      controlbarIcon = shiny::icon("sliders-h"),
      rightUi = general_header_nav_ui()
    ),

    sidebar = general_sidebar_ui(),

    body = bs4Dash::dashboardBody(
      shinybusy::add_busy_spinner(
        spin = "circle",
        position = "top-right",
        timeout = 0
      ),

      shiny::tags$head(
        shiny::tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = paste0("www/css/styles.css?v=", css_version)
        ),
        shiny::tags$script(
          src = paste0(
            "www/js/general.header.nav.js?v=",
            as.numeric(Sys.time())
          ),
          type = "text/javascript"
        )
      ),

      keyNavUI("nav"),

      shinyjs::useShinyjs(),

      general_tab_items_ui()
    )
  )
}
