#' @export
app_server <- function(input, output, session) {
  ### Session setup ------------------------------------------------------------
  session$userData$sessionCode <- uuid::UUIDgenerate()
  logger::log_info(glue::glue(
    "Session ID initialized: {session$userData$sessionCode}"
  ))

  ### Pin store ----------------------------------------------------------------
  pins <- shiny::reactiveVal(list())

  ### Header navigation --------------------------------------------------------
  general_header_nav_server(input, output, session, menu_id = "tabs")

  ### Global filter / ingestion contract --------------------------------------
  rv <- mod_global_filter_server("global_1")

  shiny::observeEvent(
    rv$ingestion_contract,
    {
      shiny::req(rv$ingestion_contract)

      logger::log_info(glue::glue(
        "[Session {session$userData$sessionCode}] Ingestion contract status: {rv$ingestion_contract$status}"
      ))
    },
    ignoreInit = FALSE
  )

  shiny::observeEvent(
    rv$capabilities,
    {
      shiny::req(rv$capabilities, rv$dataset_name)

      capability_flags <- rv$capabilities$capability_flags %||% list()
      capability_names <- names(capability_flags)

      supported <- capability_names[unlist(capability_flags)]
      unsupported <- capability_names[!unlist(capability_flags)]

      logger::log_info(glue::glue(
        "[Session {session$userData$sessionCode}] Dataset capabilities for {rv$dataset_name} | supported: {paste(supported, collapse = ', ')} | unsupported: {paste(unsupported, collapse = ', ')}"
      ))
    },
    ignoreInit = FALSE
  )

  ### Analysis modules ---------------------------------------------------------
  mod_frequencies_server("freq_1", rv)
  mod_proportions_server("prop_1", rv)
  mod_proportions2_server("prop2_1", rv)
  mod_averages_server("avg_1", rv)
  mod_averages2_server("avg2_1", rv)
  mod_lines_server("lines_1", rv, pins)
  mod_lines2_server("lines2_1", rv, pins)
  mod_bars_server("bars_1", rv, pins)
  mod_bars2_server("bars2_1", rv, pins)
  mod_bars3_server("bars3_1", rv, pins)
  mod_areas_server("areas_1", rv, pins)
  mod_areas2_server("areas2_1", rv, pins)
  mod_scatterplots_server("scatter_1", rv, pins)
  mod_density_server("density_1", rv, pins)

  ### My dashboard -------------------------------------------------------------
  mod_my_dashboard_server("dash_1", rv, pins)

  ### keynav -------------------------------------------------------------------
  keyNavServer(
    id = "nav",
    tab_order = c(
      "frontPage",
      "Frequencies",
      "Proportions",
      "Proportions2",
      "Averages",
      "Averages2",
      "Lines",
      "Lines2",
      "Bars",
      "Bars2",
      "Bars3",
      "Areas",
      "Areas2",
      "ScatterGrouped",
      "Density",
      "myDashboard"
    ),
    menu_id = "tabs",
    global_sess = session
  )

  ### It's a wrap --------------------------------------------------------------
}
