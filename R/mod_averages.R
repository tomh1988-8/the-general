#' Averages (1 Group) UI
#' @export
mod_averages_ui <- function(id) {
  ns <- shiny::NS(id)
  dropdown_container <- paste0("#", ns("averages_scope"))

  shiny::div(
    id = ns("averages_scope"),
    class = "general-averages-page",
    bs4Dash::bs4Card(
      title = "Averages · 1 Group",
      status = "info",
      solidHeader = TRUE,
      collapsible = TRUE,
      closable = FALSE,
      maximizable = TRUE,
      width = 12,
      shiny::fluidRow(
        class = "general-averages-controls",
        shiny::column(
          width = 6,
          shinyWidgets::pickerInput(
            inputId = ns("Tab4Variable1"),
            label = "Choose a time variable:",
            choices = c(
              "Month",
              "Financial_Quarter",
              "DOTW",
              "Financial_Year"
            ),
            multiple = FALSE,
            options = list(
              `style` = "btn-light btn-sm",
              `dropupAuto` = FALSE,
              `container` = dropdown_container,
              `size` = 8
            )
          )
        ),
        shiny::column(
          width = 6,
          shiny::uiOutput(ns("tab4_variable2_picker"))
        )
      ),
      mod_shared_datatable_ui(ns("dt_avg1")),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          mod_export_actions_ui(
            ns("export_avg1"),
            csv = TRUE,
            plot = FALSE,
            pin = FALSE
          )
        )
      )
    )
  )
}

#' Averages (1 Group) Server
#' @export
mod_averages_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session_code <- session$userData$sessionCode
    dropdown_container <- paste0("#", ns("averages_scope"))

    if (is.null(session_code)) {
      session_code <- "unknown"
    }

    output$tab4_variable2_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Tab4Variable2"),
        label = "Choose a numeric variable:",
        choices = names(rv$data)[startsWith(names(rv$data), "Num_")],
        multiple = FALSE,
        options = list(
          `style` = "btn-light btn-sm",
          `dropupAuto` = FALSE,
          `container` = dropdown_container,
          `size` = 8
        )
      )
    })

    tab4_data <- shiny::reactive({
      shiny::req(input$Tab4Variable1, input$Tab4Variable2, rv$data)

      logger::log_info(glue::glue(
        "[Session {session_code}] Tab 4: data process started for Variable: {input$Tab4Variable2}"
      ))

      data <- data_summary_one_variable(
        rv$data,
        input$Tab4Variable1,
        input$Tab4Variable2
      )

      logger::log_info(glue::glue(
        "[Session {session_code}] Tab 4: data process completed with {nrow(data)} rows"
      ))

      data
    })

    mod_shared_datatable_server(
      id = "dt_avg1",
      data_reactive = tab4_data,
      style_col = "n()",
      filter = "none",
      table_class = "compact hover stripe general-averages-table",
      dt_options = list(
        scrollY = "200px",
        pagingType = "simple_numbers",
        autoWidth = FALSE,
        dom = paste0(
          "<'row align-items-center mb-1'",
          "<'col-sm-6'l>",
          "<'col-sm-6'f>",
          ">",
          "t",
          "<'row align-items-center mt-1'",
          "<'col-sm-6'i>",
          "<'col-sm-6'p>",
          ">"
        )
      ),
      bg_colors = c(
        "rgba(220, 53, 69, 0.5)",
        "rgba(255, 193, 7, 0.5)",
        "rgba(40, 167, 69, 0.5)"
      )
    )

    mod_export_actions_server(
      id = "export_avg1",
      data_r = tab4_data,
      filename_prefix_r = shiny::reactive(input$Tab4Variable2)
    )
  })
}
