#' Frequencies UI
#' @export
mod_frequencies_ui <- function(id) {
  ns <- shiny::NS(id)
  dropdown_container <- paste0("#", ns("frequencies_scope"))

  shiny::div(
    id = ns("frequencies_scope"),
    class = "general-frequencies-page",
    bs4Dash::bs4Card(
      title = "Frequencies",
      status = "info",
      solidHeader = TRUE,
      collapsible = TRUE,
      closable = FALSE,
      maximizable = TRUE,
      width = 12,
      shiny::fluidRow(
        class = "general-frequencies-controls",
        shiny::column(
          width = 6,
          shinyWidgets::pickerInput(
            inputId = ns("Variable1"),
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
          shiny::uiOutput(ns("freq_variable2_picker"))
        )
      ),
      mod_shared_datatable_ui(ns("dt_freq")),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          mod_export_actions_ui(
            ns("export_freq"),
            csv = TRUE,
            plot = FALSE,
            pin = FALSE
          )
        )
      )
    )
  )
}

#' Frequencies Server
#' @export
mod_frequencies_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session_code <- session$userData$sessionCode
    dropdown_container <- paste0("#", ns("frequencies_scope"))

    if (is.null(session_code)) {
      session_code <- "unknown"
    }

    output$freq_variable2_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Variable2"),
        label = "Choose a grouping variable:",
        choices = names(rv$data)[startsWith(names(rv$data), "Cat_")],
        multiple = FALSE,
        options = list(
          `style` = "btn-light btn-sm",
          `dropupAuto` = FALSE,
          `container` = dropdown_container,
          `size` = 8
        )
      )
    })

    table_data <- shiny::reactive({
      shiny::req(input$Variable1, input$Variable2, rv$data)

      logger::log_info(glue::glue(
        "[Session {session_code}] Tab 1: Generating table data for variables {input$Variable1} and {input$Variable2}"
      ))

      data <- data_freq_table(rv$data, input$Variable1, input$Variable2)

      logger::log_info(glue::glue(
        "[Session {session_code}] Tab 1: Table data generated successfully"
      ))

      data
    })

    mod_shared_datatable_server(
      id = "dt_freq",
      data_reactive = table_data,
      style_col = "Freq",
      filter = "none",
      table_class = "compact hover stripe general-frequency-table",
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
      id = "export_freq",
      data_r = table_data,
      filename_prefix_r = shiny::reactive(input$Variable2)
    )
  })
}
