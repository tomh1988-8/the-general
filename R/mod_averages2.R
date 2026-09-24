#' Averages (2 Groups) UI
#' @export
mod_averages2_ui <- function(id) {
  ns <- shiny::NS(id)
  dropdown_container <- paste0("#", ns("averages2_scope"))

  shiny::div(
    id = ns("averages2_scope"),
    class = "general-averages2-page",
    bs4Dash::bs4Card(
      title = "Averages · 2 Groups",
      status = "info",
      solidHeader = TRUE,
      collapsible = TRUE,
      closable = FALSE,
      maximizable = TRUE,
      width = 12,
      shiny::fluidRow(
        class = "general-averages2-controls",
        shiny::column(
          width = 4,
          shinyWidgets::pickerInput(
            inputId = ns("Tab5Variable1"),
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
          width = 4,
          shiny::uiOutput(ns("tab5_variable2_picker"))
        ),
        shiny::column(
          width = 4,
          shiny::uiOutput(ns("tab5_variable3_picker"))
        )
      ),
      mod_shared_datatable_ui(ns("dt_avg2")),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          mod_export_actions_ui(
            ns("export_avg2"),
            csv = TRUE,
            plot = FALSE,
            pin = FALSE
          )
        )
      )
    )
  )
}

#' Averages (2 Groups) Server
#' @export
mod_averages2_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session_code <- session$userData$sessionCode
    dropdown_container <- paste0("#", ns("averages2_scope"))

    if (is.null(session_code)) {
      session_code <- "unknown"
    }

    output$tab5_variable2_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Tab5Variable2"),
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

    output$tab5_variable3_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Tab5Variable3"),
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

    tab5_data <- shiny::reactive({
      shiny::req(
        input$Tab5Variable1,
        input$Tab5Variable2,
        input$Tab5Variable3,
        rv$data
      )

      logger::log_info(glue::glue(
        "[Session {session_code}] Tab 5: data process started for Variable: {input$Tab5Variable3}"
      ))

      data <- data_summary_grouped(
        rv$data,
        input$Tab5Variable1,
        input$Tab5Variable2,
        input$Tab5Variable3
      )

      logger::log_info(glue::glue(
        "[Session {session_code}] Tab 5: data process completed with {nrow(data)} rows"
      ))

      data
    })

    mod_shared_datatable_server(
      id = "dt_avg2",
      data_reactive = tab5_data,
      style_col = "n()",
      filter = "none",
      table_class = "compact hover stripe general-averages2-table",
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
      id = "export_avg2",
      data_r = tab5_data,
      filename_prefix_r = shiny::reactive(input$Tab5Variable3)
    )
  })
}
