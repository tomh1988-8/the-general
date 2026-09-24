#' Percentages (1 Group) UI
#' @export
mod_proportions_ui <- function(id) {
  ns <- shiny::NS(id)
  dropdown_container <- paste0("#", ns("proportions_scope"))

  shiny::div(
    id = ns("proportions_scope"),
    class = "general-proportions-page",
    bs4Dash::bs4Card(
      title = "Percentages · 1 Group",
      status = "info",
      solidHeader = TRUE,
      collapsible = TRUE,
      closable = FALSE,
      maximizable = TRUE,
      width = 12,
      shiny::fluidRow(
        class = "general-proportions-controls",
        shiny::column(
          width = 6,
          shinyWidgets::pickerInput(
            inputId = ns("Tab2Variable1"),
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
          shiny::uiOutput(ns("tab2_variable2_picker"))
        )
      ),
      mod_shared_datatable_ui(ns("dt_prop1")),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          mod_export_actions_ui(
            ns("export_prop1"),
            csv = TRUE,
            plot = FALSE,
            pin = FALSE
          )
        )
      )
    )
  )
}

#' Proportions (1 Group) Server
#' @export
mod_proportions_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session_code <- session$userData$sessionCode
    dropdown_container <- paste0("#", ns("proportions_scope"))

    if (is.null(session_code)) {
      session_code <- "unknown"
    }

    output$tab2_variable2_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Tab2Variable2"),
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

    tab2_data <- shiny::reactive({
      shiny::req(input$Tab2Variable1, input$Tab2Variable2, rv$data)

      logger::log_info(glue::glue(
        "[Session {session_code}] Tab 2: Generating table data for variables {input$Tab2Variable1} and {input$Tab2Variable2}"
      ))

      data <- data_prop_table_one_group(
        rv$data,
        input$Tab2Variable1,
        input$Tab2Variable2
      )

      logger::log_info(glue::glue(
        "[Session {session_code}] Tab 2: Table data generated successfully"
      ))

      data
    })

    mod_shared_datatable_server(
      id = "dt_prop1",
      data_reactive = tab2_data,
      style_type = "proportions",
      filter = "none",
      table_class = "compact hover stripe general-proportions-table",
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
      id = "export_prop1",
      data_r = tab2_data,
      filename_prefix_r = shiny::reactive(input$Tab2Variable2)
    )
  })
}
