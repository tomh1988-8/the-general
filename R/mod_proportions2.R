#' Percentages (2 Groups) UI
#' @export
mod_proportions2_ui <- function(id) {
  ns <- shiny::NS(id)
  dropdown_container <- paste0("#", ns("proportions2_scope"))

  shiny::div(
    id = ns("proportions2_scope"),
    class = "general-proportions2-page",
    bs4Dash::bs4Card(
      title = "Percentages · 2 Groups",
      status = "info",
      solidHeader = TRUE,
      collapsible = TRUE,
      closable = FALSE,
      maximizable = TRUE,
      width = 12,
      shiny::fluidRow(
        class = "general-proportions2-controls",
        shiny::column(
          width = 4,
          shinyWidgets::pickerInput(
            inputId = ns("Tab3Variable1"),
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
          shiny::uiOutput(ns("tab3_variable2_picker"))
        ),
        shiny::column(
          width = 4,
          shiny::uiOutput(ns("tab3_variable3_picker"))
        )
      ),
      mod_shared_datatable_ui(ns("dt_prop2")),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          mod_export_actions_ui(
            ns("export_prop2"),
            csv = TRUE,
            plot = FALSE,
            pin = FALSE
          )
        )
      )
    )
  )
}

#' Proportions (2 Groups) Server
#' @export
mod_proportions2_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session_code <- session$userData$sessionCode
    dropdown_container <- paste0("#", ns("proportions2_scope"))

    if (is.null(session_code)) {
      session_code <- "unknown"
    }

    output$tab3_variable2_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Tab3Variable2"),
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

    output$tab3_variable3_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Tab3Variable3"),
        label = "Choose a grouping variable:",
        choices = rev(names(rv$data)[startsWith(names(rv$data), "Cat_")]),
        multiple = FALSE,
        options = list(
          `style` = "btn-light btn-sm",
          `dropupAuto` = FALSE,
          `container` = dropdown_container,
          `size` = 8
        )
      )
    })

    tab3_data <- shiny::reactive({
      shiny::req(
        input$Tab3Variable1,
        input$Tab3Variable2,
        input$Tab3Variable3,
        rv$data
      )

      logger::log_info(glue::glue(
        "[Session {session_code}] Tab 3: Generating table data for variables {input$Tab3Variable1}, {input$Tab3Variable2}, and {input$Tab3Variable3}"
      ))

      data <- data_prop_table_two_groups(
        rv$data,
        input$Tab3Variable1,
        input$Tab3Variable2,
        input$Tab3Variable3
      )

      logger::log_info(glue::glue(
        "[Session {session_code}] Tab 3: Table data generated successfully"
      ))

      data
    })

    mod_shared_datatable_server(
      id = "dt_prop2",
      data_reactive = tab3_data,
      style_type = "proportions",
      filter = "none",
      table_class = "compact hover stripe general-proportions2-table",
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
      id = "export_prop2",
      data_r = tab3_data,
      filename_prefix_r = shiny::reactive(input$Tab3Variable2)
    )
  })
}
