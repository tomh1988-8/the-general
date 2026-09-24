#' Lines (2 Groups) UI
#' @export
mod_lines2_ui <- function(id) {
  ns <- shiny::NS(id)
  dropdown_container <- paste0("#", ns("lines2_scope"))
  view_input <- ns("lines2_view")

  shiny::div(
    id = ns("lines2_scope"),
    class = "general-lines2-page",
    shiny::fluidRow(
      class = "general-lines2-layout-row",
      shiny::column(
        width = 2,
        shiny::div(
          class = "general-lines2-left-rail",
          shiny::div(
            class = "general-lines2-view-switcher",
            shiny::tabsetPanel(
              id = view_input,
              selected = "plot",
              shiny::tabPanel("Plot", value = "plot"),
              shiny::tabPanel("Table", value = "table")
            )
          ),
          shiny::div(
            class = "general-lines2-side-actions",
            mod_export_actions_ui(
              ns("export_11"),
              csv = TRUE,
              plot = TRUE,
              pin = TRUE,
              pin_id = "plot11_pin"
            )
          ),
          mod_axis_limits_ui(ns("y_axis_limits"))
        )
      ),
      shiny::column(
        width = 10,
        bs4Dash::bs4Card(
          title = "Lines · 2 Groups",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          closable = FALSE,
          maximizable = TRUE,
          width = 12,
          shiny::fluidRow(
            class = "general-lines2-controls",
            shiny::column(
              width = 4,
              shiny::uiOutput(ns("tab11_variable1_picker"))
            ),
            shiny::column(
              width = 4,
              shiny::uiOutput(ns("tab11_variable2_picker"))
            ),
            shiny::column(
              width = 4,
              shinyWidgets::pickerInput(
                inputId = ns("TimeVariable2"),
                label = "Choose a time variable:",
                choices = c("Financial_Quarter", "Month"),
                multiple = FALSE,
                options = list(
                  `style` = "btn-light btn-sm",
                  `dropupAuto` = FALSE,
                  `container` = dropdown_container,
                  `size` = 8
                )
              )
            )
          ),
          shiny::conditionalPanel(
            condition = sprintf(
              "typeof input['%s'] === 'undefined' || input['%s'] === null || input['%s'] == 'plot'",
              view_input,
              view_input,
              view_input
            ),
            pinModuleUI(
              ns("plot11_pin"),
              plotly::plotlyOutput
            )
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'table'", view_input),
            mod_shared_datatable_ui(ns("dt_lines2"))
          )
        )
      )
    )
  )
}

#' Lines (2 Groups) Server
#' @export
mod_lines2_server <- function(id, rv, pins) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session_code <- session$userData$sessionCode
    dropdown_container <- paste0("#", ns("lines2_scope"))

    if (is.null(session_code)) {
      session_code <- "unknown"
    }

    y_axis_limits <- mod_axis_limits_server("y_axis_limits")

    output$tab11_variable1_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Tab11Variable1"),
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

    output$tab11_variable2_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Tab11Variable2"),
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

    table11_data <- shiny::reactive({
      shiny::req(
        input$TimeVariable2,
        input$Tab11Variable1,
        input$Tab11Variable2,
        rv$data,
        rv$unfiltered
      )

      logger::log_info(glue::glue(
        "[Session {session_code}] Processing data for table 11"
      ))

      data_grouped_lines(
        rv$data,
        rv$unfiltered,
        input$TimeVariable2,
        input$Tab11Variable1,
        input$Tab11Variable2
      )
    })

    table11_plot <- shiny::reactive({
      shiny::req(
        input$Tab11Variable1,
        input$Tab11Variable2,
        input$TimeVariable2
      )

      limits <- y_axis_limits()

      logger::log_info(glue::glue(
        "[Session {session_code}] Rendering plot for table 11"
      ))

      plot_grouped_lines(
        table11_data(),
        input$TimeVariable2,
        input$Tab11Variable1,
        input$Tab11Variable2,
        input$TimeVariable2,
        y_min = limits$min,
        y_max = limits$max
      )
    })

    pinModuleServer(
      "plot11_pin",
      render_fn = function(output, session, out_id) {
        output[[out_id]] <- plotly::renderPlotly({
          shiny::req(
            input$Tab11Variable1,
            input$Tab11Variable2,
            input$TimeVariable2
          )

          logger::log_info(glue::glue(
            "[Session {session_code}] Rendering plotly plot for table 11"
          ))

          plotly::ggplotly(
            table11_plot(),
            tooltip = c("x", "y", "colour")
          )
        })
      },
      type = "line_grouped",
      params_r = shiny::reactive({
        limits <- y_axis_limits()

        list(
          TimeVariable2 = input$TimeVariable2,
          GroupVar = input$Tab11Variable1,
          NumVar = input$Tab11Variable2,
          YAxisMin = limits$min,
          YAxisMax = limits$max
        )
      }),
      title_r = shiny::reactive(glue::glue(
        "Grouped line: {input$Tab11Variable2} by {input$Tab11Variable1} over {input$TimeVariable2}"
      )),
      pins = pins
    )

    mod_shared_datatable_server(
      id = "dt_lines2",
      data_reactive = table11_data,
      style_type = "single",
      style_col = "n()",
      filter = "none",
      table_class = "compact hover stripe general-lines2-table",
      dt_options = list(
        scrollY = "260px",
        pagingType = "simple_numbers",
        autoWidth = FALSE,
        dom = paste0(
          "<'row align-items-center mb-1'",
          "<'col-sm-6'l>",
          "<'col-sm-6'f>",
          ">",
          "t",
          "<'row align-items-center mt-1'",
          "<'col-sm-5'i>",
          "<'col-sm-7'p>",
          ">"
        )
      )
    )

    mod_export_actions_server(
      id = "export_11",
      data_r = table11_data,
      plot_r = table11_plot,
      filename_prefix_r = shiny::reactive(input$Tab11Variable1)
    )
  })
}
