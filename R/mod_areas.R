#' Areas (Stacked) UI
#' @export
mod_areas_ui <- function(id) {
  ns <- shiny::NS(id)
  dropdown_container <- paste0("#", ns("areas1_scope"))
  view_input <- ns("areas1_view")

  shiny::div(
    id = ns("areas1_scope"),
    class = "general-areas1-page",
    shiny::fluidRow(
      class = "general-areas1-layout-row",
      shiny::column(
        width = 2,
        shiny::div(
          class = "general-areas1-left-rail",
          shiny::div(
            class = "general-areas1-view-switcher",
            shiny::tabsetPanel(
              id = view_input,
              selected = "plot",
              shiny::tabPanel("Plot", value = "plot"),
              shiny::tabPanel("Table", value = "table")
            )
          ),
          shiny::div(
            class = "general-areas1-side-actions",
            mod_export_actions_ui(
              ns("export_15"),
              csv = TRUE,
              plot = TRUE,
              pin = TRUE,
              pin_id = "plot15_pin"
            )
          ),
          mod_axis_limits_ui(ns("y_axis_limits"))
        )
      ),
      shiny::column(
        width = 10,
        bs4Dash::bs4Card(
          title = "Areas · Stacked",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          closable = FALSE,
          maximizable = TRUE,
          width = 12,
          shiny::fluidRow(
            class = "general-areas1-controls",
            shiny::column(
              width = 4,
              shiny::uiOutput(ns("tab15_variable1_picker"))
            ),
            shiny::column(
              width = 4,
              shiny::uiOutput(ns("tab15_variable2_picker"))
            ),
            shiny::column(
              width = 4,
              shinyWidgets::pickerInput(
                inputId = ns("TimeVariable6"),
                label = "Choose a time variable:",
                choices = c("Year_Quarter"),
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
              ns("plot15_pin"),
              plotly::plotlyOutput
            )
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'table'", view_input),
            mod_shared_datatable_ui(ns("dt_areas1"))
          )
        )
      )
    )
  )
}

#' Areas (Stacked) Server
#' @export
mod_areas_server <- function(id, rv, pins) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session_code <- session$userData$sessionCode
    dropdown_container <- paste0("#", ns("areas1_scope"))

    if (is.null(session_code)) {
      session_code <- "unknown"
    }

    y_axis_limits <- mod_axis_limits_server("y_axis_limits")

    output$tab15_variable1_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Tab15Variable1"),
        label = "Choose a grouping variable:",
        choices = names(rv$data)[
          startsWith(names(rv$data), "Cat_") &
            names(rv$data) != "Cat_Event_Number"
        ],
        multiple = FALSE,
        options = list(
          `style` = "btn-light btn-sm",
          `dropupAuto` = FALSE,
          `container` = dropdown_container,
          `size` = 8
        )
      )
    })

    output$tab15_variable2_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Tab15Variable2"),
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

    table15_data <- shiny::reactive({
      shiny::req(
        input$Tab15Variable1,
        input$Tab15Variable2,
        input$TimeVariable6,
        rv$data
      )

      logger::log_info(glue::glue(
        "[Session {session_code}] Processing data for table 15"
      ))

      data_stacked_area(
        data = rv$data,
        time_var = input$TimeVariable6,
        group_var = input$Tab15Variable1,
        num_var = input$Tab15Variable2
      )
    })

    table15_plot <- shiny::reactive({
      shiny::req(
        input$Tab15Variable1,
        input$Tab15Variable2,
        input$TimeVariable6
      )

      limits <- y_axis_limits()

      logger::log_info(glue::glue(
        "[Session {session_code}] Generating plot for table 15"
      ))

      plot_stacked_area(
        df = table15_data(),
        time_var = input$TimeVariable6,
        group_var = input$Tab15Variable1,
        num_var = input$Tab15Variable2,
        y_min = limits$min,
        y_max = limits$max
      )
    })

    pinModuleServer(
      "plot15_pin",
      render_fn = function(output, session, out_id) {
        output[[out_id]] <- plotly::renderPlotly({
          shiny::req(
            input$Tab15Variable1,
            input$Tab15Variable2,
            input$TimeVariable6
          )

          logger::log_info(glue::glue(
            "[Session {session_code}] Rendering Plotly for table 15"
          ))

          plotly::ggplotly(
            table15_plot(),
            tooltip = c("x", "y", "fill")
          )
        })
      },
      type = "area_stacked",
      params_r = shiny::reactive({
        limits <- y_axis_limits()

        list(
          TimeVariable6 = input$TimeVariable6,
          GroupingVar = input$Tab15Variable1,
          NumericVar = input$Tab15Variable2,
          YAxisMin = limits$min,
          YAxisMax = limits$max
        )
      }),
      title_r = shiny::reactive(glue::glue(
        "Stacked area: {input$Tab15Variable2} by {input$Tab15Variable1} over {input$TimeVariable6}"
      )),
      pins = pins
    )

    mod_shared_datatable_server(
      id = "dt_areas1",
      data_reactive = table15_data,
      style_type = "single",
      style_col = "n()",
      filter = "none",
      table_class = "compact hover stripe general-areas1-table",
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
      id = "export_15",
      data_r = table15_data,
      plot_r = table15_plot,
      filename_prefix_r = shiny::reactive(input$Tab15Variable1)
    )
  })
}
