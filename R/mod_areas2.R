#' Areas (%) UI
#' @export
mod_areas2_ui <- function(id) {
  ns <- shiny::NS(id)
  dropdown_container <- paste0("#", ns("areas2_scope"))
  view_input <- ns("areas2_view")

  shiny::div(
    id = ns("areas2_scope"),
    class = "general-areas2-page",
    shiny::fluidRow(
      class = "general-areas2-layout-row",
      shiny::column(
        width = 2,
        shiny::div(
          class = "general-areas2-left-rail",
          shiny::div(
            class = "general-areas2-view-switcher",
            shiny::tabsetPanel(
              id = view_input,
              selected = "plot",
              shiny::tabPanel("Plot", value = "plot"),
              shiny::tabPanel("Table", value = "table")
            )
          ),
          shiny::div(
            class = "general-areas2-side-actions",
            mod_export_actions_ui(
              ns("export_16"),
              csv = TRUE,
              plot = TRUE,
              pin = TRUE,
              pin_id = "plot16_pin"
            )
          ),
          mod_axis_limits_ui(ns("y_axis_limits"))
        )
      ),
      shiny::column(
        width = 10,
        bs4Dash::bs4Card(
          title = "Areas · Percentage",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          closable = FALSE,
          maximizable = TRUE,
          width = 12,
          shiny::fluidRow(
            class = "general-areas2-controls",
            shiny::column(
              width = 6,
              shiny::uiOutput(ns("tab16_variable1_picker"))
            ),
            shiny::column(
              width = 6,
              shinyWidgets::pickerInput(
                inputId = ns("TimeVariable7"),
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
              ns("plot16_pin"),
              plotly::plotlyOutput
            )
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'table'", view_input),
            mod_shared_datatable_ui(ns("dt_areas2"))
          )
        )
      )
    )
  )
}

#' Areas (%) Server
#' @export
mod_areas2_server <- function(id, rv, pins) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session_code <- session$userData$sessionCode
    dropdown_container <- paste0("#", ns("areas2_scope"))

    if (is.null(session_code)) {
      session_code <- "unknown"
    }

    y_axis_limits <- mod_axis_limits_server("y_axis_limits")

    output$tab16_variable1_picker <- shiny::renderUI({
      shiny::req(rv$data)

      opts <- names(rv$data)[startsWith(names(rv$data), "Cat_")]

      shinyWidgets::pickerInput(
        inputId = ns("Tab16Variable1"),
        label = "Choose a grouping variable:",
        choices = opts,
        selected = opts[1],
        multiple = FALSE,
        options = list(
          `style` = "btn-light btn-sm",
          `dropupAuto` = FALSE,
          `container` = dropdown_container,
          `size` = 8
        )
      )
    })

    table16_data <- shiny::reactive({
      shiny::req(
        input$Tab16Variable1,
        input$TimeVariable7,
        rv$data
      )

      logger::log_info(glue::glue(
        "[Session {session_code}] Processing data for table 16"
      ))

      time_var <- if (input$TimeVariable7 == "Year") {
        "Year"
      } else {
        "Year_Quarter"
      }

      data_proportional_area(
        data = rv$data,
        time_var = time_var,
        group_var = input$Tab16Variable1
      )
    })

    table16_plot <- shiny::reactive({
      shiny::req(input$Tab16Variable1, input$TimeVariable7)

      limits <- y_axis_limits()

      logger::log_info(glue::glue(
        "[Session {session_code}] Generating plot for table 16"
      ))

      plot_proportional_area(
        table16_data(),
        y_min = limits$min,
        y_max = limits$max
      )
    })

    pinModuleServer(
      "plot16_pin",
      render_fn = function(output, session, out_id) {
        output[[out_id]] <- plotly::renderPlotly({
          shiny::req(input$Tab16Variable1, input$TimeVariable7)

          logger::log_info(glue::glue(
            "[Session {session_code}] Rendering Plotly plot for table 16"
          ))

          plotly::ggplotly(table16_plot(), tooltip = c("x", "y", "fill"))
        })
      },
      type = "area_percent",
      params_r = shiny::reactive({
        limits <- y_axis_limits()

        list(
          TimeVariable7 = input$TimeVariable7,
          GroupingVar = input$Tab16Variable1,
          YAxisMin = limits$min,
          YAxisMax = limits$max
        )
      }),
      title_r = shiny::reactive(glue::glue(
        "Proportional area: {input$Tab16Variable1} over {input$TimeVariable7}"
      )),
      pins = pins
    )

    mod_shared_datatable_server(
      id = "dt_areas2",
      data_reactive = table16_data,
      style_type = "single",
      style_col = "n",
      filter = "none",
      table_class = "compact hover stripe general-areas2-table",
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
      id = "export_16",
      data_r = table16_data,
      plot_r = table16_plot,
      filename_prefix_r = shiny::reactive(input$Tab16Variable1)
    )
  })
}
