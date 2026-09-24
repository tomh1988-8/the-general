#' Lines (1 Group) UI
#' @export
mod_lines_ui <- function(id) {
  ns <- shiny::NS(id)
  dropdown_container <- paste0("#", ns("lines1_scope"))
  view_input <- ns("lines1_view")

  shiny::div(
    id = ns("lines1_scope"),
    class = "general-lines1-page",
    shiny::fluidRow(
      class = "general-lines1-layout-row",
      shiny::column(
        width = 2,
        shiny::div(
          class = "general-lines1-left-rail",
          shiny::div(
            class = "general-lines1-view-switcher",
            shiny::tabsetPanel(
              id = view_input,
              selected = "plot",
              shiny::tabPanel("Plot", value = "plot"),
              shiny::tabPanel("Table", value = "table")
            )
          ),
          shiny::div(
            class = "general-lines1-side-actions",
            mod_export_actions_ui(
              ns("export_10"),
              csv = TRUE,
              plot = TRUE,
              pin = TRUE,
              pin_id = "plot10_pin"
            )
          ),
          mod_axis_limits_ui(ns("y_axis_limits"))
        )
      ),
      shiny::column(
        width = 10,
        bs4Dash::bs4Card(
          title = "Lines · 1 Group",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          closable = FALSE,
          maximizable = TRUE,
          width = 12,
          shiny::fluidRow(
            class = "general-lines1-controls",
            shiny::column(
              width = 6,
              shiny::uiOutput(ns("tab10_variable1_picker"))
            ),
            shiny::column(
              width = 6,
              shinyWidgets::pickerInput(
                inputId = ns("TimeVariable"),
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
              ns("plot10_pin"),
              plotly::plotlyOutput
            )
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'table'", view_input),
            mod_shared_datatable_ui(ns("dt_lines1"))
          )
        )
      )
    )
  )
}

#' Lines (1 Group) Server
#' @export
mod_lines_server <- function(id, rv, pins) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session_code <- session$userData$sessionCode
    dropdown_container <- paste0("#", ns("lines1_scope"))

    if (is.null(session_code)) {
      session_code <- "unknown"
    }

    y_axis_limits <- mod_axis_limits_server("y_axis_limits")

    output$tab10_variable1_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Tab10Variable1"),
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

    table10_data <- shiny::reactive({
      shiny::req(input$Tab10Variable1, input$TimeVariable, rv$data)

      logger::log_info(glue::glue(
        "[Session {session_code}] Preparing data for plot 10"
      ))

      data_line_single_variable(
        rv$data,
        rv$unfiltered,
        input$TimeVariable,
        input$Tab10Variable1
      )
    })

    table10_plot <- shiny::reactive({
      shiny::req(input$TimeVariable, input$Tab10Variable1)

      limits <- y_axis_limits()

      logger::log_info(glue::glue(
        "[Session {session_code}] Preparing plot 10"
      ))

      plot_line_single_variable(
        table10_data(),
        input$TimeVariable,
        input$Tab10Variable1,
        input$TimeVariable,
        y_min = limits$min,
        y_max = limits$max
      )
    })

    pinModuleServer(
      "plot10_pin",
      render_fn = function(output, session, out_id) {
        output[[out_id]] <- plotly::renderPlotly({
          logger::log_info(glue::glue(
            "[Session {session_code}] Rendering plot 10 as Plotly"
          ))

          plotly::ggplotly(table10_plot(), tooltip = c("x", "y"))
        })
      },
      type = "line_single",
      params_r = shiny::reactive({
        limits <- y_axis_limits()

        list(
          TimeVariable = input$TimeVariable,
          NumericVariable = input$Tab10Variable1,
          YAxisMin = limits$min,
          YAxisMax = limits$max
        )
      }),
      title_r = shiny::reactive(glue::glue(
        "Line plot: {input$Tab10Variable1} over {input$TimeVariable}"
      )),
      pins = pins
    )

    mod_shared_datatable_server(
      id = "dt_lines1",
      data_reactive = table10_data,
      style_type = "single",
      style_col = "n()",
      filter = "none",
      table_class = "compact hover stripe general-lines1-table",
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
      id = "export_10",
      data_r = table10_data,
      plot_r = table10_plot,
      filename_prefix_r = shiny::reactive(input$Tab10Variable1)
    )
  })
}
