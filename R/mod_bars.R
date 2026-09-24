#' Bars (Simple) UI
#' @export
mod_bars_ui <- function(id) {
  ns <- shiny::NS(id)
  dropdown_container <- paste0("#", ns("bars1_scope"))
  view_input <- ns("bars1_view")

  shiny::div(
    id = ns("bars1_scope"),
    class = "general-bars1-page",
    shiny::fluidRow(
      class = "general-bars1-layout-row",
      shiny::column(
        width = 2,
        shiny::div(
          class = "general-bars1-left-rail",
          shiny::div(
            class = "general-bars1-view-switcher",
            shiny::tabsetPanel(
              id = view_input,
              selected = "plot",
              shiny::tabPanel("Plot", value = "plot"),
              shiny::tabPanel("Table", value = "table")
            )
          ),
          shiny::div(
            class = "general-bars1-side-actions",
            mod_export_actions_ui(
              ns("export_12"),
              csv = TRUE,
              plot = TRUE,
              pin = TRUE,
              pin_id = "plot12_pin"
            )
          ),
          mod_axis_limits_ui(ns("y_axis_limits"))
        )
      ),
      shiny::column(
        width = 10,
        bs4Dash::bs4Card(
          title = "Bars · Simple",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          closable = FALSE,
          maximizable = TRUE,
          width = 12,
          shiny::fluidRow(
            class = "general-bars1-controls",
            shiny::column(
              width = 4,
              shiny::uiOutput(ns("tab12_variable1_picker"))
            ),
            shiny::column(
              width = 4,
              shiny::uiOutput(ns("tab12_variable2_picker"))
            ),
            shiny::column(
              width = 4,
              shinyWidgets::pickerInput(
                inputId = ns("TimeVariable3"),
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
              ns("plot12_pin"),
              plotly::plotlyOutput
            )
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'table'", view_input),
            mod_shared_datatable_ui(ns("dt_bars1"))
          )
        )
      )
    )
  )
}

#' Bars (Simple) Server
#' @export
mod_bars_server <- function(id, rv, pins) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session_code <- session$userData$sessionCode
    dropdown_container <- paste0("#", ns("bars1_scope"))

    if (is.null(session_code)) {
      session_code <- "unknown"
    }

    y_axis_limits <- mod_axis_limits_server("y_axis_limits")

    output$tab12_variable1_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Tab12Variable1"),
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

    output$tab12_variable2_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Tab12Variable2"),
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

    table12_data <- shiny::reactive({
      shiny::req(
        input$Tab12Variable1,
        input$Tab12Variable2,
        input$TimeVariable3,
        rv$data
      )

      logger::log_info(glue::glue(
        "[Session {session_code}] Processing data for table 12"
      ))

      data_simple_bar(
        rv$data,
        input$TimeVariable3,
        input$Tab12Variable1,
        input$Tab12Variable2
      )
    })

    table12_plot <- shiny::reactive({
      shiny::req(input$Tab12Variable1, input$Tab12Variable2)

      limits <- y_axis_limits()

      logger::log_info(glue::glue(
        "[Session {session_code}] Rendering plot for table 12"
      ))

      p <- plot_simple_bar(
        table12_data(),
        input$Tab12Variable1,
        input$Tab12Variable2,
        y_min = limits$min,
        y_max = limits$max
      )

      if (identical(input$Tab12Variable1, "Cat_Income_Band")) {
        p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank())
      }

      p
    })

    pinModuleServer(
      "plot12_pin",
      render_fn = function(output, session, out_id) {
        output[[out_id]] <- plotly::renderPlotly({
          shiny::req(input$Tab12Variable1, input$Tab12Variable2)

          logger::log_info(glue::glue(
            "[Session {session_code}] Rendering plotly plot for table 12"
          ))

          plotly::ggplotly(
            table12_plot(),
            tooltip = c("x", "y", "fill")
          ) |>
            plotly::layout(showlegend = FALSE)
        })
      },
      type = "bar_simple",
      params_r = shiny::reactive({
        limits <- y_axis_limits()

        list(
          TimeVariable3 = input$TimeVariable3,
          GroupVar = input$Tab12Variable1,
          NumVar = input$Tab12Variable2,
          YAxisMin = limits$min,
          YAxisMax = limits$max
        )
      }),
      title_r = shiny::reactive(glue::glue(
        "Simple bar: {input$Tab12Variable2} by {input$Tab12Variable1} over {input$TimeVariable3}"
      )),
      pins = pins
    )

    mod_shared_datatable_server(
      id = "dt_bars1",
      data_reactive = table12_data,
      style_type = "single",
      style_col = "n()",
      filter = "none",
      table_class = "compact hover stripe general-bars1-table",
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
      id = "export_12",
      data_r = table12_data,
      plot_r = table12_plot,
      filename_prefix_r = shiny::reactive(input$Tab12Variable1)
    )
  })
}
