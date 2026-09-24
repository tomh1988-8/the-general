#' Scatterplots UI
#' @export
mod_scatterplots_ui <- function(id) {
  ns <- shiny::NS(id)
  dropdown_container <- paste0("#", ns("scatterplots_scope"))
  view_input <- ns("scatterplots_view")

  shiny::div(
    id = ns("scatterplots_scope"),
    class = "general-scatterplots-page",
    shiny::fluidRow(
      class = "general-scatterplots-layout-row",
      shiny::column(
        width = 2,
        shiny::div(
          class = "general-scatterplots-left-rail",
          shiny::div(
            class = "general-scatterplots-view-switcher",
            shiny::tabsetPanel(
              id = view_input,
              selected = "plot",
              shiny::tabPanel("Plot", value = "plot"),
              shiny::tabPanel("Table", value = "table")
            )
          ),
          shiny::div(
            class = "general-scatterplots-side-actions",
            mod_export_actions_ui(
              ns("export_scatter"),
              csv = TRUE,
              plot = TRUE,
              pin = TRUE,
              pin_id = "plot19_pin"
            )
          ),
          mod_axis_limits_ui(ns("y_axis_limits"))
        )
      ),
      shiny::column(
        width = 10,
        bs4Dash::bs4Card(
          title = "Scatterplots",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          closable = FALSE,
          maximizable = TRUE,
          width = 12,
          shiny::fluidRow(
            class = "general-scatterplots-controls",
            shiny::column(
              width = 4,
              shiny::uiOutput(ns("tab19_variable1_picker"))
            ),
            shiny::column(
              width = 4,
              shiny::uiOutput(ns("tab19_variable2_picker"))
            ),
            shiny::column(
              width = 4,
              shiny::uiOutput(ns("tab19_variable3_picker"))
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
              ns("plot19_pin"),
              plotly::plotlyOutput
            )
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'table'", view_input),
            mod_shared_datatable_ui(ns("dt_scatterplots"))
          )
        )
      )
    )
  )
}

#' Scatterplots Server
#' @export
mod_scatterplots_server <- function(id, rv, pins) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session_code <- session$userData$sessionCode
    dropdown_container <- paste0("#", ns("scatterplots_scope"))

    if (is.null(session_code)) {
      session_code <- "unknown"
    }

    y_axis_limits <- mod_axis_limits_server("y_axis_limits")

    output$tab19_variable1_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Tab19Variable1"),
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

    output$tab19_variable2_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Tab19Variable2"),
        label = "Choose X-axis variable",
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

    output$tab19_variable3_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Tab19Variable3"),
        label = "Choose Y-axis variable",
        choices = rev(names(rv$data)[startsWith(names(rv$data), "Num_")]),
        multiple = FALSE,
        options = list(
          `style` = "btn-light btn-sm",
          `dropupAuto` = FALSE,
          `container` = dropdown_container,
          `size` = 8
        )
      )
    })

    table19_data <- shiny::reactive({
      shiny::req(
        input$Tab19Variable1,
        input$Tab19Variable2,
        input$Tab19Variable3,
        rv$data
      )

      logger::log_info(glue::glue(
        "[Session {session_code}] Filtering for scatter: {input$Tab19Variable1}, {input$Tab19Variable2}, {input$Tab19Variable3}"
      ))

      data_scatter_grouped(
        rv$data,
        input$Tab19Variable1,
        input$Tab19Variable2,
        input$Tab19Variable3
      )
    })

    table19_plot <- shiny::reactive({
      shiny::req(
        input$Tab19Variable1,
        input$Tab19Variable2,
        input$Tab19Variable3,
        rv$data
      )

      limits <- y_axis_limits()

      logger::log_info(glue::glue(
        "[Session {session_code}] Building ggplot for scatter"
      ))

      plot_scatter_grouped(
        table19_data(),
        input$Tab19Variable1,
        input$Tab19Variable2,
        input$Tab19Variable3,
        y_min = limits$min,
        y_max = limits$max
      )
    })

    pinModuleServer(
      "plot19_pin",
      render_fn = function(output, session, out_id) {
        output[[out_id]] <- plotly::renderPlotly({
          shiny::req(
            input$Tab19Variable1,
            input$Tab19Variable2,
            input$Tab19Variable3
          )

          logger::log_info(glue::glue(
            "[Session {session_code}] Rendering Plotly for scatter"
          ))

          plotly::ggplotly(
            table19_plot(),
            tooltip = c("x", "y", "colour")
          )
        })
      },
      type = "scatter",
      params_r = shiny::reactive({
        limits <- y_axis_limits()

        list(
          GroupVar = input$Tab19Variable1,
          X = input$Tab19Variable2,
          Y = input$Tab19Variable3,
          YAxisMin = limits$min,
          YAxisMax = limits$max
        )
      }),
      title_r = shiny::reactive(glue::glue(
        "Scatter: {input$Tab19Variable2} vs {input$Tab19Variable3} by {input$Tab19Variable1}"
      )),
      pins = pins
    )

    mod_shared_datatable_server(
      id = "dt_scatterplots",
      data_reactive = table19_data,
      style_type = "single",
      style_col = "n()",
      filter = "none",
      table_class = "compact hover stripe general-scatterplots-table",
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
      id = "export_scatter",
      data_r = table19_data,
      plot_r = table19_plot,
      filename_prefix_r = shiny::reactive(paste0(
        input$Tab19Variable1,
        "_scatter_data"
      ))
    )
  })
}
