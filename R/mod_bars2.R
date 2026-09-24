#' Bars (Stacked) UI
#' @export
mod_bars2_ui <- function(id) {
  ns <- shiny::NS(id)
  dropdown_container <- paste0("#", ns("bars2_scope"))
  view_input <- ns("bars2_view")

  shiny::div(
    id = ns("bars2_scope"),
    class = "general-bars2-page",
    shiny::fluidRow(
      class = "general-bars2-layout-row",
      shiny::column(
        width = 2,
        shiny::div(
          class = "general-bars2-left-rail",
          shiny::div(
            class = "general-bars2-view-switcher",
            shiny::tabsetPanel(
              id = view_input,
              selected = "plot",
              shiny::tabPanel("Plot", value = "plot"),
              shiny::tabPanel("Table", value = "table")
            )
          ),
          shiny::div(
            class = "general-bars2-side-actions",
            mod_export_actions_ui(
              ns("export_13"),
              csv = TRUE,
              plot = TRUE,
              pin = TRUE,
              pin_id = "plot13_pin"
            )
          ),
          mod_axis_limits_ui(ns("y_axis_limits"))
        )
      ),
      shiny::column(
        width = 10,
        bs4Dash::bs4Card(
          title = "Bars · Stacked",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          closable = FALSE,
          maximizable = TRUE,
          width = 12,
          shiny::fluidRow(
            class = "general-bars2-controls",
            shiny::column(
              width = 3,
              shiny::uiOutput(ns("tab13_variable1_picker"))
            ),
            shiny::column(
              width = 3,
              shiny::uiOutput(ns("tab13_variable2_picker"))
            ),
            shiny::column(
              width = 3,
              shiny::uiOutput(ns("tab13_variable3_picker"))
            ),
            shiny::column(
              width = 3,
              shinyWidgets::pickerInput(
                inputId = ns("TimeVariable4"),
                label = "Time variable:",
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
              ns("plot13_pin"),
              plotly::plotlyOutput
            )
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'table'", view_input),
            mod_shared_datatable_ui(ns("dt_bars2"))
          )
        )
      )
    )
  )
}

#' Bars (Stacked) Server
#' @export
mod_bars2_server <- function(id, rv, pins) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session_code <- session$userData$sessionCode
    dropdown_container <- paste0("#", ns("bars2_scope"))

    if (is.null(session_code)) {
      session_code <- "unknown"
    }

    y_axis_limits <- mod_axis_limits_server("y_axis_limits")

    output$tab13_variable1_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Tab13Variable1"),
        label = "Grouping variable 1:",
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

    output$tab13_variable2_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Tab13Variable2"),
        label = "Grouping variable 2:",
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

    output$tab13_variable3_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Tab13Variable3"),
        label = "Numeric variable:",
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

    table13_data <- shiny::reactive({
      shiny::req(
        input$Tab13Variable1,
        input$Tab13Variable2,
        input$Tab13Variable3,
        input$TimeVariable4,
        rv$data
      )

      logger::log_info(glue::glue(
        "[Session {session_code}] Processing data for table 13"
      ))

      data_stacked_bar(
        rv$data,
        input$TimeVariable4,
        input$Tab13Variable1,
        input$Tab13Variable2,
        input$Tab13Variable3
      )
    })

    table13_plot <- shiny::reactive({
      shiny::req(
        input$Tab13Variable1,
        input$Tab13Variable2,
        input$Tab13Variable3
      )

      limits <- y_axis_limits()

      logger::log_info(glue::glue(
        "[Session {session_code}] Rendering plot for table 13"
      ))

      p <- plot_stacked_bar(
        table13_data(),
        input$Tab13Variable1,
        input$Tab13Variable2,
        input$Tab13Variable3,
        y_min = limits$min,
        y_max = limits$max
      )

      if (identical(input$Tab13Variable1, "Cat_Income_Band")) {
        p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank())
      }

      p
    })

    pinModuleServer(
      "plot13_pin",
      render_fn = function(output, session, out_id) {
        output[[out_id]] <- plotly::renderPlotly({
          shiny::req(
            input$Tab13Variable1,
            input$Tab13Variable2,
            input$Tab13Variable3
          )

          logger::log_info(glue::glue(
            "[Session {session_code}] Rendering plotly plot for table 13"
          ))

          plotly::ggplotly(
            table13_plot(),
            tooltip = c("x", "y", "fill")
          ) |>
            plotly::layout(showlegend = FALSE)
        })
      },
      type = "bar_stacked",
      params_r = shiny::reactive({
        limits <- y_axis_limits()

        list(
          TimeVariable4 = input$TimeVariable4,
          GroupVar1 = input$Tab13Variable1,
          GroupVar2 = input$Tab13Variable2,
          NumVar = input$Tab13Variable3,
          YAxisMin = limits$min,
          YAxisMax = limits$max
        )
      }),
      title_r = shiny::reactive(glue::glue(
        "Stacked bar: {input$Tab13Variable3} by {input$Tab13Variable2} & {input$Tab13Variable1} over {input$TimeVariable4}"
      )),
      pins = pins
    )

    mod_shared_datatable_server(
      id = "dt_bars2",
      data_reactive = table13_data,
      style_type = "single",
      style_col = "n()",
      filter = "none",
      table_class = "compact hover stripe general-bars2-table",
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
      id = "export_13",
      data_r = table13_data,
      plot_r = table13_plot,
      filename_prefix_r = shiny::reactive(input$Tab13Variable1)
    )
  })
}
