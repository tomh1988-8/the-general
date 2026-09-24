#' Bars (Grouped) UI
#' @export
mod_bars3_ui <- function(id) {
  ns <- shiny::NS(id)
  dropdown_container <- paste0("#", ns("bars3_scope"))
  view_input <- ns("bars3_view")

  shiny::div(
    id = ns("bars3_scope"),
    class = "general-bars3-page",
    shiny::fluidRow(
      class = "general-bars3-layout-row",
      shiny::column(
        width = 2,
        shiny::div(
          class = "general-bars3-left-rail",
          shiny::div(
            class = "general-bars3-view-switcher",
            shiny::tabsetPanel(
              id = view_input,
              selected = "plot",
              shiny::tabPanel("Plot", value = "plot"),
              shiny::tabPanel("Table", value = "table")
            )
          ),
          shiny::div(
            class = "general-bars3-side-actions",
            mod_export_actions_ui(
              ns("export_14"),
              csv = TRUE,
              plot = TRUE,
              pin = TRUE,
              pin_id = "plot14_pin"
            )
          ),
          mod_axis_limits_ui(ns("y_axis_limits"))
        )
      ),
      shiny::column(
        width = 10,
        bs4Dash::bs4Card(
          title = "Bars · Grouped",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          closable = FALSE,
          maximizable = TRUE,
          width = 12,
          shiny::fluidRow(
            class = "general-bars3-controls",
            shiny::column(
              width = 3,
              shiny::uiOutput(ns("tab14_variable1_picker"))
            ),
            shiny::column(
              width = 3,
              shiny::uiOutput(ns("tab14_variable2_picker"))
            ),
            shiny::column(
              width = 3,
              shiny::uiOutput(ns("tab14_variable3_picker"))
            ),
            shiny::column(
              width = 3,
              shinyWidgets::pickerInput(
                inputId = ns("TimeVariable5"),
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
              ns("plot14_pin"),
              plotly::plotlyOutput
            )
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'table'", view_input),
            mod_shared_datatable_ui(ns("dt_bars3"))
          )
        )
      )
    )
  )
}

#' Bars (Grouped) Server
#' @export
mod_bars3_server <- function(id, rv, pins) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session_code <- session$userData$sessionCode
    dropdown_container <- paste0("#", ns("bars3_scope"))

    if (is.null(session_code)) {
      session_code <- "unknown"
    }

    y_axis_limits <- mod_axis_limits_server("y_axis_limits")

    output$tab14_variable1_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Tab14Variable1"),
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

    output$tab14_variable2_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Tab14Variable2"),
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

    output$tab14_variable3_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Tab14Variable3"),
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

    table14_data <- shiny::reactive({
      shiny::req(
        input$Tab14Variable1,
        input$Tab14Variable2,
        input$Tab14Variable3,
        input$TimeVariable5,
        rv$data
      )

      logger::log_info(glue::glue(
        "[Session {session_code}] Processing data for table 14"
      ))

      data_grouped_dodge_bar(
        rv$data,
        input$TimeVariable5,
        input$Tab14Variable1,
        input$Tab14Variable2,
        input$Tab14Variable3
      )
    })

    table14_plot <- shiny::reactive({
      shiny::req(
        input$Tab14Variable1,
        input$Tab14Variable2,
        input$Tab14Variable3
      )

      limits <- y_axis_limits()

      logger::log_info(glue::glue(
        "[Session {session_code}] Generating plot for table 14"
      ))

      p <- plot_grouped_dodge_bar(
        table14_data(),
        input$Tab14Variable1,
        input$Tab14Variable2,
        input$Tab14Variable3,
        y_min = limits$min,
        y_max = limits$max
      )

      if (identical(input$Tab14Variable1, "Cat_Income_Band")) {
        p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank())
      }

      p
    })

    pinModuleServer(
      "plot14_pin",
      render_fn = function(output, session, out_id) {
        output[[out_id]] <- plotly::renderPlotly({
          shiny::req(
            input$Tab14Variable1,
            input$Tab14Variable2,
            input$Tab14Variable3
          )

          logger::log_info(glue::glue(
            "[Session {session_code}] Rendering Plotly for table 14"
          ))

          plotly::ggplotly(
            table14_plot(),
            tooltip = c("x", "y", "fill")
          ) |>
            plotly::layout(showlegend = FALSE)
        })
      },
      type = "bar_grouped",
      params_r = shiny::reactive({
        limits <- y_axis_limits()

        list(
          TimeVariable5 = input$TimeVariable5,
          GroupVar1 = input$Tab14Variable1,
          GroupVar2 = input$Tab14Variable2,
          NumVar = input$Tab14Variable3,
          YAxisMin = limits$min,
          YAxisMax = limits$max
        )
      }),
      title_r = shiny::reactive(glue::glue(
        "Grouped bar: {input$Tab14Variable3} by {input$Tab14Variable2} & {input$Tab14Variable1} over {input$TimeVariable5}"
      )),
      pins = pins
    )

    mod_shared_datatable_server(
      id = "dt_bars3",
      data_reactive = table14_data,
      style_type = "single",
      style_col = "n()",
      filter = "none",
      table_class = "compact hover stripe general-bars3-table",
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
      id = "export_14",
      data_r = table14_data,
      plot_r = table14_plot,
      filename_prefix_r = shiny::reactive(input$Tab14Variable1)
    )
  })
}
