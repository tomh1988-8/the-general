#' Density UI
#' @export
mod_density_ui <- function(id) {
  ns <- shiny::NS(id)
  dropdown_container <- paste0("#", ns("density_scope"))
  view_input <- ns("density_view")

  shiny::div(
    id = ns("density_scope"),
    class = "general-density-page",
    shiny::fluidRow(
      class = "general-density-layout-row",
      shiny::column(
        width = 2,
        shiny::div(
          class = "general-density-left-rail",
          shiny::div(
            class = "general-density-view-switcher",
            shiny::tabsetPanel(
              id = view_input,
              selected = "plot",
              shiny::tabPanel("Plot", value = "plot"),
              shiny::tabPanel("Table", value = "table")
            )
          ),
          shiny::div(
            class = "general-density-side-actions",
            mod_export_actions_ui(
              ns("export_density"),
              csv = TRUE,
              plot = TRUE,
              pin = TRUE,
              pin_id = "plot17_pin"
            )
          )
        )
      ),
      shiny::column(
        width = 10,
        bs4Dash::bs4Card(
          title = "Density",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          closable = FALSE,
          maximizable = TRUE,
          width = 12,
          shiny::fluidRow(
            class = "general-density-controls",
            shiny::column(
              width = 4,
              shiny::uiOutput(ns("tab17_variable1_picker"))
            ),
            shiny::column(
              width = 4,
              shiny::uiOutput(ns("tab17_variable2_picker"))
            ),
            shiny::column(
              width = 4,
              shiny::uiOutput(ns("tab17_variable3_picker"))
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
              ns("plot17_pin"),
              plotly::plotlyOutput
            )
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'table'", view_input),
            mod_shared_datatable_ui(ns("dt_density"))
          )
        )
      )
    )
  )
}

#' Density Server
#' @export
mod_density_server <- function(id, rv, pins) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session_code <- session$userData$sessionCode
    dropdown_container <- paste0("#", ns("density_scope"))

    if (is.null(session_code)) {
      session_code <- "unknown"
    }

    output$tab17_variable1_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Tab17Variable1"),
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

    output$tab17_variable2_picker <- shiny::renderUI({
      shiny::req(rv$data)

      shinyWidgets::pickerInput(
        inputId = ns("Tab17Variable2"),
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

    output$tab17_variable3_picker <- shiny::renderUI({
      shiny::req(rv$data)
      shiny::req("Financial_Year" %in% names(rv$data))

      year_choices <- rv$data$Financial_Year |>
        as.character() |>
        unique() |>
        stats::na.omit() |>
        sort()

      shiny::req(length(year_choices) > 0L)

      logger::log_info(glue::glue(
        "[Session {session_code}] Tab17: Rendering financial year choices: {paste(year_choices, collapse = ', ')}"
      ))

      shinyWidgets::pickerInput(
        inputId = ns("Tab17Variable3"),
        label = "Select a year:",
        choices = year_choices,
        selected = year_choices[[length(year_choices)]],
        multiple = FALSE,
        options = list(
          `style` = "btn-light btn-sm",
          `dropupAuto` = FALSE,
          `container` = dropdown_container,
          `size` = 8
        )
      )
    })

    table17_data <- shiny::reactive({
      shiny::req(
        input$Tab17Variable1,
        input$Tab17Variable2,
        input$Tab17Variable3,
        rv$data
      )

      logger::log_info(glue::glue(
        "[Session {session_code}] Tab17: Processing density data | group={input$Tab17Variable1} | numeric={input$Tab17Variable2} | financial_year={input$Tab17Variable3}"
      ))

      out <- data_density(
        data = rv$data,
        group_var = input$Tab17Variable1,
        num_var = input$Tab17Variable2,
        fy_choice = as.character(input$Tab17Variable3)
      )

      logger::log_info(glue::glue(
        "[Session {session_code}] Tab17: Density data ready | rows={nrow(out)}"
      ))

      out
    })

    table17_plot <- shiny::reactive({
      shiny::req(nrow(table17_data()) > 0L)

      logger::log_info(glue::glue(
        "[Session {session_code}] Tab17: Generating density plot"
      ))

      plot_density(
        df = table17_data(),
        group_var = input$Tab17Variable1,
        num_var = input$Tab17Variable2
      )
    })

    pinModuleServer(
      "plot17_pin",
      render_fn = function(output, session, out_id) {
        output[[out_id]] <- plotly::renderPlotly({
          shiny::req(
            input$Tab17Variable1,
            input$Tab17Variable2,
            input$Tab17Variable3
          )

          logger::log_info(glue::glue(
            "[Session {session_code}] Tab17: Rendering Plotly density"
          ))

          plotly::ggplotly(table17_plot(), tooltip = c("x", "y", "fill"))
        })
      },
      type = "density",
      params_r = shiny::reactive(list(
        GroupVar = input$Tab17Variable1,
        NumVar = input$Tab17Variable2,
        Year = input$Tab17Variable3
      )),
      title_r = shiny::reactive(glue::glue(
        "Density: {input$Tab17Variable2} by {input$Tab17Variable1} ({input$Tab17Variable3})"
      )),
      pins = pins
    )

    mod_shared_datatable_server(
      id = "dt_density",
      data_reactive = table17_data,
      style_type = "single",
      style_col = "n()",
      filter = "none",
      table_class = "compact hover stripe general-density-table",
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
      id = "export_density",
      data_r = table17_data,
      plot_r = table17_plot,
      filename_prefix_r = shiny::reactive(paste0(
        input$Tab17Variable1,
        "_density"
      ))
    )
  })
}
