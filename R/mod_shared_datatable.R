#' Shared Datatable UI
#' @param id Module ID
#' @export
mod_shared_datatable_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$style(shiny::HTML(
          "
          table.dataTable thead th.general-dt-header-cell {
            white-space: nowrap !important;
            overflow: hidden !important;
            text-overflow: ellipsis !important;
            vertical-align: bottom !important;
          }

          table.dataTable thead th.general-dt-header-cell .dt-column-title,
          table.dataTable thead th.general-dt-header-cell .DataTables_sort_wrapper,
          table.dataTable thead th.general-dt-header-cell .general-dt-header-label {
            display: inline-block !important;
            max-width: 4.25rem !important;
            overflow: hidden !important;
            text-overflow: ellipsis !important;
            white-space: nowrap !important;
            vertical-align: bottom !important;
          }

          table.dataTable.general-dt-dense-headers thead th {
            max-width: 4.75rem !important;
          }

          .general-dt-error-table td {
            padding: 1rem !important;
            font-weight: 600;
          }
        "
        ))
      )
    ),
    DT::dataTableOutput(ns("table"))
  )
}

#' Shared Datatable Server
#' @param id Module ID
#' @param data_reactive A reactive expression returning the data frame to display.
#' @param style_type Character string: "single" (default), "proportions", or "none".
#' @param style_col Character string. The name of the column to style if style_type is "single".
#' @param filter Character string passed to DT::datatable(); use "none", "top", or "bottom".
#' @param table_class Character string passed to DT::datatable(class = ...).
#' @param dt_options Named list passed to DT::datatable(options = ...).
#' @param bg_colors Character vector of three CSS colours used for low, medium, and high sample-size bands.
#' @export
mod_shared_datatable_server <- function(
  id,
  data_reactive,
  style_type = "single",
  style_col = "n()",
  filter = "bottom",
  table_class = "hover row-border stripe",
  dt_options = list(scrollY = "200px"),
  bg_colors = c(
    "rgba(235, 133, 150, 0.4)",
    "rgba(230, 185, 80, 0.4)",
    "rgba(92, 107, 60, 0.4)"
  )
) {
  shiny::moduleServer(id, function(input, output, session) {
    session_code <- session$userData$sessionCode
    if (is.null(session_code)) {
      session_code <- "unknown"
    }

    output$table <- DT::renderDataTable({
      df <- tryCatch(
        data_reactive(),
        error = function(e) {
          logger::log_error(glue::glue(
            "[Session {session_code}] Shared DT: table failed | {conditionMessage(e)}"
          ))

          structure(
            list(message = conditionMessage(e)),
            class = "general_dt_error"
          )
        }
      )

      if (inherits(df, "general_dt_error")) {
        return(general_dt_error_table(
          "This table could not be built with the current selections. Try choosing different variables."
        ))
      }

      shiny::req(df)

      logger::log_info(glue::glue(
        "[Session {session_code}] Shared DT: Rendering data table"
      ))

      display_colnames <- general_dt_display_colnames(
        column_names = names(df),
        style_type = style_type
      )

      header_titles <- general_dt_header_titles(names(df))
      header_titles_json <- jsonlite::toJSON(
        header_titles,
        auto_unbox = TRUE
      )

      dense_class <- if (ncol(df) > 8L) {
        " general-dt-dense-headers"
      } else {
        ""
      }

      header_callback <- DT::JS(sprintf(
        paste0(
          "var fullNames = %s;",
          "var tableNode = $(table.table().node());",
          "var tableId = tableNode.attr('id') || Math.random().toString(36).slice(2);",
          "var eventNamespace = '.generalDtAdjust' + tableId.replace(/[^A-Za-z0-9_]/g, '_');",
          "var container = $(table.table().container());",
          "var card = container.closest('.card, .box');",
          "function applyHeaderEllipsis() {",
          "  table.columns().every(function(index) {",
          "    var header = $(this.header());",
          "    var title = fullNames[index] || header.text().trim();",
          "    header.attr('title', title);",
          "    header.addClass('general-dt-header-cell');",
          "    header.find('.dt-column-title, .DataTables_sort_wrapper')",
          "      .addClass('general-dt-header-label')",
          "      .attr('title', title);",
          "  });",
          "}",
          "function adjustTableLayout() {",
          "  applyHeaderEllipsis();",
          "  table.columns.adjust();",
          "}",
          "function scheduleAdjustments() {",
          "  [0, 50, 150, 300, 600, 900].forEach(function(delay) {",
          "    window.setTimeout(adjustTableLayout, delay);",
          "  });",
          "}",
          "applyHeaderEllipsis();",
          "scheduleAdjustments();",
          "table.off('draw.dt' + eventNamespace);",
          "table.on('draw.dt' + eventNamespace, applyHeaderEllipsis);",
          "$(window).off('resize' + eventNamespace);",
          "$(window).on('resize' + eventNamespace, scheduleAdjustments);",
          "if (card.length) {",
          "  card.off(eventNamespace);",
          "  card.on(",
          "    'maximized.lte.cardwidget' + eventNamespace + ' ' +",
          "    'minimized.lte.cardwidget' + eventNamespace + ' ' +",
          "    'expanded.lte.cardwidget' + eventNamespace + ' ' +",
          "    'collapsed.lte.cardwidget' + eventNamespace + ' ' +",
          "    'transitionend' + eventNamespace + ' ' +",
          "    'webkitTransitionEnd' + eventNamespace,",
          "    scheduleAdjustments",
          "  );",
          "  card.find('[data-card-widget=\"maximize\"], [data-card-widget=\"fullscreen\"]')",
          "    .off('click' + eventNamespace)",
          "    .on('click' + eventNamespace, scheduleAdjustments);",
          "}"
        ),
        header_titles_json
      ))

      dt <- DT::datatable(
        df,
        colnames = display_colnames,
        options = dt_options,
        class = paste0(table_class, dense_class),
        filter = filter,
        callback = header_callback
      )

      if (style_type == "single") {
        if (style_col %in% colnames(df)) {
          dt <- dt |>
            DT::formatStyle(
              style_col,
              backgroundColor = DT::styleInterval(c(100, 300), bg_colors)
            )
        }
      } else if (style_type == "proportions") {
        c_names <- colnames(df)
        target_columns <- c_names[
          !c_names %in% "Year" & !grepl("Percent", c_names)
        ]

        for (col in target_columns) {
          dt <- dt |>
            DT::formatStyle(
              col,
              backgroundColor = DT::styleInterval(c(100, 300), bg_colors)
            )
        }
      }

      dt
    })
  })
}
