#' My Dashboard UI
#' @export
mod_my_dashboard_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    id = ns("dashboard_scope"),
    class = "general-dashboard-page",
    bs4Dash::bs4Card(
      title = "My Dashboard",
      status = "info",
      solidHeader = TRUE,
      collapsible = TRUE,
      closable = FALSE,
      maximizable = TRUE,
      width = 12,
      shiny::fluidRow(
        class = "general-dashboard-actions",
        shiny::column(
          width = 12,
          shiny::actionButton(
            ns("dashboard_preview"),
            "Preview Full Dashboard",
            class = "btn btn-light"
          )
        )
      ),
      shiny::div(
        class = "general-dashboard-table-shell",
        DT::DTOutput(ns("dashboard_pins_table"))
      )
    )
  )
}

#' My Dashboard Server
#' @export
mod_my_dashboard_server <- function(id, rv, pins) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session_code <- session$userData$sessionCode
    if (is.null(session_code)) {
      session_code <- "unknown"
    }

    output$dashboard_pins_table <- DT::renderDT({
      pin_list <- pins()
      df <- dashboard_pin_table_data(pin_list, ns)

      DT::datatable(
        df,
        escape = FALSE,
        rownames = FALSE,
        class = "compact hover stripe general-dashboard-table",
        options = list(
          dom = "t",
          ordering = FALSE,
          autoWidth = FALSE
        )
      )
    })

    shiny::observeEvent(input$remove_pin, {
      pin_list <- pins()
      index <- as.integer(input$remove_pin)

      if (length(index) != 1L || is.na(index)) {
        return(invisible(NULL))
      }

      if (index < 1L || index > length(pin_list)) {
        return(invisible(NULL))
      }

      pins(pin_list[-index])

      logger::log_info(glue::glue(
        "[Session {session_code}] Dashboard pin removed | index={index}"
      ))
    })

    shiny::observeEvent(input$dashboard_preview, {
      pin_list <- pins()

      if (length(pin_list) == 0L) {
        shiny::showModal(shiny::modalDialog(
          title = "Dashboard Preview",
          shiny::div(
            class = "general-dashboard-preview-modal",
            "No views pinned yet."
          ),
          easyClose = TRUE
        ))
        return()
      }

      shiny::showModal(shiny::modalDialog(
        title = "Dashboard Preview",
        size = "l",
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Dismiss"),
          shiny::downloadButton(ns("export_dashboard_pdf"), "Export PDF"),
          shiny::downloadButton(ns("export_dashboard_xlsx"), "Export Excel"),
          shiny::downloadButton(
            ns("export_dashboard_r_scripts"),
            "Export R scripts",
            class = "btn btn-light general-dashboard-r-scripts-download"
          )
        ),
        shiny::div(
          class = "general-dashboard-preview-modal",
          shiny::fluidRow(
            lapply(seq_along(pin_list), function(i) {
              pin <- pin_list[[i]]

              bs4Dash::bs4Card(
                title = pin$title,
                status = "info",
                solidHeader = TRUE,
                collapsible = FALSE,
                closable = FALSE,
                width = 12,
                if (!is.null(pin$description)) {
                  shiny::tags$p(pin$description)
                },
                dashboard_pin_preview_output(ns, pin, i)
              )
            })
          )
        )
      ))

      lapply(seq_along(pin_list), function(i) {
        pin <- pin_list[[i]]

        output[[paste0("preview_plot_", i)]] <- shiny::renderPlot({
          shiny::req(rv$data)

          if (pin$type %in% c("line_single", "line_grouped")) {
            shiny::req(rv$unfiltered)
          }

          dashboard_pin_plot(
            pin = pin,
            rv_data = rv$data,
            rv_unfiltered = rv$unfiltered
          )
        })
      })
    })

    output$export_dashboard_pdf <- shiny::downloadHandler(
      filename = function() {
        paste0("dashboard-", Sys.Date(), ".pdf")
      },
      content = function(file) {
        pin_list <- pins()

        shiny::req(length(pin_list) > 0L, rv$data)

        grDevices::pdf(file, width = 11, height = 8.5)
        on.exit(grDevices::dev.off(), add = TRUE)

        for (pin in pin_list) {
          if (pin$type %in% c("line_single", "line_grouped")) {
            shiny::req(rv$unfiltered)
          }

          p <- dashboard_pin_plot(
            pin = pin,
            rv_data = rv$data,
            rv_unfiltered = rv$unfiltered
          )

          p2 <- cowplot::ggdraw() +
            cowplot::draw_plot(
              p,
              x = 0.09,
              y = 0.12,
              width = 0.82,
              height = 0.76
            )

          print(p2)
        }
      }
    )

    output$export_dashboard_xlsx <- shiny::downloadHandler(
      filename = function() {
        paste0("dashboard-data-", format(Sys.Date(), "%d-%m-%Y"), ".xlsx")
      },
      content = function(file) {
        pin_list <- pins()

        shiny::req(length(pin_list) > 0L, rv$data)

        wb <- openxlsx::createWorkbook()

        for (i in seq_along(pin_list)) {
          pin <- pin_list[[i]]

          if (pin$type %in% c("line_single", "line_grouped")) {
            shiny::req(rv$unfiltered)
          }

          df <- dashboard_pin_data(
            pin = pin,
            rv_data = rv$data,
            rv_unfiltered = rv$unfiltered
          )

          sheet_name <- paste0(pin$type, "_", i)
          openxlsx::addWorksheet(wb, sheetName = sheet_name)
          openxlsx::writeData(wb, sheet = sheet_name, x = df)
        }

        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )

    output$export_dashboard_r_scripts <- shiny::downloadHandler(
      filename = function() {
        paste0("dashboard-r-scripts-", format(Sys.Date(), "%d-%m-%Y"), ".zip")
      },
      content = function(file) {
        pin_list <- pins()

        shiny::req(length(pin_list) > 0L, rv$data)
        if (any(vapply(pin_list, function(pin) {
          pin$type %in% c("line_single", "line_grouped")
        }, logical(1)))) {
          shiny::req(rv$unfiltered)
        }

        dashboard_r_scripts_write_zip(
          pin_list = pin_list,
          file = file,
          data = rv$data,
          data_unfiltered = rv$unfiltered
        )

        logger::log_info(glue::glue(
          "[Session {session_code}] Dashboard R scripts exported | pins={length(pin_list)}"
        ))
      },
      contentType = "application/zip"
    )
  })
}
