#' Export Actions UI
#' @param id Module ID
#' @param csv Logical, include CSV download button
#' @param plot Logical, include Plot download button
#' @param pin Logical, include Pin to Dashboard button
#' @param pin_id Character, the base namespace ID used for the pinModule (e.g. "plot10_pin").
#' @export
mod_export_actions_ui <- function(
  id,
  csv = TRUE,
  plot = FALSE,
  pin = FALSE,
  pin_id = NULL
) {
  ns <- shiny::NS(id)

  # Dynamically reconstruct the parent module's namespace to target the pin button correctly
  btn_id <- if (pin && !is.null(pin_id)) {
    parts <- strsplit(id, "-")[[1]]
    if (length(parts) > 1) {
      # Extract everything before the last hyphen (e.g., "mod_lines1" from "mod_lines1-export_10")
      parent_ns <- paste(parts[-length(parts)], collapse = "-")
      shiny::NS(shiny::NS(parent_ns, pin_id), "pin")
    } else {
      shiny::NS(pin_id, "pin")
    }
  } else {
    NULL
  }

  shiny::div(
    class = "d-flex flex-wrap align-items-center",
    style = "gap: 8px;",
    if (csv) {
      shiny::tags$a(
        id = ns("download_csv"),
        class = "btn btn-icon shiny-download-link",
        href = "",
        target = "_blank",
        download = NA,
        htmltools::HTML('<i class="fa-solid fa-file-csv"></i>'),
        title = "Download CSV"
      )
    },
    if (plot) {
      shiny::tags$a(
        id = ns("download_plot"),
        class = "btn btn-icon shiny-download-link",
        href = "",
        target = "_blank",
        download = NA,
        htmltools::HTML('<i class="fa-solid fa-chart-line"></i>'),
        title = "Download Plot"
      )
    },
    if (!is.null(btn_id)) {
      shiny::actionButton(
        inputId = btn_id,
        label = htmltools::HTML('<i class="fa-solid fa-gauge-high"></i>'),
        class = "btn-icon",
        title = "Add to Dashboard"
      )
    }
  )
}

#' Export Actions Server
#' @param id Module ID
#' @param data_r Reactive returning the data frame for CSV export
#' @param plot_r Reactive returning the ggplot object for PNG export
#' @param filename_prefix_r Reactive returning the string prefix for the filenames
#' @export
mod_export_actions_server <- function(
  id,
  data_r = NULL,
  plot_r = NULL,
  filename_prefix_r = NULL
) {
  shiny::moduleServer(id, function(input, output, session) {
    session_code <- session$userData$sessionCode
    if (is.null(session_code)) {
      session_code <- "unknown"
    }

    if (!is.null(data_r)) {
      output$download_csv <- shiny::downloadHandler(
        filename = function() {
          prefix <- if (!is.null(filename_prefix_r)) filename_prefix_r() else
            "data"
          paste0(prefix, ".csv")
        },
        content = function(file) {
          logger::log_info(glue::glue(
            "[Session {session_code}] Export Actions: Downloading CSV"
          ))
          utils::write.csv(data_r(), file)
        }
      )
    }

    if (!is.null(plot_r)) {
      output$download_plot <- shiny::downloadHandler(
        filename = function() {
          prefix <- if (!is.null(filename_prefix_r)) filename_prefix_r() else
            "plot"
          paste0(prefix, ".png")
        },
        content = function(file) {
          logger::log_info(glue::glue(
            "[Session {session_code}] Export Actions: Downloading Plot PNG"
          ))

          device <- function(..., width, height) {
            grDevices::png(
              ...,
              width = 9,
              height = height,
              res = 300,
              units = "in"
            )
          }

          ggplot2::ggsave(
            filename = file,
            plot = plot_r(),
            device = device,
            height = 5.5
          )
        }
      )
    }
  })
}
