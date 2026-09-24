#' Dashboard pin parameter label
#' @keywords internal
dashboard_pin_params_label <- function(params) {
  if (is.null(params) || length(params) == 0L) {
    return("")
  }

  values <- vapply(
    params,
    \(value) paste(as.character(value), collapse = ", "),
    character(1)
  )

  paste(names(values), values, sep = ": ", collapse = ", ")
}

#' Dashboard remove button HTML
#' @keywords internal
dashboard_remove_pin_button <- function(index, ns) {
  input_id <- ns("remove_pin")

  as.character(shiny::tags$button(
    type = "button",
    class = "btn btn-danger btn-xs general-dashboard-remove-pin",
    title = "Remove",
    onclick = sprintf(
      "Shiny.setInputValue('%s', %d, {priority: 'event'});",
      input_id,
      index
    ),
    shiny::icon("trash")
  ))
}


#' Dashboard pins table data
#' @keywords internal
dashboard_pin_table_data <- function(pin_list, ns = identity) {
  if (length(pin_list) == 0L) {
    return(tibble::tibble(Message = "No views pinned yet."))
  }

  tibble::tibble(
    Type = purrr::map_chr(
      pin_list,
      \(pin) as.character(pin$type)
    ),
    Date = purrr::map_chr(
      pin_list,
      \(pin) {
        if (!is.null(pin$caption)) {
          as.character(pin$caption)
        } else {
          ""
        }
      }
    ),
    Remove = purrr::map_chr(
      seq_along(pin_list),
      \(i) dashboard_remove_pin_button(i, ns)
    )
  )
}

#' Resolve dashboard area percentage time variable
#' @keywords internal
dashboard_pin_area_percent_time_var <- function(time_variable) {
  if (identical(time_variable, "Year")) {
    "Year"
  } else {
    "Year_Quarter"
  }
}

#' Build dashboard pin data
#' @keywords internal
dashboard_pin_data <- function(pin, rv_data, rv_unfiltered = NULL) {
  params <- pin$params

  switch(
    pin$type,
    "line_single" = data_line_single_variable(
      rv_data,
      rv_unfiltered,
      params$TimeVariable,
      params$NumericVariable
    ),
    "line_grouped" = data_grouped_lines(
      rv_data,
      rv_unfiltered,
      params$TimeVariable2,
      params$GroupVar,
      params$NumVar
    ),
    "bar_simple" = data_simple_bar(
      rv_data,
      params$TimeVariable3,
      params$GroupVar,
      params$NumVar
    ),
    "bar_stacked" = data_stacked_bar(
      rv_data,
      params$TimeVariable4,
      params$GroupVar1,
      params$GroupVar2,
      params$NumVar
    ),
    "bar_grouped" = data_grouped_dodge_bar(
      rv_data,
      params$TimeVariable5,
      params$GroupVar1,
      params$GroupVar2,
      params$NumVar
    ),
    "area_stacked" = data_stacked_area(
      data = rv_data,
      time_var = params$TimeVariable6,
      group_var = params$GroupingVar,
      num_var = params$NumericVar
    ),
    "area_percent" = data_proportional_area(
      data = rv_data,
      time_var = dashboard_pin_area_percent_time_var(params$TimeVariable7),
      group_var = params$GroupingVar
    ),
    "scatter" = data_scatter_grouped(
      rv_data,
      params$GroupVar,
      params$X,
      params$Y
    ),
    "density" = data_density(
      data = rv_data,
      group_var = params$GroupVar,
      num_var = params$NumVar,
      fy_choice = as.character(params$Year)
    ),
    stop("Unsupported pin type: ", pin$type)
  )
}

#' Build dashboard pin plot
#' @keywords internal
dashboard_pin_plot <- function(pin, rv_data, rv_unfiltered = NULL) {
  params <- pin$params
  data <- dashboard_pin_data(pin, rv_data, rv_unfiltered)

  switch(
    pin$type,
    "line_single" = plot_line_single_variable(
      data,
      params$TimeVariable,
      params$NumericVariable,
      params$TimeVariable,
      y_min = params$YAxisMin %||% NA_real_,
      y_max = params$YAxisMax %||% NA_real_
    ),
    "line_grouped" = plot_grouped_lines(
      data,
      params$TimeVariable2,
      params$GroupVar,
      params$NumVar,
      params$TimeVariable2,
      y_min = params$YAxisMin %||% NA_real_,
      y_max = params$YAxisMax %||% NA_real_
    ),
    "bar_simple" = plot_simple_bar(
      data,
      params$GroupVar,
      params$NumVar,
      y_min = params$YAxisMin %||% NA_real_,
      y_max = params$YAxisMax %||% NA_real_
    ),
    "bar_stacked" = plot_stacked_bar(
      data,
      params$GroupVar1,
      params$GroupVar2,
      params$NumVar,
      y_min = params$YAxisMin %||% NA_real_,
      y_max = params$YAxisMax %||% NA_real_
    ),
    "bar_grouped" = plot_grouped_dodge_bar(
      data,
      params$GroupVar1,
      params$GroupVar2,
      params$NumVar,
      y_min = params$YAxisMin %||% NA_real_,
      y_max = params$YAxisMax %||% NA_real_
    ),
    "area_stacked" = plot_stacked_area(
      df = data,
      time_var = params$TimeVariable6,
      group_var = params$GroupingVar,
      num_var = params$NumericVar,
      y_min = params$YAxisMin %||% NA_real_,
      y_max = params$YAxisMax %||% NA_real_
    ),
    "area_percent" = plot_proportional_area(
      data,
      y_min = params$YAxisMin %||% NA_real_,
      y_max = params$YAxisMax %||% NA_real_
    ),
    "scatter" = plot_scatter_grouped(
      data,
      params$GroupVar,
      params$X,
      params$Y
    ),
    "density" = plot_density(
      df = data,
      group_var = params$GroupVar,
      num_var = params$NumVar
    ),
    stop("Unsupported pin type: ", pin$type)
  )
}

#' Dashboard pin preview output
#' @keywords internal
dashboard_pin_preview_output <- function(ns, pin, index) {
  supported_types <- c(
    "line_single",
    "line_grouped",
    "bar_simple",
    "bar_stacked",
    "bar_grouped",
    "area_stacked",
    "area_percent",
    "scatter",
    "density"
  )

  if (!pin$type %in% supported_types) {
    return(shiny::tags$p(
      class = "general-dashboard-unsupported-pin",
      paste0("Unsupported pin type: ", pin$type)
    ))
  }

  shiny::plotOutput(ns(paste0("preview_plot_", index)))
}
