# R/fn_global_filter.R

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }

  x
}

#' Empty schema-review object for the simplified explicit-mapping contract
#'
#' @return A list matching the contract shape expected by app-level observers.
#' @keywords internal
global_filter_empty_schema_review <- function() {
  list(
    required = FALSE,
    reasons = character(),
    suggested_date_column = "__none__",
    suggested_measure_columns = character(),
    date_candidates = character(),
    measure_candidates = character(),
    review_columns = character()
  )
}

#' Apply a single global filter to a dashboard dataset
#'
#' @param df Data frame.
#' @param group_var Column name to filter on.
#' @param group_levels Values to keep.
#'
#' @return Filtered tibble.
#' @export
data_apply_filter <- function(df, group_var = NULL, group_levels = NULL) {
  stopifnot(is.data.frame(df))

  if (
    is.null(group_var) ||
      is.null(group_levels) ||
      length(group_levels) == 0
  ) {
    return(tibble::as_tibble(df))
  }

  df |>
    tibble::as_tibble() |>
    dplyr::filter(.data[[group_var]] %in% group_levels)
}
