#' Validate a standardized dashboard dataset
#'
#' This file owns structural validation only. It receives the app-ready dataset
#' produced by explicit user mapping and checks which selected columns are usable.
#' It does not parse raw uploads and it does not decide UI behavior.
#'
#' @param standardized_data A standardized tibble produced by user mapping.
#' @param transformation_report Transformation report from user mapping.
#'
#' @return A list with `validated_columns`, `validation_checks`,
#'   `variable_sets`, and `validation_status`.
data_validate_dataset <- function(
  standardized_data,
  transformation_report
) {
  standardized_data <- tibble::as_tibble(standardized_data)

  validated_columns <- data_validate_columns(
    standardized_data = standardized_data,
    transformation_report = transformation_report
  )

  validation_checks <- data_validate_checks(
    standardized_data = standardized_data,
    validated_columns = validated_columns
  )

  variable_sets <- data_validate_variable_sets(validated_columns)

  validation_status <- data_validate_status(
    standardized_data = standardized_data,
    validated_columns = validated_columns,
    validation_checks = validation_checks
  )

  list(
    validated_columns = validated_columns,
    validation_checks = validation_checks,
    variable_sets = variable_sets,
    validation_status = validation_status
  )
}

#' Build column-level validation facts
#'
#' @param standardized_data A normalized tibble.
#' @param transformation_report Transformation report from normalization.
#'
#' @return A tibble with one record per retained/derived column.
data_validate_columns <- function(
  standardized_data,
  transformation_report
) {
  empty_validated_columns <- tibble::tibble(
    column = character(),
    role = character(),
    output_type = character(),
    derived = logical(),
    non_missing_count = integer(),
    distinct_count = integer(),
    distinct_ratio = numeric(),
    missingness = numeric(),
    usable_as_date = logical(),
    safe_for_summary = logical(),
    usable_for_grouping = logical()
  )

  base_columns <- transformation_report$columns_changed |>
    dplyr::filter(!is.na(output_type)) |>
    dplyr::transmute(
      column = cleaned_name,
      role = resolved_role,
      output_type = output_type,
      derived = FALSE
    )

  helper_names <- transformation_report$helper_fields_created %||% character()

  helper_columns <- tibble::tibble(
    column = helper_names,
    role = "dimension",
    output_type = purrr::map_chr(
      helper_names,
      \(name) class(standardized_data[[name]])[[1]]
    ),
    derived = TRUE
  )

  column_index <- dplyr::bind_rows(base_columns, helper_columns) |>
    dplyr::filter(column %in% names(standardized_data))

  if (nrow(column_index) == 0L) {
    return(empty_validated_columns)
  }

  purrr::map_dfr(
    seq_len(nrow(column_index)),
    \(i) {
      data_validate_column(
        x = standardized_data[[column_index$column[[i]]]],
        column = column_index$column[[i]],
        role = column_index$role[[i]],
        output_type = column_index$output_type[[i]],
        derived = column_index$derived[[i]]
      )
    }
  )
}

#' Validate a single standardized column
#'
#' @param x Vector to validate.
#' @param column Column name.
#' @param role Semantic role.
#' @param output_type Output type recorded during normalization.
#' @param derived Whether the column was derived during normalization.
#'
#' @return One-row tibble of validation facts.
data_validate_column <- function(
  x,
  column,
  role,
  output_type,
  derived = FALSE
) {
  n_total <- length(x)
  non_missing_count <- sum(!is.na(x))
  distinct_count <- dplyr::n_distinct(x, na.rm = TRUE)
  distinct_ratio <- dplyr::if_else(
    non_missing_count == 0L,
    0,
    distinct_count / non_missing_count
  )
  missingness <- dplyr::if_else(
    n_total == 0L,
    1,
    mean(is.na(x))
  )

  usable_as_date <- role == "date" &&
    inherits(x, "Date") &&
    non_missing_count > 0L

  safe_for_summary <- data_validate_safe_for_summary(
    x = x,
    role = role
  )

  usable_for_grouping <- data_validate_usable_for_grouping(
    x = x,
    role = role,
    derived = derived,
    distinct_ratio = distinct_ratio
  )

  tibble::tibble(
    column = column,
    role = role,
    output_type = output_type,
    derived = derived,
    non_missing_count = non_missing_count,
    distinct_count = distinct_count,
    distinct_ratio = distinct_ratio,
    missingness = missingness,
    usable_as_date = usable_as_date,
    safe_for_summary = safe_for_summary,
    usable_for_grouping = usable_for_grouping
  )
}

#' Determine whether a column is safe for numeric summaries
#'
#' @param x Standardized vector.
#' @param role Semantic role.
#'
#' @return Logical scalar.
data_validate_safe_for_summary <- function(x, role) {
  if (role != "measure") {
    return(FALSE)
  }

  if (!is.numeric(x)) {
    return(FALSE)
  }

  finite_values <- x[is.finite(x)]

  length(finite_values) > 0L
}

#' Determine whether a column is usable for grouping
#'
#' Grouping should exclude identifiers and high-cardinality dimensions that
#' behave like row-level labels.
#'
#' @param x Standardized vector.
#' @param role Semantic role.
#' @param derived Whether the column was derived.
#' @param distinct_ratio Distinct/non-missing ratio.
#'
#' @return Logical scalar.
data_validate_usable_for_grouping <- function(
  x,
  role,
  derived,
  distinct_ratio
) {
  if (!(role %in% c("dimension", "date")) && !derived) {
    return(FALSE)
  }

  non_missing_count <- sum(!is.na(x))

  if (non_missing_count == 0L) {
    return(FALSE)
  }

  if (distinct_ratio >= 0.95) {
    return(FALSE)
  }

  TRUE
}

#' Run dataset-level validation checks
#'
#' @param standardized_data A normalized tibble.
#' @param validated_columns Column-level validation facts.
#'
#' @return A tibble of named validation checks.
data_validate_checks <- function(
  standardized_data,
  validated_columns
) {
  has_rows <- nrow(standardized_data) > 0L
  has_columns <- ncol(standardized_data) > 0L
  has_validated_columns <- nrow(validated_columns) > 0L

  has_valid_dimension <- validated_columns |>
    dplyr::filter(role == "dimension" | derived) |>
    nrow() >
    0L

  has_valid_measure <- validated_columns |>
    dplyr::filter(role == "measure", safe_for_summary) |>
    nrow() >
    0L

  has_valid_date <- validated_columns |>
    dplyr::filter(usable_as_date) |>
    nrow() >
    0L

  has_grouping_columns <- validated_columns |>
    dplyr::filter(usable_for_grouping) |>
    nrow() >
    0L

  has_summary_measures <- validated_columns |>
    dplyr::filter(safe_for_summary) |>
    nrow() >
    0L

  tibble::tribble(
    ~check                  , ~passed               , ~detail                                              ,
    "has_rows"              , has_rows              , "Standardized dataset has at least one row"          ,
    "has_columns"           , has_columns           , "Standardized dataset has at least one column"       ,
    "has_validated_columns" , has_validated_columns , "At least one column survived normalization"         ,
    "has_valid_dimension"   , has_valid_dimension   , "At least one dimension-like column is available"    ,
    "has_valid_measure"     , has_valid_measure     , "At least one numeric measure is safe for summaries" ,
    "has_valid_date"        , has_valid_date        , "At least one parsed date field is available"        ,
    "has_grouping_columns"  , has_grouping_columns  , "At least one grouping column is usable"             ,
    "has_summary_measures"  , has_summary_measures  , "At least one measure can be summarized safely"
  )
}

#' Build validated variable inventories for downstream use
#'
#' @param validated_columns Column-level validation facts.
#'
#' @return A named list of validated variable vectors.
data_validate_variable_sets <- function(validated_columns) {
  list(
    date_vars = validated_columns |>
      dplyr::filter(usable_as_date) |>
      dplyr::pull(column),

    measure_vars = validated_columns |>
      dplyr::filter(role == "measure", safe_for_summary) |>
      dplyr::pull(column),

    dimension_vars = validated_columns |>
      dplyr::filter(role == "dimension" | derived) |>
      dplyr::pull(column),

    identifier_vars = validated_columns |>
      dplyr::filter(role == "identifier") |>
      dplyr::pull(column),

    grouping_vars = validated_columns |>
      dplyr::filter(usable_for_grouping) |>
      dplyr::pull(column),

    free_text_vars = validated_columns |>
      dplyr::filter(role == "free text") |>
      dplyr::pull(column)
  )
}

#' Collapse validation checks into a dataset-level status
#'
#' A dataset can be valid even if it only supports part of the app. Fatal
#' status is reserved for structurally unusable standardized results.
#'
#' @param standardized_data A normalized tibble.
#' @param validated_columns Column-level validation facts.
#' @param validation_checks Dataset-level validation checks.
#'
#' @return One of `fatal` or `valid`.
data_validate_status <- function(
  standardized_data,
  validated_columns,
  validation_checks
) {
  fatal_checks <- validation_checks |>
    dplyr::filter(
      check %in% c("has_rows", "has_columns", "has_validated_columns")
    )

  if (
    nrow(standardized_data) == 0L ||
      ncol(standardized_data) == 0L ||
      nrow(validated_columns) == 0L ||
      any(!fatal_checks$passed)
  ) {
    return("fatal")
  }

  "valid"
}
