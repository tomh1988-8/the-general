# R/data_user_mapping.R

#' Ingest an uploaded dashboard sheet through explicit user mapping
#'
#' This is the canonical upload boundary for the app.
#'
#' The user still chooses the date, numeric, and categorical columns. The app
#' then applies the proven upload cleaning rules before validation.
#'
#' @param data Raw uploaded data frame.
#' @param source_name Human-readable dataset name.
#' @param source_type Source type, normally `"upload"`.
#' @param date_column One selected date column.
#' @param numeric_columns One or more selected numeric columns.
#' @param categorical_columns One or more selected categorical columns.
#' @param financial_year_start_month Month used for financial helper fields.
#'
#' @return A standardized ingestion contract consumed by the app.
#' @export
data_ingest_uploaded_sheet <- function(
  data,
  source_name,
  source_type = "upload",
  date_column,
  numeric_columns,
  categorical_columns,
  financial_year_start_month = 4L,
  ...
) {
  logger::log_info(glue::glue(
    "User-mapped ingestion started | source_name={source_name} | source_type={source_type}"
  ))

  data <- tibble::as_tibble(data)

  mapping <- data_user_mapping_resolve_columns(
    data = data,
    date_column = date_column,
    numeric_columns = numeric_columns,
    categorical_columns = categorical_columns
  )

  standardized_result <- data_user_mapping_standardize(
    data = data,
    mapping = mapping,
    financial_year_start_month = financial_year_start_month
  )

  standardized_data <- standardized_result$data

  transformation_report <- data_user_mapping_transformation_report(
    mapping = mapping,
    standardized_data = standardized_data,
    rows_dropped = standardized_result$rows_dropped,
    parse_failures = standardized_result$parse_failures
  )

  validation_result <- data_validate_dataset(
    standardized_data = standardized_data,
    transformation_report = transformation_report
  )

  capabilities_result <- data_capabilities_dataset(validation_result)

  status <- dplyr::if_else(
    validation_result$validation_status == "fatal",
    "fatal",
    "ready"
  )

  contract <- list(
    raw_metadata = tibble::tibble(
      source_name = source_name,
      source_type = source_type,
      row_count = nrow(data),
      column_count = ncol(data),
      warnings = list(character())
    ),
    column_profile = tibble::tibble(),
    standardized_data = standardized_data,
    transformation_report = transformation_report,
    validation = validation_result,
    capabilities = capabilities_result,
    schema_review = global_filter_empty_schema_review(),
    status = status
  )

  logger::log_info(glue::glue(
    "User-mapped ingestion complete | status={status} | rows={nrow(standardized_data)} | cols={ncol(standardized_data)}"
  ))

  logger::log_debug(glue::glue(
    "User-mapped output columns | {paste(names(standardized_data), collapse = ', ')}"
  ))

  contract
}

#' Resolve user-selected columns against raw data names
data_user_mapping_resolve_columns <- function(
  data,
  date_column,
  numeric_columns,
  categorical_columns
) {
  original_names <- names(data)
  cleaned_names <- data_user_mapping_clean_names(original_names)

  column_index <- tibble::tibble(
    original_name = original_names,
    cleaned_name = unname(cleaned_names)
  )

  date_selected <- data_user_mapping_match_selected(
    selected = date_column,
    column_index = column_index
  )

  numeric_selected <- data_user_mapping_match_selected(
    selected = numeric_columns,
    column_index = column_index
  )

  categorical_selected <- data_user_mapping_match_selected(
    selected = categorical_columns,
    column_index = column_index
  )

  stopifnot(nrow(date_selected) == 1L)
  stopifnot(nrow(numeric_selected) >= 1L)
  stopifnot(nrow(categorical_selected) >= 1L)

  selected <- dplyr::bind_rows(
    date_selected |>
      dplyr::mutate(
        role = "date",
        output_name = paste0("Date_", cleaned_name)
      ),
    numeric_selected |>
      dplyr::mutate(
        role = "measure",
        output_name = paste0("Num_", cleaned_name)
      ),
    categorical_selected |>
      dplyr::mutate(
        role = "dimension",
        output_name = paste0("Cat_", cleaned_name)
      )
  ) |>
    dplyr::distinct(original_name, role, .keep_all = TRUE)

  list(selected = selected)
}

# Suggest roles from values the existing cleaners can read; never alter the input.
data_user_mapping_suggestions <- function(data) {
  profiles <- purrr::map(data, function(x) {
    values <- data_user_mapping_normalize_missing(x)
    values <- values[!is.na(values)]
    total <- length(values)
    if (total == 0L) {
      return(tibble::tibble(
        parser = "empty", suggested_role = "review", usable_values = 0L,
        numeric_values = 0L, date_values = 0L, reason = "No usable values",
        examples = ""
      ))
    }

    # Do not mistake category labels such as 1st, top-10 or 6-12 for numbers.
    number_text <- stringr::str_remove_all(values, "[\\p{Sc}\\s%]")
    number_shape <- stringr::str_detect(
      number_text, "^[+-]?([0-9]+([.,][0-9]+)?|[0-9]{1,3}([.,][0-9]{3})+([.,][0-9]+)?|[.,][0-9]+)([eE][+-]?[0-9]+)?$"
    )
    number_ok <- number_shape & is.finite(data_user_mapping_parse_number(values))

    # A year plus separators/month text avoids interpreting plain numbers as dates.
    date_shape <- stringr::str_detect(values, "[0-9]{4}") &
      stringr::str_detect(values, "[-/.[:alpha:]]")
    date_ok <- rep(FALSE, total)
    if (any(date_shape)) {
      date_ok[date_shape] <- !is.na(data_user_mapping_parse_date(values[date_shape]))
    }

    padded_codes <- any(stringr::str_detect(values, "^0[0-9]+$"))
    boolean <- data_user_mapping_is_boolean(values)
    role <- if (mean(date_ok) >= 0.9) {
      "date"
    } else if (!padded_codes && !boolean && mean(number_ok) >= 0.9) {
      "numeric"
    } else {
      "categorical"
    }
    reason <- switch(role,
      date = paste(sum(date_ok), "of", total, "values read as dates after cleaning"),
      numeric = paste(sum(number_ok), "of", total, "values read as numbers after cleaning"),
      categorical = if (padded_codes) "Leading-zero codes" else if (boolean) "Yes/no values" else "Text or mixed values"
    )
    tibble::tibble(
      parser = readr::guess_parser(values), suggested_role = role,
      usable_values = total, numeric_values = sum(number_ok), date_values = sum(date_ok),
      reason = reason, examples = paste(head(unique(values), 3L), collapse = " | ")
    )
  })

  dplyr::bind_cols(tibble::tibble(column = names(data)), dplyr::bind_rows(profiles))
}

# A draft places suggestions on the board; ingestion still needs confirmation.
data_user_mapping_board <- function(suggestions) {
  tibble::tibble(
    column = suggestions$column,
    role = ifelse(suggestions$suggested_role == "review", "categorical", suggestions$suggested_role),
    included = suggestions$suggested_role != "review",
    also_categorical = FALSE
  )
}

# Preserve the existing ability to use a numeric column for grouping too.
data_user_mapping_board_selection <- function(board) {
  list(
    date = board$column[board$included & board$role == "date"],
    numeric = board$column[board$included & board$role == "numeric"],
    categorical = board$column[board$included & (
      board$role == "categorical" | (board$role == "numeric" & board$also_categorical)
    )]
  )
}

#' Clean raw column names into simple snake_case
data_user_mapping_clean_names <- function(names) {
  cleaned <- janitor::make_clean_names(names)
  cleaned <- sub("_+$", "", cleaned)
  cleaned[cleaned == ""] <- "x"
  cleaned <- vctrs::vec_as_names(cleaned, quiet = TRUE)

  stats::setNames(cleaned, names)
}

#' Match user selections against original and cleaned names
data_user_mapping_match_selected <- function(selected, column_index) {
  selected <- selected[!is.na(selected)]
  selected <- selected[nzchar(selected)]

  selected_cleaned <- unname(data_user_mapping_clean_names(selected))

  column_index |>
    dplyr::filter(
      original_name %in% selected | cleaned_name %in% selected_cleaned
    )
}

#' Standardize selected columns into dashboard-ready names and types
data_user_mapping_standardize <- function(
  data,
  mapping,
  financial_year_start_month = 4L
) {
  selected <- mapping$selected

  parsed <- purrr::map(
    seq_len(nrow(selected)),
    \(i) {
      original_name <- selected$original_name[[i]]
      output_name <- selected$output_name[[i]]
      role <- selected$role[[i]]
      x <- data[[original_name]]

      if (role == "date") {
        value <- data_user_mapping_parse_date(x)
      } else if (role == "measure") {
        value <- data_user_mapping_parse_number(x)
      } else {
        value <- data_user_mapping_parse_category(x)
      }

      normalized <- data_user_mapping_normalize_missing(x)
      failed_count <- sum(!is.na(normalized) & is.na(value))

      list(
        output_name = output_name,
        role = role,
        value = value,
        failed_count = failed_count
      )
    }
  )

  out <- parsed |>
    purrr::map("value") |>
    rlang::set_names(purrr::map_chr(parsed, "output_name")) |>
    tibble::as_tibble()

  rows_before <- nrow(out)
  out <- out[rowSums(!is.na(out)) > 0, , drop = FALSE]
  rows_dropped <- rows_before - nrow(out)

  date_output_name <- selected |>
    dplyr::filter(role == "date") |>
    dplyr::pull(output_name)

  helper_result <- data_user_mapping_add_date_helpers(
    data = out,
    date_column = date_output_name[[1]],
    financial_year_start_month = financial_year_start_month
  )

  parse_failures <- tibble::tibble(
    original_name = selected$original_name,
    cleaned_name = selected$output_name,
    resolved_role = selected$role,
    failed_count = purrr::map_int(parsed, "failed_count")
  ) |>
    dplyr::filter(failed_count > 0L)

  list(
    data = helper_result$data,
    rows_dropped = rows_dropped,
    parse_failures = parse_failures
  )
}

#' Normalize missing-value tokens before parsing
data_user_mapping_normalize_missing <- function(x) {
  x_chr <- x |>
    as.character() |>
    stringr::str_trim()

  missing_tokens <- c(
    "",
    "na",
    "n/a",
    "nan",
    "null",
    "none",
    "missing",
    "unknown",
    "-999"
  )

  x_chr[tolower(x_chr) %in% missing_tokens] <- NA_character_
  x_chr
}

#' Parse the selected date column
data_user_mapping_parse_date <- function(x) {
  x_chr <- data_user_mapping_normalize_missing(x)

  parsed <- suppressWarnings(
    lubridate::parse_date_time(
      x_chr,
      orders = c(
        "ymd",
        "ymd HMS",
        "ymd HM",
        "dmy",
        "dmy HMS",
        "dmy HM",
        "mdy",
        "mdy HMS",
        "mdy HM",
        "Ymd",
        "Ymd HMS",
        "Ymd HM",
        "b d Y",
        "d b Y",
        "B d Y",
        "d B Y",
        "BdY HMS"
      ),
      quiet = TRUE
    )
  )

  as.Date(parsed)
}

#' Parse selected numeric columns
data_user_mapping_parse_number <- function(x) {
  x_chr <- data_user_mapping_normalize_missing(x)
  percent_mask <- stringr::str_detect(x_chr, "%")

  uk <- suppressWarnings(
    readr::parse_number(
      x_chr,
      locale = readr::locale(grouping_mark = ",", decimal_mark = ".")
    )
  )

  eu <- suppressWarnings(
    readr::parse_number(
      x_chr,
      locale = readr::locale(grouping_mark = ".", decimal_mark = ",")
    )
  )

  parsed <- purrr::map2_dbl(uk, eu, \(u, e) {
    if (is.na(u)) {
      return(e)
    }

    if (is.na(e)) {
      return(u)
    }

    if (abs(u) >= 1000 && abs(e) < 10) {
      return(u)
    }

    if (abs(e) >= 1000 && abs(u) < 10) {
      return(e)
    }

    if ((u %% 1) != 0 && (e %% 1) == 0) {
      return(u)
    }

    if ((e %% 1) != 0 && (u %% 1) == 0) {
      return(e)
    }

    if (abs(u) >= abs(e)) {
      u
    } else {
      e
    }
  })

  parsed[percent_mask & !is.na(parsed)] <- parsed[
    percent_mask & !is.na(parsed)
  ] /
    100

  parsed
}

#' Parse selected categorical columns
data_user_mapping_parse_category <- function(x) {
  x_chr <- data_user_mapping_normalize_missing(x)

  if (data_user_mapping_is_boolean(x_chr)) {
    return(
      dplyr::case_when(
        tolower(x_chr) %in% c("yes", "true", "1") ~ "Yes",
        tolower(x_chr) %in% c("no", "false", "0") ~ "No",
        TRUE ~ NA_character_
      ) |>
        factor(levels = c("No", "Yes"))
    )
  }

  out <- ifelse(
    is.na(x_chr),
    NA_character_,
    tools::toTitleCase(tolower(trimws(x_chr)))
  )

  factor(out)
}

#' Check whether a selected categorical column is boolean-like
data_user_mapping_is_boolean <- function(x) {
  values <- tolower(stats::na.omit(x))
  length(values) > 0L &&
    all(unique(values) %in% c("yes", "no", "true", "false", "0", "1"))
}

#' Add legacy-compatible date helper fields
data_user_mapping_add_date_helpers <- function(
  data,
  date_column,
  financial_year_start_month = 4L
) {
  reference_date <- data[[date_column]]

  dow_levels <- c(
    "Sunday",
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday"
  )

  month_levels <- month.abb[c(
    financial_year_start_month:12,
    1:(financial_year_start_month - 1L)
  )]

  fq_labels <- c(
    "Q1 (Apr-Jun)",
    "Q2 (Jul-Sep)",
    "Q3 (Oct-Dec)",
    "Q4 (Jan-Mar)"
  )

  fiscal_month_index <- ((lubridate::month(reference_date) -
    financial_year_start_month) %%
    12L) +
    1L

  fiscal_quarter_index <- ((fiscal_month_index - 1L) %/% 3L) + 1L

  year <- lubridate::year(reference_date)

  financial_year <- dplyr::if_else(
    lubridate::month(reference_date) >= financial_year_start_month,
    paste0(year, "-", substr(year + 1L, 3, 4)),
    paste0(year - 1L, "-", substr(year, 3, 4))
  )

  helper_fields <- tibble::tibble(
    DOTW = factor(
      weekdays(reference_date),
      levels = dow_levels,
      ordered = TRUE
    ),
    Month = factor(
      month.abb[lubridate::month(reference_date)],
      levels = month_levels,
      ordered = TRUE
    ),
    Financial_Quarter = factor(
      fq_labels[fiscal_quarter_index],
      levels = fq_labels,
      ordered = TRUE
    ),
    Year = year,
    Financial_Year = factor(financial_year),
    Year_Quarter = zoo::as.yearqtr(reference_date)
  )

  list(
    data = dplyr::bind_cols(data, helper_fields),
    helper_fields_created = names(helper_fields)
  )
}

#' Build a transformation report for selected-column ingestion
data_user_mapping_transformation_report <- function(
  mapping,
  standardized_data,
  rows_dropped = 0L,
  parse_failures = tibble::tibble()
) {
  selected <- mapping$selected

  base_columns <- selected |>
    dplyr::transmute(
      original_name = original_name,
      cleaned_name = output_name,
      resolved_role = role,
      input_type = "user_selected",
      output_type = purrr::map_chr(
        output_name,
        \(name) class(standardized_data[[name]])[[1]]
      ),
      name_changed = original_name != output_name
    )

  list(
    columns_changed = base_columns,
    rows_dropped = rows_dropped,
    parse_failures = parse_failures,
    helper_fields_created = c(
      "DOTW",
      "Month",
      "Financial_Quarter",
      "Year",
      "Financial_Year",
      "Year_Quarter"
    ),
    columns_excluded = tibble::tibble(
      column = character(),
      reason = character()
    ),
    reference_date_column = selected |>
      dplyr::filter(role == "date") |>
      dplyr::pull(output_name) |>
      dplyr::first()
  )
}
