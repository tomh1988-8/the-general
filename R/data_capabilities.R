# R/data_capabilities.R

#' Map validated dataset structure to app capabilities
#'
#' This file owns capability mapping only. It converts validated structure into
#' explicit support flags, available variable inventories, and human-readable
#' disabled reasons for the UI. It does not parse data and it does not run
#' downstream analyses.
#'
#' @param validation_result List returned by `data_validate_dataset()`.
#'
#' @return A list with `capability_flags`, `available_vars`,
#'   `disabled_reasons`, and `module_support`.
data_capabilities_dataset <- function(validation_result) {
  validation_checks <- validation_result$validation_checks
  variable_sets <- validation_result$variable_sets
  validation_status <- validation_result$validation_status

  capability_flags <- list(
    supports_frequencies = data_capabilities_supports_frequencies(
      validation_checks = validation_checks,
      variable_sets = variable_sets,
      validation_status = validation_status
    ),
    supports_proportions = data_capabilities_supports_proportions(
      validation_checks = validation_checks,
      variable_sets = variable_sets,
      validation_status = validation_status
    ),
    supports_averages = data_capabilities_supports_averages(
      validation_checks = validation_checks,
      variable_sets = variable_sets,
      validation_status = validation_status
    ),
    supports_time_series = data_capabilities_supports_time_series(
      validation_checks = validation_checks,
      variable_sets = variable_sets,
      validation_status = validation_status
    ),
    supports_grouped_plots = data_capabilities_supports_grouped_plots(
      validation_checks = validation_checks,
      variable_sets = variable_sets,
      validation_status = validation_status
    ),
    supports_scatter = data_capabilities_supports_scatter(
      validation_checks = validation_checks,
      variable_sets = variable_sets,
      validation_status = validation_status
    ),
    supports_density = data_capabilities_supports_density(
      validation_checks = validation_checks,
      variable_sets = variable_sets,
      validation_status = validation_status
    )
  )

  disabled_reasons <- purrr::imap_chr(
    capability_flags,
    \(supported, capability_name) {
      data_capabilities_disabled_reason(
        capability_name = capability_name,
        supported = supported,
        validation_checks = validation_checks,
        variable_sets = variable_sets,
        validation_status = validation_status
      )
    }
  )

  module_support <- tibble::tibble(
    capability = names(capability_flags),
    supported = unlist(capability_flags),
    disabled_reason = unname(disabled_reasons)
  )

  list(
    capability_flags = capability_flags,
    available_vars = variable_sets,
    disabled_reasons = disabled_reasons,
    module_support = module_support
  )
}

#' Determine whether grouped frequencies are supported
#'
#' @param validation_checks Dataset-level validation checks.
#' @param variable_sets Validated variable inventories.
#' @param validation_status Dataset validation status.
#'
#' @return Logical scalar.
data_capabilities_supports_frequencies <- function(
  validation_checks,
  variable_sets,
  validation_status
) {
  if (validation_status != "valid") {
    return(FALSE)
  }

  data_capabilities_check_passed(validation_checks, "has_valid_dimension") &&
    length(variable_sets$grouping_vars) > 0L
}

#' Determine whether grouped proportions are supported
#'
#' @param validation_checks Dataset-level validation checks.
#' @param variable_sets Validated variable inventories.
#' @param validation_status Dataset validation status.
#'
#' @return Logical scalar.
data_capabilities_supports_proportions <- function(
  validation_checks,
  variable_sets,
  validation_status
) {
  if (validation_status != "valid") {
    return(FALSE)
  }

  data_capabilities_check_passed(validation_checks, "has_valid_dimension") &&
    length(variable_sets$grouping_vars) > 0L
}

#' Determine whether numeric summary analyses are supported
#'
#' @param validation_checks Dataset-level validation checks.
#' @param variable_sets Validated variable inventories.
#' @param validation_status Dataset validation status.
#'
#' @return Logical scalar.
data_capabilities_supports_averages <- function(
  validation_checks,
  variable_sets,
  validation_status
) {
  if (validation_status != "valid") {
    return(FALSE)
  }

  data_capabilities_check_passed(validation_checks, "has_valid_measure") &&
    length(variable_sets$measure_vars) > 0L
}

#' Determine whether time-series analysis is supported
#'
#' Time series requires at least one safe measure and one valid date field.
#'
#' @param validation_checks Dataset-level validation checks.
#' @param variable_sets Validated variable inventories.
#' @param validation_status Dataset validation status.
#'
#' @return Logical scalar.
data_capabilities_supports_time_series <- function(
  validation_checks,
  variable_sets,
  validation_status
) {
  if (validation_status != "valid") {
    return(FALSE)
  }

  data_capabilities_check_passed(validation_checks, "has_valid_measure") &&
    data_capabilities_check_passed(validation_checks, "has_valid_date") &&
    length(variable_sets$measure_vars) > 0L &&
    length(variable_sets$date_vars) > 0L
}

#' Determine whether grouped measure plots are supported
#'
#' Grouped plots require a usable grouping column plus at least one safe
#' measure.
#'
#' @param validation_checks Dataset-level validation checks.
#' @param variable_sets Validated variable inventories.
#' @param validation_status Dataset validation status.
#'
#' @return Logical scalar.
data_capabilities_supports_grouped_plots <- function(
  validation_checks,
  variable_sets,
  validation_status
) {
  if (validation_status != "valid") {
    return(FALSE)
  }

  data_capabilities_check_passed(validation_checks, "has_valid_measure") &&
    data_capabilities_check_passed(validation_checks, "has_grouping_columns") &&
    length(variable_sets$measure_vars) > 0L &&
    length(variable_sets$grouping_vars) > 0L
}

#' Determine whether scatter plots are supported
#'
#' Scatter requires at least two safe measures.
#'
#' @param validation_checks Dataset-level validation checks.
#' @param variable_sets Validated variable inventories.
#' @param validation_status Dataset validation status.
#'
#' @return Logical scalar.
data_capabilities_supports_scatter <- function(
  validation_checks,
  variable_sets,
  validation_status
) {
  if (validation_status != "valid") {
    return(FALSE)
  }

  data_capabilities_check_passed(validation_checks, "has_valid_measure") &&
    length(variable_sets$measure_vars) >= 2L
}

#' Determine whether density plots are supported
#'
#' Density requires at least one safe measure.
#'
#' @param validation_checks Dataset-level validation checks.
#' @param variable_sets Validated variable inventories.
#' @param validation_status Dataset validation status.
#'
#' @return Logical scalar.
data_capabilities_supports_density <- function(
  validation_checks,
  variable_sets,
  validation_status
) {
  if (validation_status != "valid") {
    return(FALSE)
  }

  data_capabilities_check_passed(validation_checks, "has_valid_measure") &&
    length(variable_sets$measure_vars) > 0L
}

#' Read a named validation check
#'
#' @param validation_checks Tibble of dataset-level validation checks.
#' @param check_name Name of the check to read.
#'
#' @return Logical scalar.
data_capabilities_check_passed <- function(validation_checks, check_name) {
  matched <- validation_checks |>
    dplyr::filter(check == check_name) |>
    dplyr::pull(passed)

  if (length(matched) == 0L) {
    return(FALSE)
  }

  matched[[1]]
}

#' Explain why a capability is disabled
#'
#' @param capability_name Capability name.
#' @param supported Whether the capability is supported.
#' @param validation_checks Dataset-level validation checks.
#' @param variable_sets Validated variable inventories.
#' @param validation_status Dataset validation status.
#'
#' @return Character scalar. Empty string when supported.
data_capabilities_disabled_reason <- function(
  capability_name,
  supported,
  validation_checks,
  variable_sets,
  validation_status
) {
  if (supported) {
    return("")
  }

  if (validation_status != "valid") {
    return("Dataset is not structurally valid for downstream analysis")
  }

  if (capability_name %in% c("supports_frequencies", "supports_proportions")) {
    if (
      !data_capabilities_check_passed(validation_checks, "has_valid_dimension")
    ) {
      return(
        "No valid dimension columns are available for counting or grouping"
      )
    }

    if (
      !data_capabilities_check_passed(validation_checks, "has_grouping_columns")
    ) {
      return(
        "Available dimensions are too high-cardinality or otherwise unsuitable for grouping"
      )
    }
  }

  if (capability_name == "supports_averages") {
    if (
      !data_capabilities_check_passed(validation_checks, "has_valid_measure")
    ) {
      return("No numeric measure columns are safe for summaries")
    }
  }

  if (capability_name == "supports_time_series") {
    if (!data_capabilities_check_passed(validation_checks, "has_valid_date")) {
      return("No valid date field is available for time-based analysis")
    }

    if (
      !data_capabilities_check_passed(validation_checks, "has_valid_measure")
    ) {
      return(
        "No numeric measure columns are available for time-based summaries"
      )
    }
  }

  if (capability_name == "supports_grouped_plots") {
    if (
      !data_capabilities_check_passed(validation_checks, "has_valid_measure")
    ) {
      return("No numeric measure columns are safe for grouped plots")
    }

    if (
      !data_capabilities_check_passed(validation_checks, "has_grouping_columns")
    ) {
      return("No usable grouping columns are available for grouped plots")
    }
  }

  if (capability_name == "supports_scatter") {
    if (length(variable_sets$measure_vars) < 2L) {
      return("At least two safe measure columns are required for scatter plots")
    }
  }

  if (capability_name == "supports_density") {
    if (
      !data_capabilities_check_passed(validation_checks, "has_valid_measure")
    ) {
      return("No numeric measure columns are available for density plots")
    }
  }

  "Capability requirements were not met"
}
