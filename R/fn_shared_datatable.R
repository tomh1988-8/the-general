# File: R/mod_shared_datatable.R

general_dt_ellipsis <- function(x, max_chars = 3L) {
  x <- as.character(x)
  too_long <- nchar(x) > max_chars

  x[too_long] <- paste0(
    substr(x[too_long], 1L, max_chars - 1L),
    "\u2026"
  )

  x
}

general_dt_display_colnames <- function(column_names, style_type = "single") {
  labels <- sub("\\.Percent$", "", column_names)

  if (length(labels) <= 8L) {
    return(labels)
  }

  if (identical(style_type, "proportions")) {
    key_columns <- labels %in%
      c(
        "Month",
        "Financial_Quarter",
        "DOTW",
        "Financial_Year",
        "Year",
        "Year_Quarter"
      ) |
      startsWith(labels, "Cat_") |
      startsWith(labels, "Date_")

    labels[!key_columns] <- general_dt_ellipsis(labels[!key_columns], 3L)
    return(labels)
  }

  general_dt_ellipsis(labels, 10L)
}

general_dt_header_titles <- function(column_names) {
  labels <- sub("\\.Percent$", "", column_names)

  ifelse(
    grepl("\\.Percent$", column_names),
    paste0(labels, " percentage"),
    labels
  )
}

general_dt_error_table <- function(message) {
  DT::datatable(
    tibble::tibble(Message = message),
    options = list(
      dom = "t",
      ordering = FALSE,
      paging = FALSE,
      searching = FALSE
    ),
    class = "compact hover stripe general-dt-error-table",
    rownames = FALSE
  )
}

general_dt_asset_version <- function() {
  path <- system.file(
    "app/www/js/mod_shared_datatable.js",
    package = "thegeneral",
    mustWork = FALSE
  )

  if (
    !nzchar(path) &&
      file.exists("inst/app/www/js/mod_shared_datatable.js")
  ) {
    path <- normalizePath(
      "inst/app/www/js/mod_shared_datatable.js",
      winslash = "/",
      mustWork = TRUE
    )
  }

  if (nzchar(path) && file.exists(path)) {
    return(as.integer(file.info(path)$mtime))
  }

  as.integer(Sys.time())
}

general_dt_add_header_titles <- function(dt_options, header_titles) {
  header_defs <- lapply(seq_along(header_titles), function(index) {
    list(
      targets = index - 1L,
      name = header_titles[[index]]
    )
  })

  existing_defs <- dt_options$columnDefs
  if (is.null(existing_defs)) {
    existing_defs <- list()
  }

  dt_options$columnDefs <- c(existing_defs, header_defs)
  dt_options
}
