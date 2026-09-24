#' @keywords internal
#' @noRd
app_init <- function(debug = getOption("the_general.debug", FALSE)) {
  requireNamespace("shiny", quietly = TRUE)
  requireNamespace("shinydashboard", quietly = TRUE)
  requireNamespace("htmltools", quietly = TRUE)
  requireNamespace("logger", quietly = TRUE)
  requireNamespace("uuid", quietly = TRUE)
  requireNamespace("glue", quietly = TRUE)
  requireNamespace("stringr", quietly = TRUE)

  options(the_general.debug = isTRUE(debug))
  options(shiny.minified = TRUE)

  log_file <- getOption("the_general.log_file", "log_file.log")
  file_appender <- logger::appender_file(log_file)

  logger::log_appender(function(lines) {
    cat(paste(lines, collapse = "\n"), "\n", sep = "")
    file_appender(lines)
  })

  if (isTRUE(debug)) {
    logger::log_threshold(logger::DEBUG)
  } else {
    logger::log_threshold(logger::INFO)
  }

  logger::log_layout(logger::layout_glue_colors)

  logger::log_info(glue::glue(
    "App init started | debug={isTRUE(debug)} | logs=console + {log_file}"
  ))

  options(shiny.error = function() {
    logger::log_error(glue::glue(
      "Global Shiny error: {geterrmessage()}"
    ))
  })

  running <- utils::sessionInfo()$running
  if (is.null(running)) {
    running <- ""
  }

  assign(
    "on_windows",
    grepl("windows", tolower(running), fixed = TRUE),
    .GlobalEnv
  )

  logger::log_debug(glue::glue(
    "Runtime detected | running={running} | on_windows={get('on_windows', .GlobalEnv)}"
  ))

  ensure_assets()
  ensure_data()

  logger::log_info("App init complete")

  invisible(TRUE)
}

### Map /www -------------------------------------------------------------------

ensure_assets <- function() {
  path <- system.file("app/www", package = "thegeneral")
  if (!nzchar(path) || !dir.exists(path)) {
    if (dir.exists("inst/app/www")) {
      path <- normalizePath("inst/app/www", winslash = "/")
    } else {
      return(invisible(FALSE))
    }
  }
  shiny::addResourcePath("www", path)
  invisible(TRUE)
}

### App data and derived globals -----------------------------------------------

ensure_data <- function() {
  ### Load CSV shipped with the package or from local data/ ---------------------
  p <- if (file.exists("data/shiny_df.csv")) {
    "data/shiny_df.csv"
  } else {
    system.file("data", "shiny_df.csv", package = "thegeneral")
  }

  ### Handle missing data file -------------------------------------------------
  if (!nzchar(p) || !file.exists(p)) {
    assign("shiny_df", NULL, .GlobalEnv)
    assign("shiny_df_original", NULL, .GlobalEnv)
    assign("numeric_vars", character(0), .GlobalEnv)
    assign("uno_reverso_numeric", character(0), .GlobalEnv)
    return(invisible(FALSE))
  }

  ### Load and assign data -----------------------------------------------------
  df <- utils::read.csv(p, stringsAsFactors = FALSE)
  assign("shiny_df", df, .GlobalEnv)
  assign("shiny_df_original", df, .GlobalEnv)

  ### Generate numeric variable lists ------------------------------------------
  nms <- names(df)
  idx <- grepl("^Num", nms)
  numeric_vars <- nms[idx]
  assign("numeric_vars", numeric_vars, .GlobalEnv)
  assign("uno_reverso_numeric", rev(numeric_vars), .GlobalEnv)

  invisible(TRUE)
}
