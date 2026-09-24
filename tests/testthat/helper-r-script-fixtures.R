# Synthetic fixtures shared by export regression tests and the patch walkthrough.
r_script_fixture <- function() {
  grid <- tidyr::expand_grid(
    period = 0:3,
    group = c("0012", "East", "West", "Other group", "NA team", NA_character_),
    subgroup = c("Alpha", "Beta"),
    observation = seq_len(8)
  )
  values <- c(-10, 0, 10, 20, 30, 40, NA_real_, 1000)
  delta <- 5 * (match(grid$group, c("0012", "East", "West", "Other group", "NA team")) - 1)
  delta[is.na(delta)] <- 0
  raw <- tibble::tibble(
    Event_Date = as.Date(c("2024-04-15", "2024-07-15", "2024-10-15", "2025-03-15"))[grid$period + 1L],
    Group = grid$group,
    Subgroup = grid$subgroup,
    Value = ifelse(values[grid$observation] > 0,
                   values[grid$observation] + grid$period * 2 + delta,
                   values[grid$observation]),
    X = seq_len(nrow(grid)),
    Y = seq_len(nrow(grid)) * 1.2 + values[grid$observation] / 5
  )
  contract <- data_ingest_uploaded_sheet(
    raw, source_name = "r-export-fixture", date_column = "Event_Date",
    numeric_columns = c("Value", "X", "Y"), categorical_columns = c("Group", "Subgroup")
  )
  data <- contract$standardized_data
  list(raw = raw, all = data, filtered = dplyr::filter(data, Cat_subgroup == "Alpha"))
}

r_script_pins <- function() {
  make_pin <- function(type, params) list(type = type, title = paste("Example", type), params = params)
  list(
    make_pin("line_single", list(TimeVariable = "Month", NumericVariable = "Num_value")),
    make_pin("line_grouped", list(TimeVariable2 = "Month", GroupVar = "Cat_group", NumVar = "Num_value")),
    make_pin("bar_simple", list(TimeVariable3 = "Month", GroupVar = "Cat_group", NumVar = "Num_value")),
    make_pin("bar_stacked", list(TimeVariable4 = "Month", GroupVar1 = "Cat_group", GroupVar2 = "Cat_subgroup", NumVar = "Num_value")),
    make_pin("bar_grouped", list(TimeVariable5 = "Month", GroupVar1 = "Cat_group", GroupVar2 = "Cat_subgroup", NumVar = "Num_value")),
    make_pin("area_stacked", list(TimeVariable6 = "Year_Quarter", GroupingVar = "Cat_group", NumericVar = "Num_value")),
    make_pin("area_percent", list(TimeVariable7 = "Financial_Quarter", GroupingVar = "Cat_group")),
    make_pin("scatter", list(GroupVar = "Cat_group", X = "Num_x", Y = "Num_y", YAxisMin = 0, YAxisMax = 800)),
    make_pin("density", list(GroupVar = "Cat_group", NumVar = "Num_value", Year = "2024-25"))
  )
}

r_script_canonical <- function(data) {
  if ("period" %in% names(data)) data$period <- as.character(data$period)
  data |>
    dplyr::mutate(dplyr::across(dplyr::where(is.factor), as.character)) |>
    dplyr::select(dplyr::all_of(sort(names(data)))) |>
    dplyr::arrange(dplyr::across(dplyr::everything())) |>
    tibble::as_tibble()
}

# Adapt only output names/types; the app's functions are the independent reference.
r_script_expected <- function(pin, data, all_data) {
  withr::local_seed(42)
  expected <- dashboard_pin_data(pin, data, all_data)
  spec <- dashboard_r_scripts_spec(pin)
  if (pin$type == "scatter") {
    return(tibble::tibble(group = as.character(expected[[spec$group]]),
                          x = expected[[spec$x]], y = expected[[spec$y]]))
  }
  if (pin$type == "density") {
    return(tibble::tibble(group = as.character(expected[[spec$group]]), value = expected[[spec$value]]))
  }
  if (pin$type == "area_percent") {
    return(tibble::tibble(period = as.character(expected$Time), group = as.character(expected$Condition),
                          percentage = expected$Percentage, n = expected$n))
  }
  result <- tibble::tibble(period = as.character(expected[[spec$time]]),
                           mean = expected$Mean, median = expected$Median, n = expected[["n()"]])
  if (!is.null(spec$group)) result$group <- as.character(expected[[spec$group]])
  if (!is.null(spec$group2)) result$subgroup <- as.character(expected[[spec$group2]])
  if ("Category" %in% names(expected)) {
    result$series <- ifelse(endsWith(as.character(expected$Category), ": Filtered"), "Filtered", "All")
  }
  result
}

# Run in a fresh R process with only package library paths carried across.
# There are no application functions, globals or Shiny session in that process.
r_script_execute <- function(script, directory) {
  script <- normalizePath(script, winslash = "/", mustWork = TRUE)
  directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  wrapper <- file.path(directory, "run_one.R")
  result_file <- file.path(directory, paste0(tools::file_path_sans_ext(basename(script)), ".rds"))
  log_file <- sub("[.]rds$", ".log", result_file)
  writeLines(c(
    paste0(".libPaths(", dashboard_r_scripts_literal(.libPaths()), ")"),
    paste0("setwd(", dashboard_r_scripts_literal(directory), ")"),
    paste0("source(", dashboard_r_scripts_literal(script), ", encoding = \"UTF-8\")"),
    paste0("saveRDS(analysis_data, ", dashboard_r_scripts_literal(result_file), ")")
  ), wrapper, useBytes = TRUE)
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  withr::local_envvar(R_TESTS = "")
  code <- system2(rscript, c("--vanilla", shQuote(wrapper)), stdout = log_file, stderr = log_file)
  if (code != 0L) stop(paste(readLines(log_file, warn = FALSE), collapse = "\n"), call. = FALSE)
  readRDS(result_file)
}

r_script_evaluate <- function(directory) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  fixture <- r_script_fixture()
  pins <- r_script_pins()
  # Read the actual financial-year label, rather than assume its punctuation.
  pins[[9]]$params$Year <- as.character(fixture$all$Financial_Year[[1]])
  readr::write_csv(fixture$raw, file.path(directory, "live_input.csv"), na = "")
  archive <- file.path(directory, "example_export.zip")
  dashboard_r_scripts_write_zip(pins, archive, fixture$filtered, fixture$all)
  utils::unzip(archive, exdir = directory)
  actual <- expected <- vector("list", length(pins))
  rows <- vector("list", length(pins))
  for (i in seq_along(pins)) {
    script_name <- dashboard_r_scripts_filename(pins[[i]], i)
    script_path <- file.path(directory, script_name)
    parse(script_path, encoding = "UTF-8")
    actual[[i]] <- r_script_execute(script_path, directory)
    expected[[i]] <- r_script_expected(pins[[i]], fixture$filtered, fixture$all)
    equivalent <- isTRUE(all.equal(r_script_canonical(actual[[i]]),
                                  r_script_canonical(expected[[i]]), tolerance = 1e-8))
    lines <- readLines(script_path, warn = FALSE, encoding = "UTF-8")
    rows[[i]] <- tibble::tibble(
      type = pins[[i]]$type, script = script_name, lines = length(lines),
      analysis_rows = nrow(actual[[i]]), matches_dashboard = equivalent,
      png_written = file.exists(sub("[.]R$", ".png", script_path)),
      csv_written = file.exists(sub("[.]R$", "_data.csv", script_path))
    )
    readr::write_csv(dplyr::bind_rows(rows), file.path(directory, "evaluation.csv"))
  }
  unlink(file.path(directory, "run_one.R"))
  list(actual = actual, expected = expected, metrics = dplyr::bind_rows(rows))
}
