# Standalone, readable scripts for the nine dashboard plot types.
# Templates contain only the selected analysis; no Shiny helpers are exported.

dashboard_r_scripts_safe_name <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) "dashboard_plot" else x
}

dashboard_r_scripts_literal <- function(x) {
  paste(deparse(x, width.cutoff = 72L), collapse = "\n")
}

dashboard_r_scripts_filename <- function(pin, index) {
  paste0(sprintf("%02d", index), "_", dashboard_r_scripts_safe_name(pin$type), ".R")
}

dashboard_r_scripts_setting <- function(name, value) {
  lines <- strsplit(dashboard_r_scripts_literal(value), "\n", fixed = TRUE)[[1]]
  c(paste0(name, " <- ", lines[[1]]), lines[-1])
}

dashboard_r_scripts_spec <- function(pin) {
  p <- pin$params
  switch(
    pin$type,
    line_single = list(time = p$TimeVariable, value = p$NumericVariable),
    line_grouped = list(time = p$TimeVariable2, group = p$GroupVar, value = p$NumVar),
    bar_simple = list(time = p$TimeVariable3, group = p$GroupVar, value = p$NumVar),
    bar_stacked = list(
      time = p$TimeVariable4, group = p$GroupVar1,
      group2 = p$GroupVar2, value = p$NumVar
    ),
    bar_grouped = list(
      time = p$TimeVariable5, group = p$GroupVar1,
      group2 = p$GroupVar2, value = p$NumVar
    ),
    area_stacked = list(time = p$TimeVariable6, group = p$GroupingVar, value = p$NumericVar),
    area_percent = list(
      time = if (identical(p$TimeVariable7, "Year")) "Year" else "Year_Quarter",
      group = p$GroupingVar
    ),
    scatter = list(group = p$GroupVar, x = p$X, y = p$Y),
    density = list(group = p$GroupVar, value = p$NumVar, year = p$Year),
    stop("Unsupported pin type: ", pin$type, call. = FALSE)
  )
}

# Keep the app's period order, including unused factor levels used by bar charts.
dashboard_r_scripts_time_levels <- function(column, data = NULL) {
  x <- if (!is.null(data)) data[[column]] else NULL
  if (is.factor(x)) return(levels(x))
  if (identical(column, "Month")) return(month.abb[c(4:12, 1:3)])
  if (identical(column, "Financial_Quarter")) {
    return(c("Q1 (Apr-Jun)", "Q2 (Jul-Sep)", "Q3 (Oct-Dec)", "Q4 (Jan-Mar)"))
  }
  if (is.null(x)) return(NULL)
  as.character(sort(unique(stats::na.omit(x))))
}

dashboard_r_scripts_import <- function(spec, type, data = NULL) {
  columns <- character()
  if (!is.null(spec$time)) {
    columns <- if (is.null(dashboard_r_scripts_time_levels(spec$time, data))) {
      "    period = factor(.data[[time_column]], ordered = TRUE)"
    } else {
      "    period = factor(.data[[time_column]], levels = time_levels, ordered = TRUE)"
    }
  }
  if (identical(type, "area_percent") && identical(spec$time, "Year")) {
    # Preserve the existing dashboard calculation, without silently fixing it here.
    year_expr <- if (!is.null(data) && is.factor(data$Year)) {
      "match(.data[[time_column]], time_levels)"
    } else {
      "readr::parse_integer(.data[[time_column]])"
    }
    columns <- paste0("    period = ", year_expr, " + 2018L")
  }
  if (!is.null(spec$group)) {
    columns <- c(columns, "    group = factor(.data[[group_column]])")
  }
  if (!is.null(spec$group2)) {
    columns <- c(columns, "    subgroup = factor(.data[[second_group_column]])")
  }
  if (!is.null(spec$value)) {
    columns <- c(columns, "    value = readr::parse_double(.data[[numeric_column]])")
  }
  if (!is.null(spec$x)) {
    columns <- c(columns, "    x = readr::parse_double(.data[[x_column]])",
                 "    y = readr::parse_double(.data[[y_column]])")
  }
  if (identical(type, "density")) {
    columns <- c(columns, "    financial_year = .data$Financial_Year")
  }
  paste(c(
    "  readr::read_csv(",
    "    path,",
    "    col_types = readr::cols(.default = readr::col_character()),",
    "    na = \"\",",
    "    show_col_types = FALSE",
    "  ) |>",
    "  transmute(",
    paste0(columns, c(rep(",", length(columns) - 1L), "")),
    "  )"
  ), collapse = "\n")
}

dashboard_r_scripts_summary <- function(groups, trimmed = FALSE, median_na = FALSE) {
  paste(c(
    paste0("  group_by(", paste(groups, collapse = ", "), ") |>"),
    "  summarise(",
    if (trimmed) "    mean = mean(value, trim = 0.2, na.rm = TRUE)," else
      "    mean = mean(value, na.rm = TRUE),",
    if (median_na) "    median = median(value, na.rm = TRUE)," else
      "    median = median(value),",
    "    n = n(),",
    "    .groups = \"drop\"",
    "  ) |>",
    "  mutate(across(c(mean, median), ~ round(.x, 2)))"
  ), collapse = "\n")
}

dashboard_r_scripts_analysis <- function(type, spec) {
  if (type == "scatter") {
    return(r"---(# Keep complete x/y pairs and their group labels.
analysis_data <- dashboard_data |>
  drop_na(group, x, y)

# The dashboard displays at most 1,000 points. A fixed seed makes this export
# repeatable; a newly drawn sample in the live dashboard can use different rows.
set.seed(sample_seed)
if (nrow(analysis_data) > 1000L) {
  analysis_data <- analysis_data |> slice_sample(n = 1000L)
}
analysis_data <- analysis_data |> mutate(group = droplevels(group)))---")
  }
  if (type == "area_percent") {
    return(r"---(# Match the dashboard's label exclusions. This is a substring match:
# any label containing "NA" or "Other" (ignoring case) is excluded.
# Percentages describe row counts, not a sum of the numeric measurements.
analysis_data <- dashboard_data |>
  filter(!is.na(group), !str_detect(group, regex("NA|Other", ignore_case = TRUE))) |>
  mutate(group = droplevels(group)) |>
  count(period, group, name = "group_n") |>
  mutate(across(where(is.factor), droplevels)) |>
  complete(period, group, fill = list(group_n = 0L)) |>
  group_by(period) |>
  mutate(
    n = sum(group_n),
    percentage = round(100 * group_n / n, 2)
  ) |>
  ungroup() |>
  select(period, group, percentage, n)
# n is the period total, repeated for each group. Rounding may make the displayed
# percentages sum to slightly above or below 100.)---")
  }

  exception <- if (type == "line_single") "Num_Surplus" else "Surplus"
  positive_only <- !identical(spec$value, exception)
  cleaning <- c(
    if (positive_only) c(
      "# Match this dashboard view: exclude zero, negative and missing values.",
      "dashboard_data <- dashboard_data |> filter(value > 0)"
    ) else "# This dashboard measure retains zero and negative values.",
    if (type %in% c("line_single", "line_grouped") && positive_only)
      "reference_data <- reference_data |> filter(value > 0)",
    ""
  )
  if (type == "density") {
    return(c(cleaning, r"---(# Keep the selected financial year and complete observations.
analysis_data <- dashboard_data |>
  filter(financial_year == selected_year) |>
  drop_na(group, value)

# Match the dashboard: retain the pooled 5th-95th percentile range, inclusive.
# These cutoffs are calculated across groups, not separately within each group.
cutoffs <- quantile(analysis_data$value, probs = c(0.05, 0.95), na.rm = TRUE)
analysis_data <- analysis_data |>
  filter(between(value, cutoffs[[1]], cutoffs[[2]])) |>
  transmute(group = droplevels(group), value))---"))
  }
  if (type == "line_single") {
    body <- dashboard_r_scripts_summary("period", trimmed = TRUE, median_na = TRUE)
    return(c(cleaning,
      "# A 20% trimmed mean removes 20% from each tail before averaging.",
      "# Each period also reports its median and contributing row count.",
      "summarise_series <- function(data) {",
      "  data |>", paste0("  ", strsplit(body, "\n", fixed = TRUE)[[1]]), "}", "",
      "analysis_data <- summarise_series(dashboard_data)", "",
      "# Add the full-data comparison only when the eligible row counts differ.",
      "if (nrow(dashboard_data) != nrow(reference_data)) {",
      "  analysis_data <- bind_rows(",
      "    analysis_data |> mutate(series = \"Filtered\"),",
      "    summarise_series(reference_data) |> mutate(series = \"All\")",
      "  )", "}"
    ))
  }
  if (type == "line_grouped") {
    return(c(cleaning,
      "# Each line shows a 20% trimmed mean within a period and group.",
      "# This removes 20% from each tail; it is not the ordinary arithmetic mean.",
      "group_summary <- dashboard_data |>",
      dashboard_r_scripts_summary(c("period", "group"), trimmed = TRUE), "",
      "# Match the dashboard's substring exclusions, including labels such as",
      "# 'Other group' or 'NA'. The All series still uses all eligible source rows.",
      "group_summary <- group_summary |>",
      "  filter(!is.na(group), !str_detect(group, regex(\"NA|Other\", ignore_case = TRUE)))", "",
      "overall_summary <- reference_data |>",
      paste0(dashboard_r_scripts_summary("period", trimmed = TRUE), " |>"),
      "  mutate(group = \"All\")", "",
      "analysis_data <- bind_rows(group_summary, overall_summary) |>",
      "  mutate(group = factor(group))"
    ))
  }
  groups <- c("period", "group", if (!is.null(spec$group2)) "subgroup")
  exclusions <- c(
    "  filter(!is.na(group), !str_detect(group, regex(\"NA|Other\", ignore_case = TRUE)))"
  )
  if (!is.null(spec$group2)) {
    exclusions <- c(paste0(exclusions, " |>"),
      "  filter(!is.na(subgroup),",
      "         !str_detect(subgroup, regex(\"NA|Other\", ignore_case = TRUE)))")
  }
  bars <- type %in% c("bar_simple", "bar_stacked", "bar_grouped")
  c(cleaning,
    "# Calculate the arithmetic mean, median and row count for each group.",
    "analysis_data <- dashboard_data |>",
    dashboard_r_scripts_summary(groups, median_na = type == "bar_simple"), "",
    if (bars) c(
      "# The dashboard uses the last level of its ordered period field.",
      "# For Month this is March (April-March financial-year order).",
      "# This does not select the latest calendar date or pool all periods.",
      "latest_period <- tail(time_levels, 1)",
      "analysis_data <- analysis_data |> filter(period == latest_period)", ""
    ),
    "# Match the dashboard's case-insensitive NA/Other substring exclusions.",
    "analysis_data <- analysis_data |>",
    paste0(exclusions, c(rep("", length(exclusions) - 1L), " |>")),
    paste0("  mutate(across(c(", paste(groups[-1], collapse = ", "), "), droplevels))")
  )
}

dashboard_r_scripts_plot <- function(type) {
  switch(type,
    line_single = r"---(# Connecting period summaries describes a trend; it is not a fitted model.
if ("series" %in% names(analysis_data)) {
  plot <- ggplot(analysis_data, aes(period, mean, colour = series, group = series)) +
    geom_line(linewidth = 0.75) +
    geom_point() +
    scale_colour_manual(values = palette)
} else {
  plot <- ggplot(analysis_data, aes(period, mean, group = 1)) +
    geom_line(linewidth = 0.75, colour = "#333333") +
    geom_point(colour = "#333333")
}
plot <- plot + labs(x = time_column, y = numeric_column))---",
    line_grouped = r"---(# Compare group trends with the All reference series.
plot <- ggplot(analysis_data, aes(period, mean, colour = group, group = group)) +
  geom_line(linewidth = 0.75) +
  geom_point() +
  scale_colour_manual(values = palette) +
  labs(x = time_column, y = numeric_column, colour = group_column))---",
    bar_simple = r"---(# Bar heights show group means, not frequencies or totals.
plot <- ggplot(analysis_data, aes(group, mean, fill = group)) +
  geom_col(colour = "#333333", linewidth = 0.75) +
  scale_fill_manual(values = palette) +
  labs(x = group_column, y = numeric_column, fill = group_column))---",
    bar_stacked = r"---(# Each segment is a subgroup mean. Stacked height is a sum of means;
# it is not a pooled mean, a count, or a sum of the original measurements.
plot <- ggplot(analysis_data, aes(group, mean, fill = subgroup)) +
  geom_col(colour = "#333333", linewidth = 0.75) +
  scale_fill_manual(values = palette) +
  labs(x = group_column, y = numeric_column, fill = second_group_column))---",
    bar_grouped = r"---(# Side-by-side bars compare subgroup means within each main group.
plot <- ggplot(analysis_data, aes(group, mean, fill = subgroup)) +
  geom_col(position = "dodge", colour = "#333333", linewidth = 0.75) +
  scale_fill_manual(values = palette) +
  labs(x = group_column, y = numeric_column, fill = second_group_column))---",
    area_stacked = r"---(# Complete absent period/group combinations for the stacked display only.
# These display zeros are not observed zero measurements in analysis_data.
plot_data <- analysis_data |>
  complete(period, group, fill = list(mean = 0), explicit = FALSE) |>
  mutate(period_position = as.integer(period))

# Each band is a group mean; the stacked height is a sum of means, not a total.
plot <- ggplot(plot_data, aes(period_position, mean, fill = group, group = group)) +
  geom_area(colour = "#333333", linewidth = 1) +
  scale_x_continuous(breaks = seq_along(time_levels), labels = time_levels) +
  scale_fill_manual(values = palette) +
  labs(x = time_column, y = numeric_column, fill = group_column))---",
    area_percent = r"---(# A band's thickness is its share of rows in that period.
# Numeric x positions keep area geometry continuous between ordered periods.
plot_data <- analysis_data |>
  mutate(period_position = match(as.character(period), as.character(sort(unique(period)))))
period_labels <- as.character(sort(unique(analysis_data$period)))
plot <- ggplot(plot_data, aes(period_position, percentage, fill = group, group = group)) +
  geom_area(alpha = 0.6, colour = "#333333", linewidth = 1) +
  scale_x_continuous(breaks = seq_along(period_labels), labels = period_labels) +
  scale_fill_manual(values = palette) +
  labs(x = time_column, y = "Percentage of rows", fill = group_column))---",
    scatter = r"---(# Fit a separate ordinary least-squares line for each colour group.
# Dashed lines show association, not causation; confidence bands are hidden.
plot <- ggplot(analysis_data, aes(x, y, colour = group)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed") +
  scale_colour_manual(values = palette) +
  labs(x = x_column, y = y_column, colour = group_column))---",
    density = r"---(# Kernel density curves compare distribution shapes after trimming.
# Each group's curve has area one: heights are density, not sample counts.
# The default bandwidth controls smoothing; small groups can be unstable.
plot <- ggplot(analysis_data, aes(value, fill = group)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = palette) +
  labs(x = numeric_column, y = "Density", fill = group_column))---"
  )
}

dashboard_r_scripts_script_text <- function(
  pin, index, data_file = "your-downloaded-dashboard-data.csv",
  reference_file = data_file, data = NULL
) {
  spec <- dashboard_r_scripts_spec(pin)
  type <- pin$type
  is_line <- type %in% c("line_single", "line_grouped")
  setting <- dashboard_r_scripts_setting
  time_levels <- if (!is.null(spec$time)) dashboard_r_scripts_time_levels(spec$time, data)
  settings <- c(
    setting("data_file", data_file),
    if (is_line) c("# Full mapped data before the Home filter, for the All comparison.",
                  setting("reference_file", reference_file)),
    setting("output_prefix", tools::file_path_sans_ext(dashboard_r_scripts_filename(pin, index))),
    setting("plot_title", as.character(pin$title %||% type)),
    if (!is.null(spec$time)) c(setting("time_column", spec$time), setting("time_levels", time_levels)),
    if (!is.null(spec$group)) setting("group_column", spec$group),
    if (!is.null(spec$group2)) setting("second_group_column", spec$group2),
    if (!is.null(spec$value)) setting("numeric_column", spec$value),
    if (!is.null(spec$x)) c(setting("x_column", spec$x), setting("y_column", spec$y)),
    if (type == "density") setting("selected_year", as.character(spec$year)),
    if (type == "scatter") "sample_seed <- 42L",
    if (type != "density") c("# NA leaves that end of the y-axis automatic; limits zoom without dropping rows.",
      setting("y_limits", c(pin$params$YAxisMin %||% NA_real_, pin$params$YAxisMax %||% NA_real_)))
  )
  importer <- dashboard_r_scripts_import(spec, type, data)
  import <- if (is_line) c(
    "read_analysis_data <- function(path) {", importer, "}", "",
    "dashboard_data <- read_analysis_data(data_file)",
    "reference_data <- read_analysis_data(reference_file)"
  ) else paste0("dashboard_data <-\n", sub("    path,", "    data_file,", importer, fixed = TRUE))

  text <- c(
    "# The General | standalone dashboard analysis",
    "# Run from the folder containing this script and its CSV files (R >= 4.1).",
    "# The settings reproduce the current dashboard view at download time.",
    "# Use mapped CSV data from the export, not the original uncleaned upload.",
    "# PNG and analysis CSV outputs are written into this working directory.", "",
    "# 1. Packages -------------------------------------------------------------",
    "library(dplyr)", "library(tidyr)", "library(ggplot2)", "library(stringr)",
    "# readr is called explicitly below. No Shiny or thegeneral installation is needed.", "",
    "# 2. Settings -------------------------------------------------------------", settings, "",
    "# 3. Read and prepare data ------------------------------------------------",
    "# Read text first so categorical codes such as 0012 keep their leading zeros.",
    "# Parse only the mapped numeric measures. Rename columns to readable roles.",
    if (type == "area_percent" && identical(spec$time, "Year")) c(
      "# Compatibility note: the dashboard's legacy Year calculation adds 2018.",
      "# This export preserves that rule; check these year labels before reporting."
    ),
    import,
    if (!is.null(spec$time) && is.null(time_levels) &&
        !(type == "area_percent" && identical(spec$time, "Year")))
      "time_levels <- levels(dashboard_data$period)",
    "",
    "# 4. Analyse --------------------------------------------------------------",
    dashboard_r_scripts_analysis(type, spec), "",
    "# 5. Plot -----------------------------------------------------------------",
    "# The General's palette, in the same order as the dashboard.",
    paste0("palette <- ", dashboard_r_scripts_literal(general_palette())), "",
    dashboard_r_scripts_plot(type), "",
    "plot <- plot +",
    if (type %in% c("line_single", "scatter")) "  theme_bw(base_size = 15) +" else
      "  theme_classic(base_size = 15) +",
    "  theme(",
    "    text = element_text(colour = \"#333333\"),",
    "    axis.text = element_text(colour = \"#333333\"),",
    "    legend.text = element_text(size = 10)",
    "  ) +",
    "  labs(title = plot_title)", "",
    if (type != "density") c(
      "if (!all(is.na(y_limits))) {",
      "  plot <- plot + coord_cartesian(ylim = y_limits)", "}", ""
    ),
    "# 6. Save and display -----------------------------------------------------",
    "# Save the calculated values as well as the image so the result is inspectable.",
    "readr::write_csv(analysis_data, paste0(output_prefix, \"_data.csv\"), na = \"\")",
    "ggsave(", "  filename = paste0(output_prefix, \".png\"),", "  plot = plot,",
    "  width = 9, height = 5.5, units = \"in\", dpi = 300", ")", "",
    "if (interactive()) print(plot)"
  )
  strsplit(paste(text, collapse = "\n"), "\n", fixed = TRUE)[[1]]
}

dashboard_r_scripts_readme <- function(pin_list, bundled = FALSE, comparison = FALSE) {
  c(
    "The General: standalone R analyses", "",
    if (bundled) c(
      "dashboard_data.csv contains the mapped rows currently selected in the app.",
      if (comparison) "dashboard_all_data.csv contains the full mapped data for line comparisons."
    ) else "Download mapped data with 'Download your data', then update data_file in each script.",
    "Extract this export into its own folder and use that folder as R's working directory.",
    "Run any .R script in RStudio or with Rscript. Requires R >= 4.1 and the packages",
    "dplyr, tidyr, readr, ggplot2 and stringr (already used by the app).", "",
    "Scripts have six sections: packages, settings, data, analysis, plot and save.",
    "Each writes a PNG and a CSV of the analysed values. Edit settings at the top.",
    "Data reflects the current Home filter at export time, not a historical pin-time snapshot.",
    "Line comparisons use the full mapped data. Scatter samples use a fixed seed.",
    "Comments state each view's existing exclusions, trimming and calculation rules.",
    "The percentage-area Year option preserves the app's legacy +2018 conversion.",
    "These scripts do not require Shiny or thegeneral.", "", "Included scripts:",
    vapply(seq_along(pin_list), function(i) dashboard_r_scripts_filename(pin_list[[i]], i), character(1))
  )
}

dashboard_r_scripts_write_zip <- function(pin_list, file, data = NULL, data_unfiltered = NULL) {
  if (length(pin_list) == 0L) stop("No dashboard pins to export.", call. = FALSE)
  # Validate pin types before creating a partial download.
  lapply(pin_list, dashboard_r_scripts_spec)
  out_dir <- tempfile("r_export_")
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  bundled <- !is.null(data)
  comparison <- bundled && any(vapply(
    pin_list, function(pin) pin$type %in% c("line_single", "line_grouped"), logical(1)
  ))
  if (comparison && is.null(data_unfiltered)) {
    stop("Full mapped data is required for line-chart comparisons.", call. = FALSE)
  }
  write_data <- function(x, name) {
    # CSV must retain quarter labels (2024 Q2), not their internal numeric codes.
    x |>
      dplyr::mutate(dplyr::across(dplyr::where(~ inherits(.x, "yearqtr")), as.character)) |>
      readr::write_csv(file.path(out_dir, name), na = "")
  }
  if (bundled) write_data(data, "dashboard_data.csv")
  if (comparison) write_data(data_unfiltered, "dashboard_all_data.csv")
  writeLines(dashboard_r_scripts_readme(pin_list, bundled, comparison),
             file.path(out_dir, "RUN_SCRIPTS.txt"), useBytes = TRUE)
  for (i in seq_along(pin_list)) {
    text <- dashboard_r_scripts_script_text(
      pin_list[[i]], i,
      data_file = if (bundled) "dashboard_data.csv" else "your-downloaded-dashboard-data.csv",
      reference_file = if (comparison) "dashboard_all_data.csv" else "your-downloaded-dashboard-data.csv",
      data = data
    )
    writeLines(enc2utf8(text), file.path(out_dir, dashboard_r_scripts_filename(pin_list[[i]], i)),
               useBytes = TRUE)
  }
  file <- file.path(normalizePath(dirname(file), winslash = "/", mustWork = TRUE), basename(file))
  if (file.exists(file)) unlink(file)
  files <- list.files(out_dir)
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zipr(zipfile = file, files = files, root = out_dir)
  } else {
    old_wd <- setwd(out_dir)
    on.exit(setwd(old_wd), add = TRUE, after = FALSE)
    utils::zip(zipfile = file, files = files, flags = "-r9Xj")
  }
  invisible(file)
}
