#' Data-processing helpers for *The General* Shiny app
#'
#' All functions are prefixed with `data_` so they can be grepped easily and
#' avoid namespace clashes.  They are **pure R** utilities (no Shiny side-effects)
#' which makes them straightforward to unit-test with *testthat*.

#' Return the two canonical default CSV paths used by the app
#'
#' @return character vector of length 2, absolute or project-relative paths
#' @export
#' @keywords internal
data_default_paths <- function() {
  c("data/shiny_df.csv", "data/shiny_df_2.csv")
}

#' Convert a `shiny::fileInput()` data-frame to a named list of datapaths
#'
#' @param files_df data-frame from `input$files`.  Must contain columns `name`
#'   and `datapath`.
#' @return named list: names are *file stems* (sans extension), values are the
#'   corresponding `datapath`s.
#' @export
#' @keywords internal
data_register_uploads <- function(files_df) {
  stopifnot(is.data.frame(files_df))
  if (nrow(files_df) == 0) {
    return(list())
  }

  paths <- files_df$datapath
  names <- tools::file_path_sans_ext(basename(files_df$name))
  stats::setNames(as.list(paths), names)
}

#' Load a dataset by name, preferring user uploads over packaged defaults
#'
#' @param name          character(1). Dataset key selected in the UI.
#' @param uploads       named list as produced by [data_register_uploads()].
#' @param default_paths character vector of packaged datasets.
#' @return `tibble` read with `readr::read_csv()`. Throws if `name` not found.
#' @export
#' @keywords internal
data_load_dataset <- function(
  name,
  uploads = list(),
  default_paths = data_default_paths()
) {
  stopifnot(length(name) == 1L, is.character(name))

  upload_names <- names(uploads)
  default_names <- tools::file_path_sans_ext(basename(default_paths))

  logger::log_debug(glue::glue(
    "data_load_dataset() requested | name={name} | uploads={paste(upload_names, collapse = ', ')} | defaults={paste(default_names, collapse = ', ')}"
  ))

  if (name %in% upload_names) {
    logger::log_info(glue::glue(
      "Loading uploaded dataset | name={name} | path={uploads[[name]]}"
    ))

    return(readr::read_csv(uploads[[name]], show_col_types = FALSE))
  }

  idx <- match(name, default_names)

  if (!is.na(idx)) {
    logger::log_info(glue::glue(
      "Loading default dataset | name={name} | path={default_paths[[idx]]}"
    ))

    return(readr::read_csv(default_paths[[idx]], show_col_types = FALSE))
  }

  logger::log_error(glue::glue(
    "Dataset not found | requested={name} | uploads={paste(upload_names, collapse = ', ')} | defaults={paste(default_names, collapse = ', ')}"
  ))

  rlang::abort(glue::glue("Dataset not found: {name}"))
}

#' Return candidate grouping variables from a data-frame
#'
#' Factors *or* character columns qualify.  Certain admin columns are excluded.
#'
#' @param df `data.frame` / `tibble`
#' @param exclude character vector of columns to drop unconditionally.
#' @return character vector of column names in the order they appear in `df`.
#' @export
#' @keywords internal
data_group_vars <- function(df, exclude = c("Row_ID", "unique_id")) {
  stopifnot(is.data.frame(df))
  keep <- purrr::map_lgl(df, ~ is.factor(.x) || is.character(.x))
  vars <- names(df)[keep]
  setdiff(vars, exclude)
}

#' Return available levels for a chosen grouping variable
#'
#' @param df        data-frame containing `group_var`.
#' @param group_var character(1) - column name.
#' @return character vector of distinct levels, sorted, with `NA` removed.
#' @export
#' @keywords internal
data_group_levels <- function(df, group_var) {
  stopifnot(is.data.frame(df), length(group_var) == 1L)
  vec <- df[[group_var]]
  if (is.factor(vec)) {
    levels(vec)
  } else {
    sort(unique(stats::na.omit(as.character(vec))))
  }
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
      length(group_var) == 0L ||
      !nzchar(group_var[[1]]) ||
      is.null(group_levels) ||
      length(group_levels) == 0L
  ) {
    return(tibble::as_tibble(df))
  }

  df |>
    tibble::as_tibble() |>
    dplyr::filter(.data[[group_var[[1]]]] %in% group_levels)
}

#' data_fix_defaults
#' Restore key factor / date classes after importing a CSV.
#'
#' @param df A data.frame/tibble straight from read_csv()/read.csv()
#' @return Tibble with corrected column types
#' @importFrom dplyr mutate across any_of
#' @importFrom zoo as.yearqtr
data_fix_defaults <- function(df) {
  df <- tibble::as_tibble(df)

  # -- 1) Categorical columns -> factor -------------------------------------
  factor_cols <- grep(
    paste0(
      "^DOTW$|^Month$|^Financial_Quarter$|^Financial_Year$",
      "|^Cat_",
      collapse = ""
    ),
    names(df),
    value = TRUE
  )
  df <- dplyr::mutate(df, dplyr::across(dplyr::any_of(factor_cols), as.factor))

  # -- 2) Ordered factors ---------------------------------------------------
  if ("DOTW" %in% names(df)) {
    dow_levels <- c(
      "Sunday",
      "Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday",
      "Saturday"
    )
    df$DOTW <- factor(df$DOTW, levels = dow_levels, ordered = TRUE)
  }

  if ("Month" %in% names(df)) {
    month_levels <- month.abb[c(4:12, 1:3)]
    df$Month <- factor(df$Month, levels = month_levels, ordered = TRUE)
  }

  if ("Financial_Quarter" %in% names(df)) {
    fq_clean <- sub("^\\s*(Q[1-4]).*$", "\\1", df$Financial_Quarter)
    fq_order <- paste0("Q", 1:4)
    fq_levels <- fq_order[fq_order %in% fq_clean]
    df$Financial_Quarter <- factor(
      fq_clean,
      levels = fq_levels,
      ordered = TRUE
    )
  }

  # -- 3) Year_Quarter as yearqtr ------------------------------------------
  if ("Year_Quarter" %in% names(df)) {
    df$Year_Quarter <- zoo::as.yearqtr(df$Year_Quarter, format = "%Y Q%q")
  }

  df
}

#' Build a two-way frequency table
data_freq_table <- function(data, var1, var2) {
  table(data[[var1]], data[[var2]]) |>
    as.data.frame() |>
    dplyr::rename(
      !!var1 := Var1,
      !!var2 := Var2
    )
}

#' Build a counts-plus-row-percent table for one grouping variable
#' @importFrom dplyr count mutate_if relocate select
#' @importFrom tidyr complete spread
#' @importFrom rlang sym
data_prop_table_one_group <- function(data, var1, var2) {
  Time <- rlang::sym(var1)
  Variable <- rlang::sym(var2)

  counts_tbl <- data |>
    dplyr::count(!!Time, !!Variable) |>
    tidyr::complete(!!Time, !!Variable, fill = list(n = 0)) |>
    tidyr::spread(!!Variable, n, fill = 0)

  percent_tbl <- counts_tbl |>
    dplyr::select(-1) |>
    data.matrix() |>
    prop.table(margin = 1) |>
    as.data.frame() |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric),
        ~ dplyr::if_else(is.finite(.x), .x * 100, 0)
      )
    )

  names(percent_tbl) <- paste0(names(percent_tbl), ".Percent")

  cbind(percent_tbl, counts_tbl) |>
    dplyr::relocate(
      dplyr::where(is.factor),
      .before = dplyr::where(is.numeric)
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), \(x) round(x, 2))
    ) |>
    dplyr::select(!!Time, dplyr::everything())
}

#' Build a counts-plus-row-percent table for two grouping variables
#' @importFrom dplyr count relocate mutate across select everything
#' @importFrom tidyr complete spread
#' @importFrom rlang sym
data_prop_table_two_groups <- function(data, var1, var2, var3) {
  Time <- rlang::sym(var1)
  G1 <- rlang::sym(var2)
  G2 <- rlang::sym(var3)

  counts_tbl <- data |>
    dplyr::count(!!Time, !!G1, !!G2) |>
    tidyr::complete(!!Time, !!G1, !!G2, fill = list(n = 0)) |>
    tidyr::spread(!!G2, n, fill = 0)

  percent_tbl <- counts_tbl |>
    dplyr::select(-1, -2) |>
    data.matrix() |>
    prop.table(margin = 1) |>
    as.data.frame() |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), ~ .x * 100)
    )

  names(percent_tbl) <- paste0(names(percent_tbl), ".Percent")

  cbind(percent_tbl, counts_tbl) |>
    dplyr::relocate(
      dplyr::where(is.factor),
      .before = dplyr::where(is.numeric)
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), \(x) round(x, 2))
    ) |>
    dplyr::select(!!Time, dplyr::everything())
}

#' Build per-period summary statistics for one numeric variable
#' @importFrom dplyr summarise group_by mutate_if filter n
#' @importFrom rlang sym
data_summary_one_variable <- function(data, time, var) {
  Time <- rlang::sym(time)
  Var <- rlang::sym(var)

  out <-
    if (var == "Surplus") {
      data |>
        dplyr::group_by(!!Time) |>
        dplyr::summarise(
          Mean = mean(!!Var, na.rm = TRUE),
          Median = median(!!Var, na.rm = TRUE),
          Trimmed.Mean.20 = mean(!!Var, trim = 0.2, na.rm = TRUE),
          `n()` = dplyr::n(),
          .groups = "drop"
        )
    } else {
      {
        data |>
          dplyr::filter(!!Var != 0) |>
          dplyr::group_by(!!Time) |>
          dplyr::summarise(
            Mean = mean(!!Var, na.rm = TRUE),
            Median = median(!!Var, na.rm = TRUE),
            Trimmed.Mean.20 = mean(!!Var, trim = 0.2, na.rm = TRUE),
            `n()` = dplyr::n(),
            .groups = "drop"
          )
      } |>
        dplyr::mutate_if(is.numeric, round, 2)
    }

  out
}

#' Per-period, per-group summary statistics for one numeric variable
#' @importFrom dplyr summarise group_by filter mutate across n
#' @importFrom rlang sym
data_summary_grouped <- function(data, time, group_var, num_var) {
  Time <- rlang::sym(time)
  GroupVar <- rlang::sym(group_var)
  NumVar <- rlang::sym(num_var)

  df <- if (num_var == "Surplus") {
    data |>
      dplyr::group_by(!!Time, !!GroupVar)
  } else {
    data |>
      dplyr::filter(!!NumVar != 0) |>
      dplyr::group_by(!!Time, !!GroupVar)
  }

  df |>
    dplyr::summarise(
      Mean = mean(!!NumVar, na.rm = TRUE),
      Median = median(!!NumVar, na.rm = TRUE),
      Trimmed.Mean.20 = mean(!!NumVar, trim = 0.2, na.rm = TRUE),
      `n()` = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), \(x) round(x, 2)),
      !!GroupVar := droplevels(as.factor(!!GroupVar)) # Droplevels applied
    )
}

#' Summarise a single numeric variable over time (filtered & unfiltered)
#' @importFrom dplyr filter group_by summarise mutate bind_rows select
#' @importFrom rlang sym as_name
data_line_single_variable <- function(
  data_filtered,
  data_unfiltered,
  time_var,
  num_var
) {
  TimeSym <- rlang::sym(time_var)
  NumSym <- rlang::sym(num_var)

  conditional_filter <- function(df, var_sym) {
    if (rlang::as_name(var_sym) != "Num_Surplus") {
      df <- dplyr::filter(df, !!var_sym > 0)
    }
    df
  }

  filtered <- conditional_filter(data_filtered, NumSym)
  unfiltered <- conditional_filter(data_unfiltered, NumSym)

  make_stats <- function(df) {
    df |>
      dplyr::group_by(!!TimeSym) |>
      dplyr::summarise(
        Mean = mean(!!NumSym, na.rm = TRUE, trim = 0.2),
        Median = median(!!NumSym, na.rm = TRUE),
        `n()` = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 2)))
  }

  dat <- make_stats(filtered)
  dat2 <- make_stats(unfiltered)

  if (nrow(filtered) != nrow(unfiltered)) {
    dat <- dplyr::mutate(dat, Category = paste0(num_var, ": Filtered"))
    dat2 <- dplyr::mutate(dat2, Category = paste0(num_var, ": All"))
    dat <- dplyr::bind_rows(dat, dat2) |>
      dplyr::select(Category, dplyr::everything()) |>
      dplyr::mutate(Category = as.factor(Category)) # Explicit factor
  }

  dat
}

#' Summarise a numeric variable over time, split by a factor, with an "All" line
#' @importFrom dplyr filter group_by summarise mutate across bind_rows n
#' @importFrom rlang sym as_name
data_grouped_lines <- function(
  data_filtered,
  data_unfiltered,
  time_var,
  group_var,
  num_var
) {
  TimeSym <- rlang::sym(time_var)
  GroupSym <- rlang::sym(group_var)
  NumSym <- rlang::sym(num_var)

  conditional_filter <- function(df, var_sym) {
    if (rlang::as_name(var_sym) != "Surplus") {
      df <- dplyr::filter(df, !!var_sym > 0)
    }
    df
  }

  dat <- data_filtered |>
    conditional_filter(NumSym) |>
    dplyr::group_by(!!TimeSym, !!GroupSym) |>
    dplyr::summarise(
      Mean = mean(!!NumSym, na.rm = TRUE, trim = 0.2),
      Median = median(!!NumSym),
      `n()` = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(
      !is.na(!!GroupSym),
      !grepl("NA|Other", as.character(!!GroupSym), ignore.case = TRUE)
    )

  overall <- data_unfiltered |>
    conditional_filter(NumSym) |>
    dplyr::group_by(!!TimeSym) |>
    dplyr::summarise(
      Mean = mean(!!NumSym, na.rm = TRUE, trim = 0.2),
      Median = median(!!NumSym),
      `n()` = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(!!group_var := "All")

  dplyr::bind_rows(dat, overall) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), \(x) round(x, 2)),
      !!group_var := droplevels(as.factor(!!rlang::sym(group_var))) # Droplevels applied
    )
}

#' Summarise one numeric variable by group for the most-recent period
#' @importFrom dplyr filter group_by summarise mutate across n
#' @importFrom rlang sym as_name
data_simple_bar <- function(data, time_var, group_var, num_var) {
  TimeSym <- rlang::sym(time_var)
  GroupSym <- rlang::sym(group_var)
  NumSym <- rlang::sym(num_var)

  conditional_filter <- function(df, var_sym) {
    if (rlang::as_name(var_sym) != "Surplus") {
      df <- dplyr::filter(df, !!var_sym > 0)
    }
    df
  }

  data |>
    conditional_filter(NumSym) |>
    dplyr::group_by(!!TimeSym, !!GroupSym) |>
    dplyr::summarise(
      Mean = mean(!!NumSym, na.rm = TRUE),
      Median = median(!!NumSym, na.rm = TRUE),
      `n()` = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(
      !!TimeSym == levels(!!TimeSym)[length(levels(!!TimeSym))]
    ) |>
    dplyr::filter(
      !is.na(!!GroupSym),
      !grepl("NA|Other", as.character(!!GroupSym), ignore.case = TRUE)
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), \(x) round(x, 2)),
      !!GroupSym := droplevels(as.factor(!!GroupSym)) # Droplevels applied
    )
}

#' Summarise a numeric variable by two grouping factors (latest period only)
#' @importFrom dplyr filter group_by summarise mutate across n
#' @importFrom rlang sym as_name
data_stacked_bar <- function(data, time_var, group1_var, group2_var, num_var) {
  TimeSym <- rlang::sym(time_var)
  G1Sym <- rlang::sym(group1_var)
  G2Sym <- rlang::sym(group2_var)
  NumSym <- rlang::sym(num_var)

  conditional_filter <- function(df, var_sym) {
    if (rlang::as_name(var_sym) != "Surplus") {
      df <- dplyr::filter(df, !!var_sym > 0)
    }
    df
  }

  latest_level <- levels(data[[time_var]])[length(levels(data[[time_var]]))]

  data |>
    conditional_filter(NumSym) |>
    dplyr::group_by(!!TimeSym, !!G1Sym, !!G2Sym) |>
    dplyr::summarise(
      Mean = mean(!!NumSym, na.rm = TRUE),
      Median = median(!!NumSym),
      `n()` = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(!!TimeSym == latest_level) |>
    dplyr::filter(
      !is.na(!!G1Sym),
      !is.na(!!G2Sym),
      !grepl("Other|NA", as.character(!!G1Sym), ignore.case = TRUE),
      !grepl("Other|NA", as.character(!!G2Sym), ignore.case = TRUE)
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), \(x) round(x, 2)),
      !!G1Sym := droplevels(as.factor(!!G1Sym)), # Droplevels applied
      !!G2Sym := droplevels(as.factor(!!G2Sym)) # Droplevels applied
    )
}

#' Summarise a numeric variable by two grouping factors for the most-recent period
#' @importFrom dplyr filter group_by summarise mutate across n
#' @importFrom rlang sym as_name
data_grouped_dodge_bar <- function(
  data,
  time_var,
  group1_var,
  group2_var,
  num_var
) {
  TimeSym <- rlang::sym(time_var)
  G1Sym <- rlang::sym(group1_var)
  G2Sym <- rlang::sym(group2_var)
  NumSym <- rlang::sym(num_var)

  conditional_filter <- function(df, var_sym) {
    if (rlang::as_name(var_sym) != "Surplus") {
      df <- dplyr::filter(df, !!var_sym > 0)
    }
    df
  }

  latest_level <- levels(data[[time_var]])[length(levels(data[[time_var]]))]

  data |>
    conditional_filter(NumSym) |>
    dplyr::group_by(!!TimeSym, !!G1Sym, !!G2Sym) |>
    dplyr::summarise(
      Mean = mean(!!NumSym, na.rm = TRUE),
      Median = median(!!NumSym),
      `n()` = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(!!TimeSym == latest_level) |>
    dplyr::filter(
      !is.na(!!G1Sym),
      !is.na(!!G2Sym),
      !grepl("Other|NA", as.character(!!G1Sym), ignore.case = TRUE),
      !grepl("Other|NA", as.character(!!G2Sym), ignore.case = TRUE)
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), \(x) round(x, 2)),
      !!G1Sym := droplevels(as.factor(!!G1Sym)), # Droplevels applied
      !!G2Sym := droplevels(as.factor(!!G2Sym)) # Droplevels applied
    )
}

#' Summarise a numeric variable over time by a grouping factor (area chart data)
#' @importFrom dplyr filter group_by summarise mutate across n
#' @importFrom rlang sym
data_stacked_area <- function(data, time_var, group_var, num_var) {
  TimeSym <- rlang::sym(time_var)
  GroupSym <- rlang::sym(group_var)
  NumVar <- num_var

  conditional_filter <- function(df, var_name) {
    if (var_name != "Surplus") {
      df <- dplyr::filter(df, .data[[var_name]] > 0)
    }
    df
  }

  data |>
    conditional_filter(NumVar) |>
    dplyr::group_by(!!TimeSym, !!GroupSym) |>
    dplyr::summarise(
      Mean = mean(.data[[NumVar]], na.rm = TRUE),
      Median = median(.data[[NumVar]]),
      `n()` = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(
      !grepl("Other|NA", as.character(!!GroupSym), ignore.case = TRUE),
      !is.na(!!GroupSym)
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), \(x) round(x, 2)),
      !!GroupSym := droplevels(as.factor(!!GroupSym)) # Droplevels applied
    )
}

#' Build proportional-area data over time for a factor
#' @importFrom dplyr filter group_by summarise ungroup mutate across
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom rlang sym .data
data_proportional_area <- function(data, time_var, group_var) {
  df <- data

  if (time_var == "Year") {
    df <- df |> dplyr::mutate(Year = as.integer(Year) + 2018)
  }

  TimeSym <- rlang::sym(time_var)
  GroupSym <- rlang::sym(group_var)

  counts <- df |>
    dplyr::filter(
      !grepl("Other|NA", as.character(!!GroupSym), ignore.case = TRUE),
      !is.na(!!GroupSym)
    ) |>
    dplyr::group_by(!!TimeSym, !!GroupSym) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = !!GroupSym,
      values_from = n,
      values_fill = 0
    ) |>
    dplyr::ungroup()

  time_vec <- counts[[time_var]]

  prop_tbl <- counts |>
    dplyr::select(-!!TimeSym) |>
    as.matrix() |>
    prop.table(margin = 1) |>
    as.data.frame() |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), ~ .x * 100),
      Time = time_vec,
      n = rowSums(counts[-1])
    ) |>
    tidyr::pivot_longer(
      cols = -c(Time, n),
      names_to = "Condition",
      values_to = "Percentage"
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), \(x) round(x, 2)),
      Condition = droplevels(as.factor(Condition))
    ) |>
    dplyr::select(Time, Condition, Percentage, n)

  prop_tbl
}

#' Prepare sampled scatterplot data with grouping
#' @importFrom dplyr filter select sample_n
#' @importFrom rlang sym
data_scatter_grouped <- function(data, group, x_var, y_var) {
  G <- rlang::sym(group)
  X <- rlang::sym(x_var)
  Y <- rlang::sym(y_var)

  df <- data |>
    dplyr::filter(
      !is.na(!!G),
      !is.na(!!X),
      !is.na(!!Y)
    ) |>
    dplyr::select(!!G, !!X, !!Y)

  if (nrow(df) > 1000) {
    df <- dplyr::sample_n(df, size = 1000)
  }

  df |> dplyr::mutate(!!G := droplevels(as.factor(!!G))) # Droplevels applied
}

#' Prepare trimmed density-plot data for a numeric variable by group
#' @importFrom dplyr filter select mutate summarise
#' @importFrom rlang sym as_string
#' @importFrom stats quantile
data_density <- function(data, group_var, num_var, fy_choice) {
  Gsym <- rlang::sym(group_var)
  Nsym <- rlang::sym(num_var)

  df <- data |>
    dplyr::filter(Financial_Year == fy_choice) |>
    dplyr::select(!!Gsym, !!Nsym)

  if (num_var != "Surplus") {
    df <- df |> dplyr::filter(!!Nsym > 0)
  }

  df <- df |> dplyr::filter(!is.na(!!Gsym), !is.na(!!Nsym))

  vals <- df[[rlang::as_string(Nsym)]]
  lower_q <- stats::quantile(vals, 0.05, na.rm = TRUE)
  upper_q <- stats::quantile(vals, 0.95, na.rm = TRUE)

  df |>
    dplyr::filter(!!Nsym >= lower_q, !!Nsym <= upper_q) |>
    dplyr::mutate(!!Gsym := droplevels(as.factor(!!Gsym))) # Droplevels applied
}
