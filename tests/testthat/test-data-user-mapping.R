testthat::test_that("canonical upload ingestion keeps only selected columns", {
  raw_data <- tibble::tibble(
    Event_Date = c("2024-01-01", "2024-01-02"),
    Region = c("North", "South"),
    Cases = c("10", "20"),
    Notes = c("keep out", "also keep out")
  )

  contract <- data_ingest_uploaded_sheet(
    data = raw_data,
    source_name = "unit-test",
    date_column = "Event_Date",
    numeric_columns = "Cases",
    categorical_columns = "Region"
  )

  expected_base_columns <- c(
    "Date_event_date",
    "Num_cases",
    "Cat_region"
  )

  expected_helper_columns <- c(
    "DOTW",
    "Month",
    "Financial_Quarter",
    "Year",
    "Financial_Year",
    "Year_Quarter"
  )

  testthat::expect_setequal(
    names(contract$standardized_data),
    c(expected_base_columns, expected_helper_columns)
  )

  testthat::expect_false("Cat_notes" %in% names(contract$standardized_data))
  testthat::expect_false(
    "Notes" %in% contract$transformation_report$columns_changed$original_name
  )
  testthat::expect_equal(
    nrow(contract$transformation_report$columns_excluded),
    0
  )
  testthat::expect_equal(contract$status, "ready")
})

testthat::test_that("extra migration arguments do not alter selected mapping", {
  raw_data <- tibble::tibble(
    Event_Date = c("2024-01-01", "2024-01-02"),
    Region = c("North", "South"),
    Cases = c("10", "20")
  )

  contract <- data_ingest_uploaded_sheet(
    data = raw_data,
    source_name = "unit-test",
    date_column = "Event_Date",
    numeric_columns = "Cases",
    categorical_columns = "Region",
    excluded_columns = "Region"
  )

  testthat::expect_true("Cat_region" %in% names(contract$standardized_data))
  testthat::expect_equal(
    nrow(contract$transformation_report$columns_excluded),
    0
  )
})

testthat::test_that("resolver counts matched rows rather than tibble columns", {
  raw_data <- tibble::tibble(
    Event_Date = c("2024-01-01", "2024-01-02"),
    `Cases Total` = c("10", "20"),
    Region = c("North", "South")
  )

  mapping <- data_user_mapping_resolve_columns(
    data = raw_data,
    date_column = "Event_Date",
    numeric_columns = "cases_total",
    categorical_columns = "Region"
  )

  testthat::expect_equal(nrow(mapping$selected), 3L)
  testthat::expect_setequal(
    mapping$selected$output_name,
    c("Date_event_date", "Num_cases_total", "Cat_region")
  )
})

testthat::test_that("empty schema review has no exclude fields", {
  review <- global_filter_empty_schema_review()

  testthat::expect_false(any(grepl("exclude", names(review))))
  testthat::expect_false("exclude_candidates" %in% names(review))
  testthat::expect_false("suggested_excluded_columns" %in% names(review))
})

testthat::test_that("canonical upload ingestion creates dashboard date helpers", {
  raw_data <- tibble::tibble(
    Event_Date = as.Date(c(
      "2022-04-01",
      "2022-07-01",
      "2022-10-01",
      "2023-01-01"
    )),
    Region = c("North", "South", "Midlands", "London"),
    Cases = c(10, 20, 30, 40)
  )

  contract <- data_ingest_uploaded_sheet(
    data = raw_data,
    source_name = "unit-test",
    date_column = "Event_Date",
    numeric_columns = "Cases",
    categorical_columns = "Region"
  )

  data <- contract$standardized_data

  testthat::expect_s3_class(data$DOTW, "ordered")
  testthat::expect_s3_class(data$Month, "ordered")
  testthat::expect_s3_class(data$Financial_Quarter, "ordered")
  testthat::expect_s3_class(data$Financial_Year, "factor")
  testthat::expect_s3_class(data$Year_Quarter, "yearqtr")

  testthat::expect_equal(levels(data$Month), month.abb[c(4:12, 1:3)])
  testthat::expect_equal(
    levels(data$Financial_Quarter),
    c("Q1 (Apr-Jun)", "Q2 (Jul-Sep)", "Q3 (Oct-Dec)", "Q4 (Jan-Mar)")
  )

  testthat::expect_equal(
    as.character(data$Financial_Quarter),
    c("Q1 (Apr-Jun)", "Q2 (Jul-Sep)", "Q3 (Oct-Dec)", "Q4 (Jan-Mar)")
  )

  testthat::expect_equal(
    as.character(data$Financial_Year),
    rep("2022-23", 4)
  )

  testthat::expect_equal(
    as.character(data$Year_Quarter),
    c("2022 Q2", "2022 Q3", "2022 Q4", "2023 Q1")
  )
})

testthat::test_that("mapped financial years are usable by density filtering", {
  raw_data <- tibble::tibble(
    Event_Date = c(
      "2024-01-03",
      "2024-01-10",
      "2024-02-07",
      "2024-02-14",
      "2024-03-06",
      "2024-03-13",
      "2024-04-03",
      "2024-04-10",
      "2024-05-01",
      "2024-05-08"
    ),
    Region = c(
      "North",
      "South",
      "North",
      "South",
      "North",
      "South",
      "North",
      "South",
      "North",
      "South"
    ),
    Score = c(78, 72, 81, 69, 70, 82, 83, 74, 68, 80)
  )

  contract <- data_ingest_uploaded_sheet(
    data = raw_data,
    source_name = "density-test",
    date_column = "Event_Date",
    numeric_columns = "Score",
    categorical_columns = "Region"
  )

  year_choices <- contract$standardized_data$Financial_Year |>
    as.character() |>
    unique() |>
    sort()

  testthat::expect_equal(year_choices, c("2023-24", "2024-25"))

  density_data <- data_density(
    data = contract$standardized_data,
    group_var = "Cat_region",
    num_var = "Num_score",
    fy_choice = "2023-24"
  )

  testthat::expect_gt(nrow(density_data), 0)
  testthat::expect_true(all(
    as.character(density_data$Cat_region) %in% c("North", "South")
  ))
})

testthat::test_that("canonical upload ingestion applies the proven messy-sheet cleaning rules", {
  raw_data <- tibble::tibble(
    Date___ = c("2023-04-05", "05/04/2023", "13-08-2023", ""),
    score = c("1,234", "1.234,5", "-999", ""),
    pct = c("85 %", "0.85", "", ""),
    flag = c("Yes", "no", "True", ""),
    empty = c("", "NA", "missing", ""),
    label = c("alpha", "ALPHA", "beta", ""),
    not_selected = c("keep out", "still out", "out", "")
  )

  contract <- data_ingest_uploaded_sheet(
    data = raw_data,
    source_name = "messy-test",
    date_column = "Date___",
    numeric_columns = c("score", "pct"),
    categorical_columns = c("flag", "empty", "label")
  )

  clean <- contract$standardized_data

  testthat::expect_equal(nrow(clean), nrow(raw_data) - 1L)
  testthat::expect_false("Cat_not_selected" %in% names(clean))

  testthat::expect_equal(clean$Num_score[1], 1234)
  testthat::expect_equal(clean$Num_score[2], 1234.5, tolerance = 1e-2)
  testthat::expect_true(is.na(clean$Num_score[3]))

  testthat::expect_equal(clean$Num_pct[1], 0.85)
  testthat::expect_equal(clean$Num_pct[2], 0.85)

  testthat::expect_s3_class(clean$Cat_flag, "factor")
  testthat::expect_equal(levels(clean$Cat_flag), c("No", "Yes"))
  testthat::expect_equal(as.character(clean$Cat_flag), c("Yes", "No", "Yes"))

  testthat::expect_s3_class(clean$Cat_empty, "factor")
  testthat::expect_true(all(is.na(clean$Cat_empty)))

  testthat::expect_s3_class(clean$Cat_label, "factor")
  testthat::expect_equal(levels(clean$Cat_label), c("Alpha", "Beta"))
  testthat::expect_equal(
    as.character(clean$Cat_label),
    c("Alpha", "Alpha", "Beta")
  )

  testthat::expect_true(all(
    c(
      "DOTW",
      "Month",
      "Financial_Quarter",
      "Year",
      "Financial_Year",
      "Year_Quarter"
    ) %in%
      names(clean)
  ))

  testthat::expect_equal(contract$transformation_report$rows_dropped, 1L)
  testthat::expect_equal(contract$status, "ready")
})

testthat::test_that("canonical upload ingestion handles multiple numeric formats", {
  raw_data <- tibble::tibble(
    dt = c("2025-01-01", "2025-01-02"),
    metric_a = c("1,234.56", "2,345.67"),
    metric_b = c("3.456,78", "4.567,89"),
    group = c("north", "SOUTH")
  )

  contract <- data_ingest_uploaded_sheet(
    data = raw_data,
    source_name = "numeric-test",
    date_column = "dt",
    numeric_columns = c("metric_a", "metric_b"),
    categorical_columns = "group"
  )

  clean <- contract$standardized_data

  testthat::expect_true(all(
    c("Num_metric_a", "Num_metric_b") %in% names(clean)
  ))
  testthat::expect_type(clean$Num_metric_a, "double")
  testthat::expect_type(clean$Num_metric_b, "double")
  testthat::expect_equal(clean$Num_metric_a[1], 1234.56, tolerance = 1e-2)
  testthat::expect_equal(clean$Num_metric_b[1], 3456.78, tolerance = 1e-2)
  testthat::expect_equal(as.character(clean$Cat_group), c("North", "South"))
})

testthat::test_that("old upload-cleaning entry points have been removed", {
  testthat::expect_false(
    exists(
      "data_fix_uploads",
      envir = asNamespace("thegeneral"),
      inherits = FALSE
    )
  )

  testthat::expect_false(
    exists(
      "data_user_mapping_build_contract",
      envir = asNamespace("thegeneral"),
      inherits = FALSE
    )
  )
})
