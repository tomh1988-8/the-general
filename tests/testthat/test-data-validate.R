# tests/testthat/test-data-validate.R

testthat::test_that("data_validate_dataset returns expected top-level objects", {
  standardized_data <- tibble::tibble(
    order_date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    region = c("North", "South", "North"),
    sales = c(10, 20, 30),
    DOTW = c("Monday", "Tuesday", "Wednesday")
  )

  transformation_report <- list(
    columns_changed = tibble::tribble(
      ~original_name , ~cleaned_name , ~resolved_role , ~input_type , ~output_type , ~name_changed ,
      "order_date"   , "order_date"  , "date"         , "character" , "Date"       , FALSE         ,
      "region"       , "region"      , "dimension"    , "character" , "character"  , FALSE         ,
      "sales"        , "sales"       , "measure"      , "character" , "numeric"    , FALSE
    ),
    rows_dropped = 0L,
    parse_failures = tibble::tibble(),
    helper_fields_created = c("DOTW"),
    columns_excluded = tibble::tibble(),
    reference_date_column = "order_date"
  )

  result <- data_validate_dataset(
    standardized_data = standardized_data,
    transformation_report = transformation_report
  )

  testthat::expect_named(
    result,
    c(
      "validated_columns",
      "validation_checks",
      "variable_sets",
      "validation_status"
    )
  )

  testthat::expect_s3_class(result$validated_columns, "tbl_df")
  testthat::expect_s3_class(result$validation_checks, "tbl_df")
  testthat::expect_type(result$variable_sets, "list")
  testthat::expect_equal(result$validation_status, "valid")
})

testthat::test_that("data_validate_columns preserves roles and marks derived helpers", {
  standardized_data <- tibble::tibble(
    order_date = as.Date(c("2024-01-01", "2024-01-02")),
    region = c("North", "South"),
    sales = c(10, 20),
    DOTW = c("Monday", "Tuesday")
  )

  transformation_report <- list(
    columns_changed = tibble::tribble(
      ~original_name , ~cleaned_name , ~resolved_role , ~input_type , ~output_type , ~name_changed ,
      "order_date"   , "order_date"  , "date"         , "character" , "Date"       , FALSE         ,
      "region"       , "region"      , "dimension"    , "character" , "character"  , FALSE         ,
      "sales"        , "sales"       , "measure"      , "character" , "numeric"    , FALSE
    ),
    rows_dropped = 0L,
    parse_failures = tibble::tibble(),
    helper_fields_created = c("DOTW"),
    columns_excluded = tibble::tibble(),
    reference_date_column = "order_date"
  )

  validated <- data_validate_columns(
    standardized_data = standardized_data,
    transformation_report = transformation_report
  )

  dotw_row <- dplyr::filter(validated, column == "DOTW")
  sales_row <- dplyr::filter(validated, column == "sales")
  date_row <- dplyr::filter(validated, column == "order_date")

  testthat::expect_true(dotw_row$derived[[1]])
  testthat::expect_equal(dotw_row$role[[1]], "dimension")

  testthat::expect_false(sales_row$derived[[1]])
  testthat::expect_true(sales_row$safe_for_summary[[1]])

  testthat::expect_true(date_row$usable_as_date[[1]])
})

testthat::test_that("measure columns are safe for summary only when numeric with finite values", {
  testthat::expect_true(
    data_validate_safe_for_summary(c(1, 2, 3), role = "measure")
  )

  testthat::expect_false(
    data_validate_safe_for_summary(c(Inf, NA_real_), role = "measure")
  )

  testthat::expect_false(
    data_validate_safe_for_summary(c("1", "2"), role = "measure")
  )

  testthat::expect_false(
    data_validate_safe_for_summary(c(1, 2, 3), role = "dimension")
  )
})

testthat::test_that("grouping usability excludes identifiers and high-cardinality columns", {
  testthat::expect_true(
    data_validate_usable_for_grouping(
      x = c("North", "South", "North"),
      role = "dimension",
      derived = FALSE,
      distinct_ratio = 2 / 3
    )
  )

  testthat::expect_false(
    data_validate_usable_for_grouping(
      x = c("1001", "1002", "1003"),
      role = "identifier",
      derived = FALSE,
      distinct_ratio = 1
    )
  )

  testthat::expect_false(
    data_validate_usable_for_grouping(
      x = c("a", "b", "c", "d"),
      role = "dimension",
      derived = FALSE,
      distinct_ratio = 1
    )
  )

  testthat::expect_true(
    data_validate_usable_for_grouping(
      x = c("Q1", "Q1", "Q2", "Q2"),
      role = "dimension",
      derived = TRUE,
      distinct_ratio = 0.5
    )
  )
})

testthat::test_that("validation checks reflect presence of dimensions, measures, dates, and grouping vars", {
  validated_columns <- tibble::tribble(
    ~column      , ~role       , ~output_type , ~derived , ~non_missing_count , ~distinct_count , ~distinct_ratio , ~missingness , ~usable_as_date , ~safe_for_summary , ~usable_for_grouping ,
    "order_date" , "date"      , "Date"       , FALSE    , 3L                 , 3L              ,               1 ,            0 , TRUE            , FALSE             , FALSE                ,
    "region"     , "dimension" , "character"  , FALSE    , 3L                 , 2L              , 2 / 3           ,            0 , FALSE           , FALSE             , TRUE                 ,
    "sales"      , "measure"   , "numeric"    , FALSE    , 3L                 , 3L              ,               1 ,            0 , FALSE           , TRUE              , FALSE                ,
    "DOTW"       , "dimension" , "character"  , TRUE     , 3L                 , 2L              , 2 / 3           ,            0 , FALSE           , FALSE             , TRUE
  )

  checks <- data_validate_checks(
    standardized_data = tibble::tibble(
      order_date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
      region = c("North", "South", "North"),
      sales = c(10, 20, 30),
      DOTW = c("Monday", "Tuesday", "Monday")
    ),
    validated_columns = validated_columns
  )

  check_map <- stats::setNames(checks$passed, checks$check)

  testthat::expect_true(check_map[["has_rows"]])
  testthat::expect_true(check_map[["has_columns"]])
  testthat::expect_true(check_map[["has_validated_columns"]])
  testthat::expect_true(check_map[["has_valid_dimension"]])
  testthat::expect_true(check_map[["has_valid_measure"]])
  testthat::expect_true(check_map[["has_valid_date"]])
  testthat::expect_true(check_map[["has_grouping_columns"]])
  testthat::expect_true(check_map[["has_summary_measures"]])
})

testthat::test_that("variable sets return validated inventories for downstream use", {
  validated_columns <- tibble::tribble(
    ~column       , ~role        , ~output_type , ~derived , ~non_missing_count , ~distinct_count , ~distinct_ratio , ~missingness , ~usable_as_date , ~safe_for_summary , ~usable_for_grouping ,
    "order_date"  , "date"       , "Date"       , FALSE    , 3L                 , 3L              ,               1 ,            0 , TRUE            , FALSE             , FALSE                ,
    "region"      , "dimension"  , "character"  , FALSE    , 3L                 , 2L              , 2 / 3           ,            0 , FALSE           , FALSE             , TRUE                 ,
    "sales"       , "measure"    , "numeric"    , FALSE    , 3L                 , 3L              ,               1 ,            0 , FALSE           , TRUE              , FALSE                ,
    "customer_id" , "identifier" , "character"  , FALSE    , 3L                 , 3L              ,               1 ,            0 , FALSE           , FALSE             , FALSE                ,
    "comments"    , "free text"  , "character"  , FALSE    , 3L                 , 3L              ,               1 ,            0 , FALSE           , FALSE             , FALSE                ,
    "DOTW"        , "dimension"  , "character"  , TRUE     , 3L                 , 2L              , 2 / 3           ,            0 , FALSE           , FALSE             , TRUE
  )

  variable_sets <- data_validate_variable_sets(validated_columns)

  testthat::expect_equal(variable_sets$date_vars, "order_date")
  testthat::expect_equal(variable_sets$measure_vars, "sales")
  testthat::expect_equal(variable_sets$identifier_vars, "customer_id")
  testthat::expect_equal(variable_sets$free_text_vars, "comments")
  testthat::expect_equal(variable_sets$grouping_vars, c("region", "DOTW"))
  testthat::expect_equal(variable_sets$dimension_vars, c("region", "DOTW"))
})

testthat::test_that("validation status is fatal when no rows are available", {
  standardized_data <- tibble::tibble(
    region = character(),
    sales = numeric()
  )

  transformation_report <- list(
    columns_changed = tibble::tribble(
      ~original_name , ~cleaned_name , ~resolved_role , ~input_type , ~output_type , ~name_changed ,
      "region"       , "region"      , "dimension"    , "character" , "character"  , FALSE         ,
      "sales"        , "sales"       , "measure"      , "character" , "numeric"    , FALSE
    ),
    rows_dropped = 0L,
    parse_failures = tibble::tibble(),
    helper_fields_created = character(),
    columns_excluded = tibble::tibble(),
    reference_date_column = NULL
  )

  result <- data_validate_dataset(
    standardized_data = standardized_data,
    transformation_report = transformation_report
  )

  testthat::expect_equal(result$validation_status, "fatal")
})

testthat::test_that("validation status is fatal when no columns survive normalization", {
  standardized_data <- tibble::tibble()

  transformation_report <- list(
    columns_changed = tibble::tibble(
      original_name = character(),
      cleaned_name = character(),
      resolved_role = character(),
      input_type = character(),
      output_type = character(),
      name_changed = logical()
    ),
    rows_dropped = 0L,
    parse_failures = tibble::tibble(),
    helper_fields_created = character(),
    columns_excluded = tibble::tibble(),
    reference_date_column = NULL
  )

  result <- data_validate_dataset(
    standardized_data = standardized_data,
    transformation_report = transformation_report
  )

  testthat::expect_equal(result$validation_status, "fatal")
})

testthat::test_that("dimension-only datasets remain valid but without safe measures", {
  standardized_data <- tibble::tibble(
    region = c("North", "South", "North"),
    category = c("A", "B", "A")
  )

  transformation_report <- list(
    columns_changed = tibble::tribble(
      ~original_name , ~cleaned_name , ~resolved_role , ~input_type , ~output_type , ~name_changed ,
      "region"       , "region"      , "dimension"    , "character" , "character"  , FALSE         ,
      "category"     , "category"    , "dimension"    , "character" , "character"  , FALSE
    ),
    rows_dropped = 0L,
    parse_failures = tibble::tibble(),
    helper_fields_created = character(),
    columns_excluded = tibble::tibble(),
    reference_date_column = NULL
  )

  result <- data_validate_dataset(
    standardized_data = standardized_data,
    transformation_report = transformation_report
  )

  check_map <- stats::setNames(
    result$validation_checks$passed,
    result$validation_checks$check
  )

  testthat::expect_equal(result$validation_status, "valid")
  testthat::expect_true(check_map[["has_valid_dimension"]])
  testthat::expect_false(check_map[["has_valid_measure"]])
  testthat::expect_false(check_map[["has_valid_date"]])
})

testthat::test_that("date-only datasets remain valid but without grouping or measures when unsuitable", {
  standardized_data <- tibble::tibble(
    event_date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))
  )

  transformation_report <- list(
    columns_changed = tibble::tribble(
      ~original_name , ~cleaned_name , ~resolved_role , ~input_type , ~output_type , ~name_changed ,
      "event_date"   , "event_date"  , "date"         , "character" , "Date"       , FALSE
    ),
    rows_dropped = 0L,
    parse_failures = tibble::tibble(),
    helper_fields_created = character(),
    columns_excluded = tibble::tibble(),
    reference_date_column = "event_date"
  )

  result <- data_validate_dataset(
    standardized_data = standardized_data,
    transformation_report = transformation_report
  )

  check_map <- stats::setNames(
    result$validation_checks$passed,
    result$validation_checks$check
  )

  testthat::expect_equal(result$validation_status, "valid")
  testthat::expect_true(check_map[["has_valid_date"]])
  testthat::expect_false(check_map[["has_valid_measure"]])
  testthat::expect_false(check_map[["has_grouping_columns"]])
})
