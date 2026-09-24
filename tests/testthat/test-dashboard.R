test_that("dashboard pin parameter labels are readable", {
  params <- list(
    TimeVariable = "Month",
    NumericVariable = "Num_Score",
    GroupVar = c("Cat_Group", "Cat_Status")
  )

  label <- dashboard_pin_params_label(params)

  expect_equal(
    label,
    "TimeVariable: Month, NumericVariable: Num_Score, GroupVar: Cat_Group, Cat_Status"
  )
})

test_that("dashboard pins table handles empty and populated pin lists", {
  empty <- dashboard_pin_table_data(list())

  expect_equal(names(empty), "Message")
  expect_equal(empty$Message, "No views pinned yet.")

  pins <- list(
    list(
      type = "bar_simple",
      title = "Simple bar",
      params = list(
        TimeVariable3 = "Month",
        GroupVar = "Cat_Group",
        NumVar = "Num_Value"
      ),
      caption = as.Date("2026-06-06")
    )
  )

  populated <- dashboard_pin_table_data(pins, ns = identity)

  expect_equal(
    names(populated),
    c("Type", "Date", "Remove")
  )
  expect_equal(populated$Type, "bar_simple")
  expect_equal(populated$Date, "2026-06-06")
  expect_match(populated$Remove, "remove_pin")
  expect_match(populated$Remove, "trash")
})

test_that("dashboard area percentage pins use modern reactive data", {
  testthat::local_mocked_bindings(
    data_proportional_area = function(data, time_var, group_var) {
      tibble::tibble(
        source = data$source,
        time_var = time_var,
        group_var = group_var
      )
    }
  )

  pin <- list(
    type = "area_percent",
    params = list(
      TimeVariable7 = "Year",
      GroupingVar = "Cat_Group"
    )
  )

  rv_data <- tibble::tibble(source = "modern-rv-data")

  out <- dashboard_pin_data(
    pin = pin,
    rv_data = rv_data,
    rv_unfiltered = NULL
  )

  expect_equal(out$source, "modern-rv-data")
  expect_equal(out$time_var, "Year")
  expect_equal(out$group_var, "Cat_Group")
})

test_that("dashboard area percentage pins default to Year_Quarter when not Year", {
  expect_equal(
    dashboard_pin_area_percent_time_var("Financial_Quarter"),
    "Year_Quarter"
  )

  expect_equal(
    dashboard_pin_area_percent_time_var("Year"),
    "Year"
  )
})

test_that("dashboard remove input removes the selected pin", {
  rv <- shiny::reactiveValues(
    data = tibble::tibble(x = 1),
    unfiltered = tibble::tibble(x = 1)
  )

  pins <- shiny::reactiveVal(list(
    list(
      type = "bar_simple",
      title = "First",
      params = list()
    ),
    list(
      type = "bar_simple",
      title = "Second",
      params = list()
    )
  ))

  shiny::testServer(
    mod_my_dashboard_server,
    args = list(
      rv = rv,
      pins = pins
    ),
    {
      session$setInputs(remove_pin = 1)

      expect_equal(length(pins()), 1L)
      expect_equal(pins()[[1]]$title, "Second")
    }
  )
})

test_that("pin module captures dashboard pin metadata", {
  pins <- shiny::reactiveVal(list())

  shiny::testServer(
    pinModuleServer,
    args = list(
      render_fn = function(output, session, out_id) {
        output[[out_id]] <- shiny::renderText("ok")
      },
      type = "unit_pin",
      params_r = shiny::reactive(list(a = 1)),
      title_r = shiny::reactive("Unit pin"),
      pins = pins
    ),
    {
      session$setInputs(pin = 1)

      expect_equal(length(pins()), 1L)
      expect_equal(pins()[[1]]$type, "unit_pin")
      expect_equal(pins()[[1]]$params, list(a = 1))
      expect_equal(pins()[[1]]$title, "Unit pin")
    }
  )
})

testthat::test_that("dashboard R script export filenames are stable", {
  pin <- list(
    type = "bar_simple",
    title = "Simple bar",
    params = list(
      TimeVariable3 = "Month",
      GroupVar = "Cat_Group",
      NumVar = "Num_Value"
    )
  )

  testthat::expect_equal(
    dashboard_r_scripts_filename(pin, 1),
    "01_bar_simple.R"
  )
})

testthat::test_that("dashboard R scripts are standalone and avoid shiny globals", {
  pin <- list(
    type = "area_percent",
    title = "Area percent",
    params = list(
      TimeVariable7 = "Year",
      GroupingVar = "Cat_Group"
    )
  )

  script <- paste(
    dashboard_r_scripts_script_text(pin, 1),
    collapse = "\n"
  )

  testthat::expect_match(script, "readr::read_csv")
  testthat::expect_false(grepl("library\\(thegeneral\\)", script))
  testthat::expect_false(grepl("shiny_df", script, fixed = TRUE))
  testthat::expect_false(grepl("shiny::", script, fixed = TRUE))
})

testthat::test_that("dashboard R script zip writer creates one script per pin", {
  pin_list <- list(
    list(
      type = "bar_simple",
      title = "Simple bar",
      params = list(
        TimeVariable3 = "Month",
        GroupVar = "Cat_Group",
        NumVar = "Num_Value"
      )
    ),
    list(
      type = "scatter",
      title = "Scatter",
      params = list(
        GroupVar = "Cat_Group",
        X = "Num_X",
        Y = "Num_Y"
      )
    )
  )

  out_file <- tempfile(fileext = ".zip")

  dashboard_r_scripts_write_zip(
    pin_list = pin_list,
    file = out_file
  )

  testthat::expect_true(file.exists(out_file))
  testthat::expect_gt(file.info(out_file)$size, 0)
})
