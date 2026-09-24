test_that("all nine exported scripts run independently and reproduce dashboard data", {
  output <- getOption("the_general.export_eval_dir", withr::local_tempdir(pattern = "rx_"))
  results <- r_script_evaluate(output)
  for (i in seq_len(nrow(results$metrics))) {
    expect_equal(r_script_canonical(results$actual[[i]]),
                 r_script_canonical(results$expected[[i]]),
                 tolerance = 1e-8, info = results$metrics$type[[i]])
  }
  expect_true(all(results$metrics$png_written))
  expect_true(all(results$metrics$csv_written))

  # An independent, hand-calculated example: March values are 16,26,36,46,1006.
  bar <- dplyr::filter(results$actual[[3]], group == "0012")
  line <- dplyr::filter(results$actual[[2]], period == "Mar", group == "0012")
  expect_equal(bar$mean, 226)
  expect_equal(line$mean, 36)
  expect_equal(line$n, 5L)
  expect_setequal(results$actual[[1]]$series, c("Filtered", "All"))
})

test_that("exported code has focused sections and safely quoted settings", {
  pins <- r_script_pins()
  for (pin in pins) {
    pin$title <- "Quoted \"title\"\nnext line / \\ path"
    lines <- dashboard_r_scripts_script_text(pin, 1)
    expect_no_error(parse(text = lines))
    for (section in c("1. Packages", "2. Settings", "3. Read", "4. Analyse", "5. Plot", "6. Save")) {
      expect_true(any(grepl(section, lines, fixed = TRUE)))
    }
    expect_false(any(grepl("expand.grid|rlang::sym|ensure_columns|resolve_y_limit|params <-", lines)))
    expect_false(any(grepl("shiny::|library\\(thegeneral\\)", lines)))
    expect_lt(length(lines), 200)
  }
  bar <- paste(dashboard_r_scripts_script_text(pins[[3]], 1), collapse = "\n")
  expect_false(grepl("geom_density|geom_smooth|plot_stacked_area", bar))
  expect_match(bar, "geom_col", fixed = TRUE)
  expect_match(bar, "latest_period", fixed = TRUE)
  expect_error(dashboard_r_scripts_script_text(list(type = "unknown", params = list()), 1),
               "Unsupported pin type", fixed = TRUE)
})

test_that("unfiltered single lines, large scatter samples and Year mode retain their rules", {
  fixture <- r_script_fixture()
  pins <- r_script_pins()
  output <- withr::local_tempdir(pattern = "rx_")
  run_pin <- function(pin, data = fixture$all, all_data = data) {
    archive <- file.path(output, "edge.zip")
    dashboard_r_scripts_write_zip(list(pin), archive, data, all_data)
    utils::unzip(archive, exdir = output)
    result <- r_script_execute(file.path(output, dashboard_r_scripts_filename(pin, 1)), output)
    expect_equal(r_script_canonical(result),
                 r_script_canonical(r_script_expected(pin, data, all_data)), tolerance = 1e-8)
    result
  }
  single <- run_pin(pins[[1]])
  expect_false("series" %in% names(single))

  large <- dplyr::bind_rows(fixture$all, fixture$all, fixture$all, fixture$all)
  scatter <- run_pin(pins[[8]], large)
  expect_equal(nrow(scatter), 1000L)

  year_pin <- pins[[7]]
  year_pin$params$TimeVariable7 <- "Year"
  year <- run_pin(year_pin)
  expect_setequal(year$period, unique(fixture$all$Year) + 2018L)

  # Cover the single-line surplus exception, including missing and negative values.
  surplus <- fixture$all
  surplus$Num_Surplus <- surplus$Num_value
  single_pin <- pins[[1]]
  single_pin$params$NumericVariable <- "Num_Surplus"
  run_pin(single_pin, surplus)

  # Non-syntactic column names must remain data, not become executable R code.
  quoted <- fixture$all
  names(quoted)[names(quoted) == "Cat_group"] <- "Group 'quoted' / \\ code"
  pin <- pins[[3]]
  pin$params$GroupVar <- "Group 'quoted' / \\ code"
  run_pin(pin, quoted)
})

test_that("ZIPs contain only this export and never a repo README", {
  output <- withr::local_tempdir(pattern = "rx_")
  fixture <- r_script_fixture()
  pins <- r_script_pins()
  archive <- file.path(output, "export.zip")
  dashboard_r_scripts_write_zip(pins, archive, fixture$filtered, fixture$all)
  names <- utils::unzip(archive, list = TRUE)$Name
  expect_equal(sum(endsWith(names, ".R")), 9L)
  expect_true(all(c("dashboard_data.csv", "dashboard_all_data.csv", "RUN_SCRIPTS.txt") %in% names))
  expect_false(any(tolower(basename(names)) == "readme.md"))

  dashboard_r_scripts_write_zip(list(pins[[3]]), archive, fixture$filtered)
  names <- utils::unzip(archive, list = TRUE)$Name
  expect_setequal(names, c("01_bar_simple.R", "dashboard_data.csv", "RUN_SCRIPTS.txt"))
  expect_error(dashboard_r_scripts_write_zip(list(pins[[1]]), archive, fixture$filtered),
               "Full mapped data", fixed = TRUE)
  expect_error(dashboard_r_scripts_write_zip(list(), archive), "No dashboard pins", fixed = TRUE)
})
