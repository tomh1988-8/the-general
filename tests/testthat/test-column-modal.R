testthat::test_that("suggestions use cleaning without changing the raw data", {
  raw <- tibble::tibble(
    event_date = c("2024-01-01", "02/01/2024", "Jan 03, 2024", "missing"),
    amount = c(" £1,234 ", "1.234,56", "85%", "-999"),
    region = c("North", "South", "North", "NA"),
    edition = c("1st", "2nd", "revised", "3rd"),
    tier = c("top-10", "top-100", "long-tail", "missing"),
    age_group = c("6-12", "13-17", "18+", "0-5"),
    padded_id = c("0012", "0013", "0014", "NA"),
    flag = c("TRUE", "FALSE", "TRUE", "unknown"),
    blank = c("", "unknown", "missing", "NA")
  )
  before <- raw
  suggestions <- data_user_mapping_suggestions(raw)

  testthat::expect_identical(raw, before)
  testthat::expect_equal(
    suggestions$suggested_role,
    c("date", "numeric", rep("categorical", 6), "review")
  )
  testthat::expect_equal(suggestions$numeric_values[2], 3)
  testthat::expect_equal(suggestions$date_values[1], 3)
  board <- data_user_mapping_board(suggestions)
  testthat::expect_equal(board$column, names(raw))
  testthat::expect_true(all(board$included[1:8]))
  testthat::expect_false(board$included[9])
})

testthat::test_that("a few dirty entries do not hide a numeric column", {
  suggestions <- data_user_mapping_suggestions(tibble::tibble(
    mostly_numbers = c(as.character(1:9), "oops"),
    mixed_text = c(as.character(1:8), "alpha", "beta")
  ))
  testthat::expect_equal(suggestions$suggested_role, c("numeric", "categorical"))
  testthat::expect_equal(suggestions$numeric_values, c(9, 8))
})

testthat::test_that("the messy books fixture matches all 26 labelled roles", {
  books <- readr::read_csv(
    testthat::test_path("..", "..", "data", "books.csv"),
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )
  expected_numeric <- c(
    "pages", "Chapters", "WORD COUNT", "rating \u2b50?", "PRINT RUN",
    "rrp\u00a3", "discount %", "stock_units", "reprints/", "FILM RIGHTS \u00a3"
  )
  expected_categorical <- c(
    "Genre", "format?", "language", "binding", "audience", "Publisher?",
    "series", "EDITION", "illustrated", "AWARD WINNING", "BESTSELLER TIER",
    "age_group", "Region Setting", "authorNationality", "Signed?"
  )
  suggestions <- data_user_mapping_suggestions(books)
  testthat::expect_equal(nrow(books), 1000L)
  testthat::expect_setequal(
    trimws(suggestions$column[suggestions$suggested_role == "numeric"]), expected_numeric
  )
  testthat::expect_setequal(
    trimws(suggestions$column[suggestions$suggested_role == "categorical"]), expected_categorical
  )
  testthat::expect_equal(suggestions$column[suggestions$suggested_role == "date"], "Date")
  board <- data_user_mapping_board(suggestions)
  testthat::expect_true(all(board$included))
  selection <- data_user_mapping_board_selection(board)
  contract <- data_ingest_uploaded_sheet(
    books, source_name = "books-regression", date_column = selection$date,
    numeric_columns = selection$numeric, categorical_columns = selection$categorical
  )
  testthat::expect_equal(contract$status, "ready")
  testthat::expect_equal(nrow(contract$standardized_data), 1000L)
  testthat::expect_equal(contract$standardized_data$Num_pages[1:2], c(1404, 880))
  testthat::expect_equal(contract$standardized_data$Num_word_count[1], 5668)
  testthat::expect_equal(as.character(contract$standardized_data$Cat_edition[1]), "2nd")
  testthat::expect_equal(as.character(contract$standardized_data$Cat_age_group[1]), "6-12")
})

testthat::test_that("the board shows selected columns, arrows and one confirmation action", {
  raw <- tibble::tibble(event_date = "2024-01-01", cases = "12", region = "North")
  suggestions <- data_user_mapping_suggestions(raw)
  board <- data_user_mapping_board(suggestions)
  html <- as.character(helper_mapping_board(shiny::NS("columns"), board, suggestions, 1L))
  modal <- as.character(helper_choose_types_modal(shiny::NS("columns")))

  for (role in c("date", "numeric", "categorical")) {
    testthat::expect_match(html, paste0('data-role="', role, '"'), fixed = TRUE)
  }
  for (column in names(raw)) {
    testthat::expect_match(html, paste0("<span>", column, "</span>"), fixed = TRUE)
  }
  testthat::expect_match(html, 'id="columns-map_include_1"', fixed = TRUE)
  testthat::expect_match(html, 'checked="checked"', fixed = TRUE)
  testthat::expect_match(html, 'aria-label="Move cases to Date"', fixed = TRUE)
  testthat::expect_match(html, 'aria-label="Move cases to Categorical"', fixed = TRUE)
  testthat::expect_match(modal, "Confirm columns", fixed = TRUE)
  testthat::expect_match(modal, "Suggestions can be wrong.", fixed = TRUE)
  testthat::expect_false(grepl("Select suggested", modal, fixed = TRUE))

  names(raw)[3] <- '<img src=x onerror="alert(1)">'
  suggestions <- data_user_mapping_suggestions(raw)
  html <- as.character(helper_mapping_board(
    shiny::NS("columns"), data_user_mapping_board(suggestions), suggestions, 1L
  ))
  testthat::expect_false(grepl("<img", html, fixed = TRUE))
  testthat::expect_match(html, "&lt;img", fixed = TRUE)
})

testthat::test_that("review supports edits, confirmation, cancel and fresh uploads", {
  modal <- NULL
  notification <- NULL
  testthat::local_mocked_bindings(
    showModal = function(ui, ...) { modal <<- ui },
    removeModal = function(...) {},
    showNotification = function(ui, ...) { notification <<- ui },
    .package = "shiny"
  )
  upload <- tempfile(fileext = ".csv")
  withr::defer(unlink(upload))
  readr::write_csv(tibble::tibble(
    event_date = c("2024-01-01", "2024-01-02"),
    cases = c("10", "20"),
    rating = c("1", "5"),
    region = c("North", "South"),
    learner_id = c("0012", "0013"),
    empty = c("", "unknown")
  ), upload)

  shiny::testServer(mod_global_filter_server, {
    session$setInputs(files = data.frame(name = "example.csv", datapath = upload))
    testthat::expect_equal(rv$raw_upload$learner_id, c("0012", "0013"))
    session$setInputs(map_open = 1)
    testthat::expect_null(rv$data)
    testthat::expect_match(as.character(modal), "Confirm columns", fixed = TRUE)
    draft <- data_user_mapping_board_selection(mapping_draft())
    testthat::expect_equal(draft$date, "event_date")
    testthat::expect_equal(draft$numeric, c("cases", "rating"))
    testthat::expect_equal(draft$categorical, c("region", "learner_id"))

    edit <- function(index, action, value = NULL, epoch = mapping_epoch()) {
      session$setInputs(map_edit = list(index = index, action = action, value = value, epoch = epoch))
    }
    edit(3, "right")
    testthat::expect_equal(mapping_draft()$role[3], "categorical")
    edit(3, "left")
    testthat::expect_equal(mapping_draft()$role[3], "numeric")
    edit(2, "include", FALSE)
    testthat::expect_false(mapping_draft()$included[2])
    session$setInputs(map_cancel = 1)
    testthat::expect_null(rv$data)
    session$setInputs(map_open = 2)
    testthat::expect_true(mapping_draft()$included[2])

    # Reject no date or multiple dates without changing the loaded dataset.
    edit(1, "include", FALSE)
    session$setInputs(map_apply = 1)
    testthat::expect_null(rv$data)
    testthat::expect_match(notification, "exactly one date", fixed = TRUE)
    edit(1, "include", TRUE)
    edit(3, "also_categorical", TRUE)
    session$setInputs(map_apply = 2)
    testthat::expect_true(rv$step2_done)
    testthat::expect_false(mapping_is_open())
    testthat::expect_true(all(c("Num_rating", "Cat_rating") %in% names(rv$data)))
    testthat::expect_equal(as.character(rv$data$Cat_learner_id), c("0012", "0013"))
    loaded <- rv$data
    saved <- mapping_saved_board()

    session$setInputs(map_open = 3)
    testthat::expect_identical(mapping_draft(), saved)
    edit(3, "left")
    session$setInputs(map_apply = 3)
    testthat::expect_identical(rv$data, loaded)
    testthat::expect_true(mapping_is_open())
    session$setInputs(map_cancel = 2)
    session$setInputs(map_open = 4)
    testthat::expect_identical(mapping_draft(), saved)

    old_epoch <- mapping_epoch()
    session$setInputs(files = data.frame(name = "new.csv", datapath = upload))
    testthat::expect_null(rv$data)
    testthat::expect_null(mapping_saved_board())
    session$setInputs(map_open = 5)
    fresh <- mapping_draft()
    edit(3, "right", epoch = old_epoch)
    testthat::expect_identical(mapping_draft(), fresh)
    testthat::expect_false(mapping_draft()$also_categorical[3])
  })
})
