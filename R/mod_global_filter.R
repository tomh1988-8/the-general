#' Global Filter UI
#' @export
mod_global_filter_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    id = ns("home_scope"),
    class = "general-home-page",
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::div(
          class = "general-home-actions",
          shiny::downloadButton(
            outputId = ns("download_example_input"),
            label = "Download example sheet",
            class = "btn btn-light"
          ),
          shiny::div(
            id = ns("download_posttreatment_dataset_lock"),
            class = "general-download-lock is-locked",
            title = "Apply filters or continue with all rows to unlock this download.",
            shinyjs::disabled(shiny::downloadButton(
              outputId = ns("download_posttreatment_dataset"),
              label = "Download your data",
              class = "btn btn-light"
            ))
          ),
          shiny::tags$a(
            id = ns("documentation"),
            href = "www/docs/index.html",
            target = "_blank",
            rel = "noopener",
            class = "btn btn-light",
            title = "Open documentation in a new tab",
            shiny::icon("book"),
            "Documentation",
            shiny::span(" (opens in a new tab)", class = "sr-only")
          )
        )
      )
    ),

    shiny::fluidRow(
      shiny::column(
        width = 4,
        bs4Dash::bs4Card(
          title = "1 · Upload",
          status = "info",
          width = 12,
          collapsible = FALSE,
          shiny::fileInput(
            ns("files"),
            label = NULL,
            buttonLabel = "Browse.",
            placeholder = "Upload one CSV",
            accept = ".csv",
            multiple = FALSE
          )
        )
      ),

      shiny::column(
        width = 4,
        bs4Dash::bs4Card(
          title = "2 · Review columns",
          status = "info",
          width = 12,
          collapsible = FALSE,
          shiny::uiOutput(ns("mappingControls"))
        )
      ),

      shiny::column(
        width = 4,
        shiny::div(
          class = "general-filter-card-shell",
          bs4Dash::bs4Card(
            title = "3 · Filters",
            status = "info",
            width = 12,
            collapsible = FALSE,
            shiny::uiOutput(ns("filterControls"))
          ),
          shiny::div(
            class = "general-filter-actions",
            shinyjs::disabled(shiny::actionButton(
              ns("apply_filters"),
              "Apply",
              class = "btn btn-light"
            )),
            shiny::actionButton(
              ns("reset_filters"),
              "Reset",
              class = "btn btn-light"
            )
          )
        )
      )
    )
  )
}

#' Global Filter Server
#' @export
mod_global_filter_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session_code <- session$userData$sessionCode %||% "unknown"
    dropdown_container <- paste0("#", ns("home_scope"))

    is_blank <- function(x) {
      is.null(x) || length(x) == 0L || !nzchar(x[[1]])
    }

    log_home_info <- function(text) {
      text <- as.character(text)
      logger::log_info(text)

      if (isTRUE(getOption("the_general.debug", FALSE))) {
        base::message("INFO ", text)
      }
    }

    log_home_debug <- function(text) {
      text <- as.character(text)
      logger::log_debug(text)

      if (isTRUE(getOption("the_general.debug", FALSE))) {
        base::message("DEBUG ", text)
      }
    }

    log_home_info(glue::glue(
      "[Session {session_code}] Home/global filter server starting | explicit user mapping flow"
    ))

    rv <- shiny::reactiveValues(
      dataset_name = NULL,
      raw_upload = NULL,
      ingestion_contract = NULL,
      schema_review = NULL,
      validation = NULL,
      capabilities = NULL,
      unfiltered = NULL,
      data = NULL,
      group_vars = character(0),
      step1_done = FALSE,
      step2_done = FALSE,
      step3_done = FALSE,
      ready_for_filters = FALSE
    )

    shiny::observe({
      download_ready <- isTRUE(rv$step3_done) && !is.null(rv$data)

      if (download_ready) {
        shinyjs::enable("download_posttreatment_dataset")
        shinyjs::removeClass("download_posttreatment_dataset_lock", "is-locked")
        shinyjs::addClass("download_posttreatment_dataset_lock", "is-ready")
      } else {
        shinyjs::disable("download_posttreatment_dataset")
        shinyjs::removeClass("download_posttreatment_dataset_lock", "is-ready")
        shinyjs::addClass("download_posttreatment_dataset_lock", "is-locked")
      }

      if (isTRUE(rv$ready_for_filters)) {
        shinyjs::enable("apply_filters")
      } else {
        shinyjs::disable("apply_filters")
      }
    })

    output$download_example_input <- shiny::downloadHandler(
      filename = function() {
        "example_input.csv"
      },
      content = function(file) {
        log_home_info(glue::glue(
          "[Session {session_code}] Downloading example input sheet"
        ))

        file.copy(
          from = file.path("data", "example_input.csv"),
          to = file,
          overwrite = TRUE
        )
      }
    )

    output$download_posttreatment_dataset <- shiny::downloadHandler(
      filename = function() {
        shiny::req(isTRUE(rv$step3_done), rv$data)

        base_name <- rv$dataset_name %||% "dashboard-data"
        base_name <- tools::file_path_sans_ext(basename(base_name))
        paste0(base_name, "-dashboard-data.csv")
      },
      content = function(file) {
        shiny::req(isTRUE(rv$step3_done), rv$data)

        log_home_info(glue::glue(
          "[Session {session_code}] Downloading mapped dashboard data | dataset={rv$dataset_name} | rows={nrow(rv$data)} | cols={ncol(rv$data)}"
        ))

        readr::write_csv(rv$data, file)
      }
    )

    finalize_dataset_ready <- function(contract) {
      standardized_data <- contract$standardized_data

      log_home_info(glue::glue(
        "[Session {session_code}] Finalizing mapped dataset | status={contract$status} | rows={nrow(standardized_data)} | cols={ncol(standardized_data)}"
      ))

      log_home_debug(glue::glue(
        "[Session {session_code}] Final mapped columns | {paste(names(standardized_data), collapse = ', ')}"
      ))

      log_home_debug(glue::glue(
        "[Session {session_code}] Available filter variables | {paste(contract$validation$variable_sets$grouping_vars, collapse = ', ')}"
      ))

      rv$ingestion_contract <- contract
      rv$schema_review <- contract$schema_review
      rv$capabilities <- contract$capabilities
      rv$validation <- contract$validation
      rv$unfiltered <- standardized_data
      rv$data <- standardized_data
      rv$group_vars <- contract$validation$variable_sets$grouping_vars
      rv$ready_for_filters <- TRUE
      rv$step2_done <- TRUE
      rv$step3_done <- FALSE

      invisible(rv)
    }

    mapping_saved_board <- shiny::reactiveVal(NULL)
    mapping_draft <- shiny::reactiveVal(NULL)
    mapping_is_open <- shiny::reactiveVal(FALSE)
    mapping_epoch <- shiny::reactiveVal(0L)
    mapping_focus <- shiny::reactiveVal(NULL)
    mapping_suggestions <- shiny::reactive({
      shiny::req(rv$raw_upload)
      data_user_mapping_suggestions(rv$raw_upload)
    })

    output$mappingControls <- shiny::renderUI({
      button <- shiny::actionButton(
        ns("map_open"), "Review columns", class = "btn btn-light"
      )
      if (is.null(rv$raw_upload)) shinyjs::disabled(button) else button
    })

    output$mappingBoard <- shiny::renderUI({
      shiny::req(mapping_is_open(), mapping_draft())
      helper_mapping_board(
        ns, mapping_draft(), mapping_suggestions(), mapping_epoch(), mapping_focus()
      )
    })

    shiny::observeEvent(input$map_open, {
      shiny::req(rv$raw_upload)
      mapping_epoch(mapping_epoch() + 1L)
      mapping_focus(NULL)
      saved <- mapping_saved_board()
      mapping_draft(if (is.null(saved)) data_user_mapping_board(mapping_suggestions()) else saved)
      mapping_is_open(TRUE)
      shiny::showModal(helper_choose_types_modal(ns))
    })

    shiny::observeEvent(input$map_cancel, {
      mapping_is_open(FALSE)
      mapping_draft(NULL)
      shiny::removeModal()
    })

    shiny::observeEvent(input$map_edit, {
      shiny::req(mapping_is_open(), mapping_draft())
      event <- input$map_edit
      shiny::req(isTRUE(event$epoch == mapping_epoch()))
      board <- mapping_draft()
      i <- event$index
      shiny::req(length(i) == 1L, i %in% seq_len(nrow(board)))
      roles <- c("date", "numeric", "categorical")
      if (event$action %in% c("left", "right")) {
        position <- match(board$role[i], roles)
        destination <- position + if (event$action == "left") -1L else 1L
        shiny::req(destination %in% seq_along(roles))
        board$role[i] <- roles[destination]
        board$also_categorical[i] <- FALSE
      } else if (event$action == "include") {
        board$included[i] <- isTRUE(event$value)
      } else if (event$action == "also_categorical" && board$role[i] == "numeric") {
        board$also_categorical[i] <- isTRUE(event$value)
      } else {
        return(invisible(NULL))
      }
      mapping_focus(list(index = i, action = event$action))
      mapping_draft(board)
    })

    shiny::observeEvent(input$files, {
      shiny::req(input$files)

      log_home_info(glue::glue(
        "[Session {session_code}] Upload received | name={input$files$name} | path={input$files$datapath}"
      ))

      rv$dataset_name <- input$files$name
      rv$raw_upload <- readr::read_csv(
        input$files$datapath,
        col_types = readr::cols(.default = readr::col_character()),
        show_col_types = FALSE
      )

      mapping_is_open(FALSE)
      shiny::removeModal()
      mapping_epoch(mapping_epoch() + 1L)
      mapping_saved_board(NULL)
      mapping_draft(NULL)

      rv$step1_done <- TRUE
      rv$step2_done <- FALSE
      rv$step3_done <- FALSE
      rv$ready_for_filters <- FALSE
      rv$ingestion_contract <- NULL
      rv$schema_review <- NULL
      rv$validation <- NULL
      rv$capabilities <- NULL
      rv$unfiltered <- NULL
      rv$data <- NULL
      rv$group_vars <- character(0)

      log_home_info(glue::glue(
        "[Session {session_code}] Raw upload loaded | rows={nrow(rv$raw_upload)} | cols={ncol(rv$raw_upload)}"
      ))

      log_home_debug(glue::glue(
        "[Session {session_code}] Raw upload columns | {paste(names(rv$raw_upload), collapse = ', ')}"
      ))
    })

    shiny::observeEvent(input$map_apply, {
      shiny::req(rv$raw_upload, mapping_is_open())
      selection <- data_user_mapping_board_selection(mapping_draft())
      if (length(selection$date) != 1L || length(selection$numeric) == 0L ||
          length(selection$categorical) == 0L) {
        shiny::showNotification(
          "Keep exactly one date column, at least one numeric and one categorical column ticked.",
          type = "warning"
        )
        return(invisible(NULL))
      }

      log_home_info(glue::glue(
        "[Session {session_code}] Applying explicit column mapping | dataset={rv$dataset_name}"
      ))

      log_home_debug(glue::glue(
        "[Session {session_code}] Mapping selections | date={selection$date} | numeric={paste(selection$numeric, collapse = ', ')} | categorical={paste(selection$categorical, collapse = ', ')}"
      ))

      contract <- data_ingest_uploaded_sheet(
        data = rv$raw_upload,
        source_name = rv$dataset_name,
        source_type = "upload",
        date_column = selection$date,
        numeric_columns = selection$numeric,
        categorical_columns = selection$categorical
      )

      log_home_info(glue::glue(
        "[Session {session_code}] Mapping contract built | status={contract$status}"
      ))

      log_home_debug(glue::glue(
        "[Session {session_code}] Validation status | {contract$validation$validation_status}"
      ))

      log_home_debug(glue::glue(
        "[Session {session_code}] Capability table | {paste(contract$capabilities$module_support$capability, contract$capabilities$module_support$supported, sep = '=', collapse = ', ')}"
      ))

      if (contract$status == "fatal") {
        log_home_info(glue::glue(
          "[Session {session_code}] Explicit mapping failed validation | dataset={rv$dataset_name}"
        ))

        shiny::showNotification(
          "Those selected columns could not produce a usable dashboard dataset.",
          type = "error"
        )

        return(invisible(NULL))
      }

      finalize_dataset_ready(contract)
      mapping_saved_board(mapping_draft())
      mapping_is_open(FALSE)
      shiny::removeModal()

      shiny::showNotification(
        "Selected columns loaded. Apply filters or continue with all rows to unlock the download.",
        type = "message"
      )

      log_home_info(glue::glue(
        "[Session {session_code}] Explicit column mapping complete | dataset={rv$dataset_name}"
      ))
    })

    output$filterControls <- shiny::renderUI({
      if (!rv$ready_for_filters) {
        return(
          shiny::tagList(
            shinyjs::disabled(shinyWidgets::pickerInput(
              inputId = ns("filter_var"),
              label = "Filter:",
              choices = c("Select a filter column" = ""),
              selected = "",
              multiple = FALSE,
              options = list(
                `live-search` = TRUE,
                `style` = "btn-light",
                `none-selected-text` = "Select a filter column"
              )
            )),
            shinyjs::disabled(shinyWidgets::pickerInput(
              inputId = ns("filter_values"),
              label = "Values:",
              choices = character(0),
              selected = character(0),
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                `live-search` = TRUE,
                `style` = "btn-light",
                `none-selected-text` = "Select a filter column first"
              )
            ))
          )
        )
      }

      shiny::req(rv$unfiltered)

      grouping_vars <- rv$validation$variable_sets$grouping_vars %||%
        character()

      filter_choices <- c(
        "Select a filter column" = "",
        stats::setNames(grouping_vars, grouping_vars)
      )

      log_home_debug(glue::glue(
        "[Session {session_code}] Rendering filter controls | available={paste(grouping_vars, collapse = ', ')} | selected=none"
      ))

      shiny::tagList(
        shinyWidgets::pickerInput(
          inputId = ns("filter_var"),
          label = "Filter:",
          choices = filter_choices,
          selected = "",
          multiple = FALSE,
          options = list(
            `live-search` = TRUE,
            `style` = "btn-light",
            `none-selected-text` = "Select a filter column",
            `dropupAuto` = FALSE,
            `container` = dropdown_container,
            `size` = 8
          )
        ),
        shiny::uiOutput(ns("filterValueControls"))
      )
    })

    output$filterValueControls <- shiny::renderUI({
      shiny::req(rv$unfiltered)

      if (is_blank(input$filter_var)) {
        return(
          shinyjs::disabled(shinyWidgets::pickerInput(
            inputId = ns("filter_values"),
            label = "Values:",
            choices = character(0),
            selected = character(0),
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              `style` = "btn-light",
              `none-selected-text` = "Select a filter column first",
              `dropupAuto` = FALSE,
              `container` = dropdown_container,
              `size` = 8
            )
          ))
        )
      }

      vals <- rv$unfiltered |>
        dplyr::pull(input$filter_var) |>
        unique() |>
        stats::na.omit() |>
        sort()

      log_home_debug(glue::glue(
        "[Session {session_code}] Rendering filter values | var={input$filter_var} | values={paste(vals, collapse = ', ')} | selected=none"
      ))

      shinyWidgets::pickerInput(
        inputId = ns("filter_values"),
        label = "Values:",
        choices = vals,
        selected = character(0),
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `style` = "btn-light",
          `none-selected-text` = "Select filter values",
          `dropupAuto` = FALSE,
          `container` = dropdown_container,
          `size` = 8
        )
      )
    })

    shiny::observeEvent(input$apply_filters, {
      shiny::req(rv$unfiltered)

      if (is_blank(input$filter_var)) {
        rv$data <- rv$unfiltered
        rv$step3_done <- TRUE

        log_home_info(glue::glue(
          "[Session {session_code}] Optional filters skipped | no filter column selected | rows={nrow(rv$data)}"
        ))

        shiny::showNotification(
          "No filter selected. All rows are available for download.",
          type = "message"
        )

        return(invisible(NULL))
      }

      if (is.null(input$filter_values) || length(input$filter_values) == 0L) {
        rv$data <- rv$unfiltered
        rv$step3_done <- TRUE

        log_home_info(glue::glue(
          "[Session {session_code}] Optional filters skipped | var={input$filter_var} | no values selected | rows={nrow(rv$data)}"
        ))

        shiny::showNotification(
          "No filter values selected. All rows are available for download.",
          type = "message"
        )

        return(invisible(NULL))
      }

      log_home_info(glue::glue(
        "[Session {session_code}] Applying global filter | var={input$filter_var} | selected_count={length(input$filter_values)}"
      ))

      log_home_debug(glue::glue(
        "[Session {session_code}] Selected filter values | {paste(input$filter_values, collapse = ', ')}"
      ))

      rv$data <- data_apply_filter(
        rv$unfiltered,
        input$filter_var,
        input$filter_values
      )

      rv$step3_done <- TRUE

      log_home_info(glue::glue(
        "[Session {session_code}] Global filter applied | rows_before={nrow(rv$unfiltered)} | rows_after={nrow(rv$data)}"
      ))

      shiny::showNotification(
        glue::glue(
          "Filters applied. {nrow(rv$data)} rows are available for download."
        ),
        type = "message"
      )
    })

    shiny::observeEvent(input$reset_filters, {
      log_home_info(glue::glue(
        "[Session {session_code}] Full Home page reset requested"
      ))

      session$reload()
    })

    log_home_info(glue::glue(
      "[Session {session_code}] Home/global filter server ready"
    ))

    rv
  })
}
