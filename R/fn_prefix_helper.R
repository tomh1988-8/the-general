# Keep review in one modal; the draft is only loaded after confirmation.
helper_choose_types_modal <- function(ns) {
  shiny::modalDialog(
    title = "Review columns",
    size = "l",
    easyClose = FALSE,
    shiny::div(
      class = "general-mapping-review",
      shiny::p(
        class = "text-muted",
        "Suggestions can be wrong. Move columns with the arrows; untick to leave out."
      ),
      shiny::uiOutput(ns("mappingBoard"))
    ),
    shiny::tags$style(shiny::HTML(r"(
      #shiny-modal .modal-lg {max-width:1100px;width:calc(100vw - 2rem);}
      .general-mapping-review {overflow-x:auto;}
      .general-mapping-board {display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:12px;min-width:630px;}
      .general-mapping-lane {min-width:0;border:1px solid #dce3e9;border-radius:8px;background:#f5f7f9;}
      .general-mapping-heading {display:flex;justify-content:space-between;gap:8px;font-size:16px;font-weight:600;margin:0;padding:12px;border-bottom:1px solid #dce3e9;}
      .general-mapping-count {color:#546575;font-size:13px;}
      .general-mapping-list {padding:8px;max-height:52vh;overflow-y:auto;min-height:120px;}
      .general-mapping-card {background:#fff;border:1px solid #dce3e9;border-radius:6px;padding:10px;margin-bottom:8px;}
      .general-mapping-card.is-excluded {background:#f0f2f4;color:#687581;}
      .general-mapping-label {display:flex;align-items:flex-start;gap:8px;font-weight:600;margin:0;overflow-wrap:anywhere;}
      .general-mapping-label input {margin-top:4px;flex-shrink:0;}
      .general-mapping-controls {display:flex;justify-content:space-between;margin-top:8px;}
      .general-mapping-controls button {min-width:38px;border:1px solid #cbd5df;background:#fff;color:#274b66;border-radius:4px;padding:3px 9px;}
      .general-mapping-controls button:hover:not(:disabled) {background:#e6f1f7;}
      .general-mapping-controls button:disabled {opacity:.25;cursor:default;}
      .general-mapping-card button:focus-visible,.general-mapping-card input:focus-visible {outline:3px solid #167eab;outline-offset:2px;}
      .general-mapping-note {font-size:12px;color:#586b7a;margin-top:6px;}
      .general-mapping-empty {color:#647582;padding:8px;font-size:13px;}
      .general-mapping-extra {margin-top:12px;font-size:13px;}
      .general-mapping-extra summary {cursor:pointer;color:#375e78;}
      .general-mapping-extra label {display:inline-flex;gap:6px;margin:8px 16px 0 0;font-weight:normal;}
      @media(max-width:650px) {
        .general-mapping-board {gap:6px;}
        .general-mapping-heading {padding:8px 5px;font-size:13px;}
        .general-mapping-list {padding:4px;}
        .general-mapping-card {padding:6px;font-size:12px;}
      }
    )")),
    footer = shiny::tagList(
      shiny::actionButton(ns("map_cancel"), "Cancel", class = "btn btn-light"),
      shiny::actionButton(ns("map_apply"), "Confirm columns", class = "btn btn-info")
    )
  )
}

# Use stable row numbers in events, never user column names in JavaScript.
helper_mapping_board <- function(ns, board, suggestions, epoch, focus = NULL) {
  roles <- c("date", "numeric", "categorical")
  labels <- c(date = "Date", numeric = "Numeric", categorical = "Categorical")
  event_js <- function(i, action, checkbox = FALSE) {
    payload <- jsonlite::toJSON(
      list(index = i, action = action, epoch = epoch), auto_unbox = TRUE
    )
    paste0(
      "var event = ", payload, ";",
      if (checkbox) "event.value = this.checked;" else "",
      "Shiny.setInputValue(", jsonlite::toJSON(ns("map_edit"), auto_unbox = TRUE),
      ", event, {priority:'event'});"
    )
  }

  card <- function(i) {
    role <- board$role[i]
    position <- match(role, roles)
    arrow <- function(action, step) {
      destination <- position + step
      enabled <- destination %in% seq_along(roles)
      label <- if (enabled) paste("Move", board$column[i], "to", labels[destination]) else "No column in this direction"
      shiny::tags$button(
        id = ns(paste0("map_", action, "_", i)), type = "button",
        title = label, "aria-label" = label,
        disabled = if (!enabled) "disabled" else NULL,
        onclick = event_js(i, action),
        shiny::icon(paste0("arrow-", action))
      )
    }
    shiny::div(
      class = paste("general-mapping-card", if (board$included[i]) "" else "is-excluded"),
      "data-column-index" = i,
      title = paste(suggestions$reason[i], suggestions$examples[i], sep = "\n"),
      shiny::tags$label(
        class = "general-mapping-label",
        shiny::tags$input(
          id = ns(paste0("map_include_", i)), type = "checkbox",
          checked = if (board$included[i]) "checked" else NULL,
          onchange = event_js(i, "include", TRUE)
        ),
        shiny::span(board$column[i])
      ),
      if (suggestions$suggested_role[i] == "review") {
        shiny::div(class = "general-mapping-note", "No usable values")
      },
      if (role == "numeric" && board$also_categorical[i]) {
        shiny::div(class = "general-mapping-note", "Also categorical")
      },
      shiny::div(class = "general-mapping-controls", arrow("left", -1L), arrow("right", 1L))
    )
  }

  lanes <- lapply(roles, function(role) {
    indices <- which(board$role == role)
    shiny::tags$section(
      class = "general-mapping-lane", "aria-label" = labels[[role]], "data-role" = role,
      shiny::h4(
        class = "general-mapping-heading", labels[[role]],
        shiny::span(class = "general-mapping-count", sum(board$included[indices]))
      ),
      shiny::div(
        class = "general-mapping-list",
        if (role == "date" && sum(board$included[indices]) != 1L) {
          shiny::div(class = "general-mapping-note", "Keep one date column ticked.")
        },
        if (!length(indices)) shiny::div(class = "general-mapping-empty", "Move a column here."),
        lapply(indices, card)
      )
    )
  })

  numeric_indices <- which(board$role == "numeric")
  extra <- if (length(numeric_indices)) {
    shiny::tags$details(
      class = "general-mapping-extra",
      open = if (any(board$also_categorical[numeric_indices])) "open" else NULL,
      shiny::tags$summary("Also use numeric columns for grouping"),
      lapply(numeric_indices, function(i) {
        shiny::tags$label(
          shiny::tags$input(
            id = ns(paste0("map_also_categorical_", i)), type = "checkbox",
            checked = if (board$also_categorical[i]) "checked" else NULL,
            onchange = event_js(i, "also_categorical", TRUE)
          ),
          board$column[i]
        )
      })
    )
  }

  focus_script <- if (!is.null(focus)) {
    target <- ns(paste0("map_", focus$action, "_", focus$index))
    fallback <- ns(paste0("map_include_", focus$index))
    shiny::tags$script(shiny::HTML(sprintf(r"(
      requestAnimationFrame(function() {
        var el = document.getElementById(%s);
        if (!el || el.disabled) el = document.getElementById(%s);
        if (el) { el.focus({preventScroll:true}); el.scrollIntoView({block:'nearest'}); }
      });
    )", jsonlite::toJSON(target, auto_unbox = TRUE), jsonlite::toJSON(fallback, auto_unbox = TRUE))))
  }

  shiny::tagList(shiny::div(class = "general-mapping-board", lanes), extra, focus_script)
}
