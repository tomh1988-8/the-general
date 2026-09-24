#' @title Keyboard Navigation UI (throttled, singleton-injected)
#'
#' @description
#' Injects a small JavaScript listener that turns Left/Right Arrow key
#' presses into Shiny inputs (`key_left`, `key_right`) within this module’s
#' namespace.
#'
#' @param id A character string with the module namespace.
#'
#' @return A UI fragment (`shiny.tag.list`) that injects the keyboard handler
#'   script into the page.
#' @export
keyNavUI <- function(id) {
  ns <- NS(id)

  tagList(
    singleton(
      tags$head(
        tags$script(HTML(sprintf(
          "
          $(function() {
            var cooldownMs = 250;
            var lastTs = 0;

            $(document).on('keydown', function(e) {
              var isInInput = $(e.target).is('input, textarea, select, [contenteditable=\"true\"]');
              if (isInInput) return;

              if (e.repeat) return;

              var nowTs = e.timeStamp || Date.now();
              if (nowTs - lastTs < cooldownMs) return;

              var code = e.which || e.keyCode;
              if (code === 37) {
                Shiny.setInputValue('%s', Date.now(), {priority: 'event'});
                lastTs = nowTs;
              } else if (code === 39) {
                Shiny.setInputValue('%s', Date.now(), {priority: 'event'});
                lastTs = nowTs;
              }
            });
          });
          ",
          ns("key_left"),
          ns("key_right")
        )))
      )
    )
  )
}

#' @title Keyboard Navigation Server Logic
#'
#' @description
#' Server-side module that listens for `key_left` / `key_right` events from
#' `keyNavUI()` and switches the active tab in a `bs4Dash` tab container.
#'
#' @param id Character scalar. Module namespace ID.
#' @param tab_order Character vector of tab names in navigation order.
#' @param menu_id Character scalar. The `inputId` of the tab container.
#' @param global_sess A Shiny session object.
#'
#' @return Invisibly returns `NULL`.
keyNavServer <- function(
  id,
  tab_order,
  menu_id = "tabs",
  global_sess = getDefaultReactiveDomain()
) {
  moduleServer(id, function(input, output, session) {
    sess <- if (is.null(global_sess)) session else global_sess

    get_current_index <- function() {
      current_tab <- sess$input[[menu_id]]
      match(current_tab, tab_order)
    }

    observeEvent(
      input$key_left,
      {
        idx <- get_current_index()
        if (is.na(idx)) {
          return(invisible(NULL))
        }

        prev_idx <- if (idx == 1) length(tab_order) else idx - 1

        bs4Dash::updateTabItems(
          session = sess,
          inputId = menu_id,
          selected = tab_order[[prev_idx]]
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$key_right,
      {
        idx <- get_current_index()
        if (is.na(idx)) {
          return(invisible(NULL))
        }

        next_idx <- if (idx == length(tab_order)) 1 else idx + 1

        bs4Dash::updateTabItems(
          session = sess,
          inputId = menu_id,
          selected = tab_order[[next_idx]]
        )
      },
      ignoreInit = TRUE
    )
  })
}
