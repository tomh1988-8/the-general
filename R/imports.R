# R/imports.R

# Core Shiny
#' @importFrom shiny
#'   shinyApp
#'   tags
#'   tagList
#'   singleton
#'   icon
#'   req
#'   validate
#'   need
#'   renderUI
#'   renderPlot
#'   plotOutput
#'   NS
#'   moduleServer
#'   observe
#'   observeEvent
#'   isolate
#'   reactive
#'   reactiveVal
#'   getDefaultReactiveDomain
#'   downloadButton
#'   downloadHandler
#'   fileInput
#'   uiOutput
#'   actionButton
#'   showNotification
#'   fluidRow
#'   column
#'   div
#'   h2
#'   tabsetPanel
#'   tabPanel
#'   showModal
#'   modalDialog
#'   modalButton

# Dashboards
#' @importFrom shinydashboard
#'   dashboardPage
#'   dashboardHeader
#'   dashboardSidebar
#'   dashboardBody
#'   sidebarMenu
#'   menuItem
#'   menuSubItem
#'   sidebarMenuOutput
#'   renderMenu
#'   tabItems
#'   tabItem
#'   updateTabItems
#'   box
#'   tabBox
#'   valueBox
#'   valueBoxOutput
#'   renderValueBox
#'   infoBox
#'   infoBoxOutput
#'   renderInfoBox
#'   dropdownMenu
#'   dropdownMenuOutput
#'   messageItem
#'   notificationItem
#'   taskItem
#'   sidebarSearchForm

# Auth
#' @importFrom shinymanager secure_app set_labels secure_server check_credentials

# UI widgets / HTML
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinybusy add_busy_spinner
#' @importFrom htmltools tags HTML
#' @importFrom htmlwidgets JS
#' @importFrom fresh use_theme create_theme adminlte_color adminlte_global adminlte_sidebar

# Logging / utils
#' @importFrom logger log_info log_error log_appender appender_file
#' @importFrom uuid UUIDgenerate
#' @importFrom glue glue
#' @importFrom utils read.csv write.csv sessionInfo
#' @importFrom stats quantile setNames na.omit
#' @importFrom grDevices png

# Tidyverse-friendly
#' @importFrom dplyr
#'   mutate
#'   mutate_if
#'   filter
#'   select
#'   arrange
#'   rename
#'   rename_with
#'   relocate
#'   group_by
#'   summarise
#'   across
#'   left_join
#'   inner_join
#'   right_join
#'   full_join
#'   semi_join
#'   anti_join
#'   bind_rows
#'   bind_cols
#'   distinct
#'   count
#'   n
#'   desc
#'   everything
#'   case_when
#'   if_else
#'   coalesce
#'   lag
#'   lead
#'   starts_with
#'   ends_with
#'   matches
#'   any_of
#'   n_distinct
#'   na_if
#' @importFrom tidyr
#'   pivot_longer
#'   pivot_wider
#'   unnest
#'   nest
#'   separate
#'   unite
#'   replace_na
#'   drop_na
#'   fill
#'   complete
#'   spread
#' @importFrom stringr
#'   str_detect
#'   str_replace
#'   str_replace_all
#'   str_to_lower
#'   str_to_upper
#'   str_to_title
#'   str_trim
#'   str_squish
#'   str_remove
#'   str_remove_all
#'   str_sub
#' @importFrom tibble tibble as_tibble tribble
#' @importFrom purrr map map2 map_chr map_dfr map_lgl imap_chr keep walk
#' @importFrom readr read_csv write_csv parse_number locale
#' @importFrom lubridate parse_date_time month year as_date
#' @importFrom zoo as.yearqtr
#' @importFrom janitor clean_names make_clean_names
#' @importFrom cli cli_alert_info
#' @importFrom vctrs vec_as_names

# Tables / plots
#' @import ggplot2
#' @importFrom DT
#'   datatable
#'   renderDT
#'   renderDataTable
#'   DTOutput
#'   dataTableOutput
#'   dataTableProxy
#'   replaceData
#'   formatStyle
#'   formatCurrency
#'   formatRound
#'   formatPercentage
#'   styleInterval
#' @importFrom plotly
#'   renderPlotly
#'   plotlyOutput
#'   ggplotly
#'   plot_ly
#'   add_trace
#'   add_markers
#'   add_lines
#'   add_bars
#'   layout
#'   config
#'   event_data
#' @importFrom openxlsx
#'   createWorkbook
#'   addWorksheet
#'   writeData
#'   saveWorkbook

NULL
