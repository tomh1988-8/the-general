# fn_ui.R

#' Header chip: single tab link
#' @noRd
general_header_nav_link <- function(label, tab_name, chip_id) {
  tags$button(
    id = chip_id,
    type = "button",
    class = "btn btn-sm header-chip general-header-chip general-header-nav-item",
    `data-tab` = tab_name,
    label
  )
}

#' Header chip: grouped dropdown
#' @noRd
general_header_nav_dropdown <- function(label, chip_id, items) {
  tags$div(
    class = "dropdown general-header-nav-dropdown",
    style = "display:inline-block;",
    tags$button(
      id = chip_id,
      type = "button",
      class = "btn btn-sm header-chip general-header-chip dropdown-toggle general-header-dropdown-toggle",
      `data-toggle` = "dropdown",
      `aria-haspopup` = "true",
      `aria-expanded` = "false",
      label
    ),
    tags$div(
      class = "dropdown-menu dropdown-menu-right general-header-dropdown-menu",
      lapply(names(items), function(tab_name) {
        tags$a(
          href = "#",
          class = "dropdown-item general-header-dropdown-item",
          `data-tab` = tab_name,
          items[[tab_name]]
        )
      })
    )
  )
}

#' Header navigation UI
#' @noRd
general_header_nav_ui <- function() {
  tags$li(
    class = "dropdown general-header-nav-shell",
    style = paste(
      "display:flex;",
      "align-items:center;",
      "gap:6px;",
      "font-size:10px;",
      "margin-left:10px;",
      "margin-right:14px;",
      "padding-right:10px;",
      "flex-wrap:wrap;"
    ),
    general_header_nav_link("Home", "frontPage", "ghn_frontPage"),
    general_header_nav_link("Frequencies", "Frequencies", "ghn_Frequencies"),
    general_header_nav_dropdown(
      "Percentages",
      "ghn_Proportions",
      c(
        Proportions = "1 Group",
        Proportions2 = "2 Groups"
      )
    ),
    general_header_nav_dropdown(
      "Averages",
      "ghn_Averages",
      c(
        Averages = "1 Group",
        Averages2 = "2 Groups"
      )
    ),
    general_header_nav_dropdown(
      "Lines",
      "ghn_Lines",
      c(
        Lines = "1 Group",
        Lines2 = "2 Groups"
      )
    ),
    general_header_nav_dropdown(
      "Bars",
      "ghn_Bars",
      c(
        Bars = "Simple",
        Bars2 = "Stacked",
        Bars3 = "Grouped"
      )
    ),
    general_header_nav_dropdown(
      "Areas",
      "ghn_Areas",
      c(
        Areas = "Stacked",
        Areas2 = "%"
      )
    ),
    general_header_nav_link("Scatterplots", "ScatterGrouped", "ghn_Scatter"),
    general_header_nav_link("Density", "Density", "ghn_Density"),
    general_header_nav_link("Dashboard", "myDashboard", "ghn_Dashboard")
  )
}

#' Sidebar for The General navigation
#' @noRd
general_sidebar_ui <- function() {
  bs4Dash::bs4DashSidebar(
    skin = "dark",
    status = "info",
    elevation = 4,
    expandOnHover = FALSE,
    minified = FALSE,
    collapsed = FALSE,

    bs4Dash::bs4SidebarMenu(
      id = "tabs",

      bs4Dash::bs4SidebarMenuItem(
        "Home",
        tabName = "frontPage",
        icon = shiny::icon("house")
      ),

      shiny::tags$li(
        class = "nav-header general-sidebar-divider general-sidebar-divider-after-home",
        shiny::tags$hr()
      ),

      bs4Dash::bs4SidebarMenuItem(
        "Frequencies",
        tabName = "Frequencies",
        icon = shiny::icon("table")
      ),

      bs4Dash::bs4SidebarMenuItem(
        "Percentages",
        icon = shiny::icon("percent"),
        startExpanded = FALSE,
        bs4Dash::bs4SidebarMenuSubItem(
          "1 Group",
          tabName = "Proportions",
          icon = shiny::icon("circle-dot")
        ),
        bs4Dash::bs4SidebarMenuSubItem(
          "2 Groups",
          tabName = "Proportions2",
          icon = shiny::icon("circle-dot")
        )
      ),

      bs4Dash::bs4SidebarMenuItem(
        "Averages",
        icon = shiny::icon("chart-simple"),
        startExpanded = FALSE,
        bs4Dash::bs4SidebarMenuSubItem(
          "1 Group",
          tabName = "Averages",
          icon = shiny::icon("circle-dot")
        ),
        bs4Dash::bs4SidebarMenuSubItem(
          "2 Groups",
          tabName = "Averages2",
          icon = shiny::icon("circle-dot")
        )
      ),

      bs4Dash::bs4SidebarMenuItem(
        "Lines",
        icon = shiny::icon("chart-line"),
        startExpanded = FALSE,
        bs4Dash::bs4SidebarMenuSubItem(
          "1 Group",
          tabName = "Lines",
          icon = shiny::icon("circle-dot")
        ),
        bs4Dash::bs4SidebarMenuSubItem(
          "2 Groups",
          tabName = "Lines2",
          icon = shiny::icon("circle-dot")
        )
      ),

      bs4Dash::bs4SidebarMenuItem(
        "Bars",
        icon = shiny::icon("chart-column"),
        startExpanded = FALSE,
        bs4Dash::bs4SidebarMenuSubItem(
          "Simple",
          tabName = "Bars",
          icon = shiny::icon("circle-dot")
        ),
        bs4Dash::bs4SidebarMenuSubItem(
          "Stacked",
          tabName = "Bars2",
          icon = shiny::icon("circle-dot")
        ),
        bs4Dash::bs4SidebarMenuSubItem(
          "Grouped",
          tabName = "Bars3",
          icon = shiny::icon("circle-dot")
        )
      ),

      bs4Dash::bs4SidebarMenuItem(
        "Areas",
        icon = shiny::icon("chart-area"),
        startExpanded = FALSE,
        bs4Dash::bs4SidebarMenuSubItem(
          "Stacked",
          tabName = "Areas",
          icon = shiny::icon("circle-dot")
        ),
        bs4Dash::bs4SidebarMenuSubItem(
          "Percentage",
          tabName = "Areas2",
          icon = shiny::icon("circle-dot")
        )
      ),

      bs4Dash::bs4SidebarMenuItem(
        "Scatterplots",
        tabName = "ScatterGrouped",
        icon = shiny::icon("braille")
      ),

      bs4Dash::bs4SidebarMenuItem(
        "Density",
        tabName = "Density",
        icon = shiny::icon("wave-square")
      ),

      bs4Dash::bs4SidebarMenuItem(
        "My Dashboard",
        tabName = "myDashboard",
        icon = shiny::icon("gauge-high")
      )
    ),

    customArea = shiny::tags$div(
      class = "general-sidebar-logo-shell",
      shiny::tags$span(
        class = "general-sidebar-name",
        "The General"
      )
    )
  )
}

#' Wrapper: Front Page UI
#' @noRd
front_page_tab_panel_ui <- function() {
  mod_global_filter_ui("global_1")
}

#' Wrapper: Frequencies UI
#' @noRd
frequencies_tab_panel_ui <- function() {
  mod_frequencies_ui("freq_1")
}

#' Wrapper: Proportions (1 Group) UI
#' @noRd
proportions_tab_panel_ui <- function() {
  mod_proportions_ui("prop_1")
}

#' Wrapper: Proportions (2 Groups) UI
#' @noRd
proportions2_tab_panel_ui <- function() {
  mod_proportions2_ui("prop2_1")
}

#' Wrapper: Averages (1 Group) UI
#' @noRd
averages_tab_panel_ui <- function() {
  mod_averages_ui("avg_1")
}

#' Wrapper: Averages (2 Groups) UI
#' @noRd
averages2_tab_panel_ui <- function() {
  mod_averages2_ui("avg2_1")
}

#' Wrapper: Lines (1 Group) UI
#' @noRd
lines_tab_panel_ui <- function() {
  mod_lines_ui("lines_1")
}

#' Wrapper: Lines (2 Groups) UI
#' @noRd
lines2_tab_panel_ui <- function() {
  mod_lines2_ui("lines2_1")
}

#' Wrapper: Bars (Simple) UI
#' @noRd
bars_tab_panel_ui <- function() {
  mod_bars_ui("bars_1")
}

#' Wrapper: Bars (Stacked) UI
#' @noRd
bars2_tab_panel_ui <- function() {
  mod_bars2_ui("bars2_1")
}

#' Wrapper: Bars (Grouped) UI
#' @noRd
bars3_tab_panel_ui <- function() {
  mod_bars3_ui("bars3_1")
}

#' Wrapper: Areas (Stacked) UI
#' @noRd
areas_tab_panel_ui <- function() {
  mod_areas_ui("areas_1")
}

#' Wrapper: Areas (%) UI
#' @noRd
areas2_tab_panel_ui <- function() {
  mod_areas2_ui("areas2_1")
}

#' Wrapper: Scatterplots UI
#' @noRd
scatter_tab_panel_ui <- function() {
  mod_scatterplots_ui("scatter_1")
}

#' Wrapper: Density UI
#' @noRd
density_tab_panel_ui <- function() {
  mod_density_ui("density_1")
}

#' Wrapper: Dashboard UI
#' @noRd
my_dashboard_tab_panel_ui <- function() {
  mod_my_dashboard_ui("dash_1")
}

#' Wrapper: App tab items compilation
#' @noRd
general_tab_items_ui <- function() {
  bs4Dash::tabItems(
    bs4Dash::tabItem(tabName = "frontPage", front_page_tab_panel_ui()),
    bs4Dash::tabItem(tabName = "Frequencies", frequencies_tab_panel_ui()),
    bs4Dash::tabItem(tabName = "Proportions", proportions_tab_panel_ui()),
    bs4Dash::tabItem(tabName = "Proportions2", proportions2_tab_panel_ui()),
    bs4Dash::tabItem(tabName = "Averages", averages_tab_panel_ui()),
    bs4Dash::tabItem(tabName = "Averages2", averages2_tab_panel_ui()),
    bs4Dash::tabItem(tabName = "Lines", lines_tab_panel_ui()),
    bs4Dash::tabItem(tabName = "Lines2", lines2_tab_panel_ui()),
    bs4Dash::tabItem(tabName = "Bars", bars_tab_panel_ui()),
    bs4Dash::tabItem(tabName = "Bars2", bars2_tab_panel_ui()),
    bs4Dash::tabItem(tabName = "Bars3", bars3_tab_panel_ui()),
    bs4Dash::tabItem(tabName = "Areas", areas_tab_panel_ui()),
    bs4Dash::tabItem(tabName = "Areas2", areas2_tab_panel_ui()),
    bs4Dash::tabItem(tabName = "ScatterGrouped", scatter_tab_panel_ui()),
    bs4Dash::tabItem(tabName = "Density", density_tab_panel_ui()),
    bs4Dash::tabItem(tabName = "myDashboard", my_dashboard_tab_panel_ui())
  )
}
