#' Shared Custom Theme for Plots
#' @keywords internal
theme_general <- function() {
  ggplot2::theme(
    text = ggplot2::element_text(size = 15),
    axis.text.x = ggplot2::element_text(color = "#333333"),
    axis.text.y = ggplot2::element_text(color = "#333333"),
    strip.background = ggplot2::element_rect(
      colour = "#333333",
      fill = "#e6b950",
      linewidth = 1,
      linetype = "solid"
    ),
    strip.text = ggplot2::element_text(colour = "#333333"),
    axis.line.x.bottom = ggplot2::element_line(colour = "#333333"),
    axis.line.y.left = ggplot2::element_line(colour = "#333333"),
    axis.title.x = ggplot2::element_text(colour = "#333333"),
    axis.title.y = ggplot2::element_text(colour = "#333333"),
    legend.background = ggplot2::element_rect(
      colour = "#333333",
      fill = "white"
    ),
    legend.text = ggplot2::element_text(colour = "#333333"),
    legend.title = ggplot2::element_text(colour = "#333333")
  )
}

#' Shared palette for General plots
#' @keywords internal
general_palette <- function() {
  c(
    "#333333",
    "#17a2b8",
    "#eb8596",
    "#5c6b3c",
    "#d32f2f",
    "#e6b950",
    "#e0e0e0",
    "#90caf9",
    "#8e24aa",
    "#fcbb69",
    "#fdd835",
    "#1e88e5",
    "#bdbdbd",
    "#5e35b1",
    "#ff7043",
    "#00bfa5",
    "#ff4081",
    "#3949ab",
    "#f44336",
    "#c2185b",
    "#7b1fa2",
    "#512da8",
    "#303f9f",
    "#0288d1",
    "#0097a7",
    "#00796b",
    "#388e3c",
    "#689f38",
    "#afb42b",
    "#f57c00"
  )
}

#' Shared colour scale for General plots
#' @keywords internal
scale_colour_general <- function(...) {
  ggplot2::scale_colour_manual(values = general_palette(), ...)
}

#' Shared fill scale for General plots
#' @keywords internal
scale_fill_general <- function(...) {
  ggplot2::scale_fill_manual(values = general_palette(), ...)
}

#' Shared x-axis breaks for time-based plots
#' @keywords internal
plot_time_breaks <- function(df, time_var, time_in) {
  if (identical(time_in, "Month")) {
    return(c("Apr", "Jul", "Sep", "Dec"))
  }

  time_vals <- df[[time_var]]

  if (identical(time_in, "Financial_Quarter")) {
    return(unique(time_vals))
  }

  if (grepl("Quarter", time_in)) {
    u <- unique(time_vals)
    return(u[seq(1, length(u), by = 4)])
  }

  NULL
}

#' Shared x scale for time-based plots
#' @keywords internal
scale_x_time_general <- function(df, time_var, time_in) {
  x_breaks <- plot_time_breaks(df, time_var, time_in)

  if (is.null(x_breaks)) {
    return(NULL)
  }

  ggplot2::scale_x_discrete(breaks = x_breaks)
}

#' Create the ggplot object for Tab 10
#'
#' @import ggplot2
#' @importFrom rlang sym
plot_line_single_variable <- function(
  df,
  time_var,
  num_var,
  time_in,
  y_min = NA_real_,
  y_max = NA_real_
) {
  TimeSym <- rlang::sym(time_var)
  grouped <- "Category" %in% names(df)

  plt <- if (grouped) {
    ggplot2::ggplot(
      df,
      ggplot2::aes(
        x = !!TimeSym,
        y = Mean,
        group = Category,
        colour = Category
      )
    ) +
      ggplot2::geom_line(linewidth = 0.75) +
      ggplot2::geom_point() +
      scale_colour_general()
  } else {
    ggplot2::ggplot(df, ggplot2::aes(x = !!TimeSym, y = Mean)) +
      ggplot2::geom_line(
        group = 1,
        linewidth = 0.75,
        colour = "#333333"
      ) +
      ggplot2::geom_point(colour = "#333333")
  }

  x_scale <- scale_x_time_general(df, time_var, time_in)
  if (!is.null(x_scale)) {
    plt <- plt + x_scale
  }

  if (!is.na(y_min) || !is.na(y_max)) {
    data_limits <- range(df$Mean, na.rm = TRUE, finite = TRUE)

    if (is.na(y_min)) {
      y_min <- data_limits[[1]]
    }

    if (is.na(y_max)) {
      y_max <- data_limits[[2]]
    }

    if (y_min < y_max) {
      plt <- plt + ggplot2::coord_cartesian(ylim = c(y_min, y_max))
    }
  }

  plt +
    ggplot2::theme_bw() +
    theme_general() +
    ggplot2::ylab(num_var)
}

#' Build the ggplot for Tab 11 (grouped line chart)
#'
#' @import ggplot2
#' @importFrom rlang sym
plot_grouped_lines <- function(
  df,
  time_var,
  group_var,
  num_var,
  time_in,
  y_min = NA_real_,
  y_max = NA_real_
) {
  TimeSym <- rlang::sym(time_var)
  GroupSym <- rlang::sym(group_var)

  gp <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = !!TimeSym,
      y = Mean,
      group = !!GroupSym,
      colour = !!GroupSym
    )
  ) +
    ggplot2::geom_line(linewidth = 0.75) +
    ggplot2::geom_point() +
    ggplot2::theme_classic() +
    theme_general() +
    scale_colour_general() +
    ggplot2::ylab(num_var)

  x_scale <- scale_x_time_general(df, time_var, time_in)
  if (!is.null(x_scale)) {
    gp <- gp + x_scale
  }

  if (!is.na(y_min) || !is.na(y_max)) {
    data_limits <- range(df$Mean, na.rm = TRUE, finite = TRUE)

    if (is.na(y_min)) {
      y_min <- data_limits[[1]]
    }

    if (is.na(y_max)) {
      y_max <- data_limits[[2]]
    }

    if (y_min < y_max) {
      gp <- gp + ggplot2::coord_cartesian(ylim = c(y_min, y_max))
    }
  }

  gp
}

#' Build the ggplot bar chart for Tab 12
#'
#' @import ggplot2
#' @importFrom rlang sym
plot_simple_bar <- function(
  df,
  group_var,
  num_var,
  y_min = NA_real_,
  y_max = NA_real_
) {
  GroupSym <- rlang::sym(group_var)

  plt <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = !!GroupSym, y = Mean, fill = !!GroupSym)
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      colour = "#333333",
      linewidth = 0.75
    ) +
    scale_fill_general() +
    ggplot2::theme_classic() +
    theme_general() +
    ggplot2::ylab(num_var)

  if (!is.na(y_min) || !is.na(y_max)) {
    data_limits <- range(df$Mean, na.rm = TRUE, finite = TRUE)

    if (is.na(y_min)) {
      y_min <- data_limits[[1]]
    }

    if (is.na(y_max)) {
      y_max <- data_limits[[2]]
    }

    if (y_min < y_max) {
      plt <- plt + ggplot2::coord_cartesian(ylim = c(y_min, y_max))
    }
  }

  plt
}

#' Build the stacked-bar ggplot for Tab 13
#'
#' @import ggplot2
#' @importFrom rlang sym
plot_stacked_bar <- function(
  df,
  group1_var,
  group2_var,
  num_var,
  y_min = NA_real_,
  y_max = NA_real_
) {
  G1Sym <- rlang::sym(group1_var)
  G2Sym <- rlang::sym(group2_var)

  plt <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = !!G1Sym, y = Mean, fill = !!G2Sym)
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      colour = "#333333",
      linewidth = 0.75
    ) +
    scale_fill_general() +
    ggplot2::theme_classic() +
    theme_general() +
    ggplot2::ylab(num_var)

  if (!is.na(y_min) || !is.na(y_max)) {
    stacked_totals <- tapply(
      df$Mean,
      df[[group1_var]],
      sum,
      na.rm = TRUE
    )

    data_limits <- range(c(0, stacked_totals), na.rm = TRUE, finite = TRUE)

    if (is.na(y_min)) {
      y_min <- data_limits[[1]]
    }

    if (is.na(y_max)) {
      y_max <- data_limits[[2]]
    }

    if (y_min < y_max) {
      plt <- plt + ggplot2::coord_cartesian(ylim = c(y_min, y_max))
    }
  }

  plt
}

#' Build the dodge-bar ggplot for Tab 14
#'
#' @import ggplot2
#' @importFrom rlang sym
plot_grouped_dodge_bar <- function(
  df,
  group1_var,
  group2_var,
  num_var,
  y_min = NA_real_,
  y_max = NA_real_
) {
  G1Sym <- rlang::sym(group1_var)
  G2Sym <- rlang::sym(group2_var)

  plt <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = !!G1Sym, y = Mean, fill = !!G2Sym)
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      position = "dodge",
      colour = "#333333",
      linewidth = 0.75
    ) +
    scale_fill_general() +
    ggplot2::theme_classic() +
    theme_general() +
    ggplot2::ylab(num_var)

  if (!is.na(y_min) || !is.na(y_max)) {
    data_limits <- range(df$Mean, na.rm = TRUE, finite = TRUE)

    if (is.na(y_min)) {
      y_min <- data_limits[[1]]
    }

    if (is.na(y_max)) {
      y_max <- data_limits[[2]]
    }

    if (y_min < y_max) {
      plt <- plt + ggplot2::coord_cartesian(ylim = c(y_min, y_max))
    }
  }

  plt
}

#' Build the stacked-area ggplot for Tab 15
#'
#' @import ggplot2
#' @importFrom rlang sym
plot_stacked_area <- function(
  df,
  time_var,
  group_var,
  num_var,
  y_min = NA_real_,
  y_max = NA_real_
) {
  TimeSym <- rlang::sym(time_var)
  GroupSym <- rlang::sym(group_var)

  plt <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = !!TimeSym, y = Mean, fill = !!GroupSym)
  ) +
    ggplot2::geom_area(
      colour = "#333333",
      linewidth = 1,
      show.legend = FALSE
    ) +
    scale_fill_general() +
    ggplot2::theme_classic() +
    theme_general() +
    ggplot2::theme(legend.title = ggplot2::element_blank()) +
    ggplot2::ylab(num_var)

  if (!is.na(y_min) || !is.na(y_max)) {
    stacked_totals <- tapply(
      df$Mean,
      df[[time_var]],
      sum,
      na.rm = TRUE
    )

    data_limits <- range(c(0, stacked_totals), na.rm = TRUE, finite = TRUE)

    if (is.na(y_min)) {
      y_min <- data_limits[[1]]
    }

    if (is.na(y_max)) {
      y_max <- data_limits[[2]]
    }

    if (y_min < y_max) {
      plt <- plt + ggplot2::coord_cartesian(ylim = c(y_min, y_max))
    }
  }

  plt
}

#' Build the proportional-area ggplot for Tab 16
#'
#' @import ggplot2
plot_proportional_area <- function(
  df,
  y_min = NA_real_,
  y_max = NA_real_
) {
  plt <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = Time, y = Percentage, fill = Condition)
  ) +
    ggplot2::geom_area(
      alpha = 0.6,
      linewidth = 1,
      colour = "#333333"
    ) +
    scale_fill_general() +
    ggplot2::theme_classic() +
    theme_general() +
    ggplot2::theme(
      legend.text = ggplot2::element_text(colour = "#333333", size = 8),
      legend.title = ggplot2::element_blank()
    ) +
    ggplot2::ylab("Percentage") +
    ggplot2::labs(fill = NULL)

  if (!is.na(y_min) || !is.na(y_max)) {
    stacked_totals <- tapply(
      df$Percentage,
      df$Time,
      sum,
      na.rm = TRUE
    )

    data_limits <- range(c(0, stacked_totals), na.rm = TRUE, finite = TRUE)

    if (is.na(y_min)) {
      y_min <- data_limits[[1]]
    }

    if (is.na(y_max)) {
      y_max <- data_limits[[2]]
    }

    if (y_min < y_max) {
      plt <- plt + ggplot2::coord_cartesian(ylim = c(y_min, y_max))
    }
  }

  plt
}

#' Build grouped scatterplot with linear trend lines
#'
#' @import ggplot2
#' @importFrom rlang sym as_label
plot_scatter_grouped <- function(
  df,
  group,
  x_var,
  y_var,
  y_min = NA_real_,
  y_max = NA_real_
) {
  G <- rlang::sym(group)
  X <- rlang::sym(x_var)
  Y <- rlang::sym(y_var)

  plt <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = !!X, y = !!Y, colour = !!G)
  ) +
    ggplot2::geom_point(size = 3, alpha = 0.6) +
    ggplot2::geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
    scale_colour_general() +
    ggplot2::theme_bw() +
    theme_general() +
    ggplot2::theme(
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(color = "#333333", size = 8)
    ) +
    ggplot2::xlab(x_var) +
    ggplot2::ylab(y_var) +
    ggplot2::labs(colour = rlang::as_label(G))

  if (!is.na(y_min) || !is.na(y_max)) {
    data_limits <- range(df[[y_var]], na.rm = TRUE, finite = TRUE)

    if (is.na(y_min)) {
      y_min <- data_limits[[1]]
    }

    if (is.na(y_max)) {
      y_max <- data_limits[[2]]
    }

    if (y_min < y_max) {
      plt <- plt + ggplot2::coord_cartesian(ylim = c(y_min, y_max))
    }
  }

  plt
}

#' Build the density ggplot for Tab 17
#'
#' @import ggplot2
#' @importFrom rlang sym
plot_density <- function(df, group_var, num_var) {
  Gsym <- rlang::sym(group_var)
  Nsym <- rlang::sym(num_var)

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = !!Nsym, fill = !!Gsym)
  ) +
    ggplot2::geom_density(alpha = 0.5) +
    scale_fill_general() +
    ggplot2::theme_classic() +
    theme_general() +
    ggplot2::theme(
      legend.text = ggplot2::element_text(colour = "#333333", size = 8),
      legend.title = ggplot2::element_blank()
    ) +
    ggplot2::xlab(num_var) +
    ggplot2::ylab("Density") +
    ggplot2::labs(fill = NULL)
}
