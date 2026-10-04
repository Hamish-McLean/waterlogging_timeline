library(dplyr)
library(ggplot2)
library(patchwork)


okabe_ito <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000"
)


#' Experiment timeline class
experiment_timeline <- function(
  id,
  title,
  date_range,
  waterlogging,
  growth,
  events,
  phenology
) {
  structure(
    list(
      id = id,
      title = title,
      date_range = date_range,
      waterlogging = waterlogging,
      growth = growth,
      events = events,
      phenology = phenology
    ),
    class = "experiment_timeline"
  )
}


#' Experiment timeline plot method
plot.experiment_timeline <- function(
  object, y_levels = NULL, x_limits = NULL, show_x_axis = TRUE, ...
) {
  # Default y axis ordering
  if (is.null(y_levels)) {
    y_levels <- c(
      "Planting", "Inoculation", "Waterlogging", "Growth Assessment", "Canker Assessment"
    )
  }

  if (is.null(x_limits)) {
    x_limits <- object$date_range
  }

  # Filter background phenology data
  phenology <- phenology_df %>% filter(xmax > object$date_range[1] & xmin <= object$date_range[2])

  # Enforce factor levels for y positions
  waterlogging_df <- object$waterlogging %>% mutate(y_pos = factor(y_pos, levels = y_levels))
  growth_df       <- object$growth       %>% mutate(y_pos = factor(y_pos, levels = y_levels))
  events_df       <- object$events       %>% mutate(y_pos = factor(y_pos, levels = y_levels))

  # Palette and shapes
  activity_colours <- c(
    "Planting"          = "#009E73",
    "Inoculation"       = "#CC79A7",
    "Waterlogging"      = "#0072B2",
    "Growth Assessment" = "#009E73",
    "Canker Assessment" = "#D55E00"
  )
  activity_shapes <- c(
    "Planting"          = 17, # Triangle
    "Inoculation"       = 15, # Square
    "Canker Assessment" = 16  # Circle
    # "Waterlogging"      = NA, # No shape for bar
    # "Growth Assessment" = NA  # No shape for bar
  )
  activity_levels <- c(
    "Planting", "Inoculation", "Waterlogging", "Growth Assessment", "Canker Assessment"
  )

  # Timeline plot
  p <- ggplot() +
    # Phenology shading
    geom_rect(
      data = phenology,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = season),
      alpha = 0.18
    ) +
    # Waterlogging duration bars
    geom_segment(
      data = waterlogging_df,
      aes(x = start, xend = end, y = y_pos, yend = y_pos, color = "Waterlogging"),
      linewidth = 3
    ) +
    # Growth bars
    geom_segment(
      data = growth_df,
      aes(x = start, xend = end, y = y_pos, yend = y_pos, color = "Growth Assessment"),
      linewidth = 3
    ) +
    # Event points
    geom_point(
      data = events_df,
      aes(x = date, y = y_pos, color = y_pos, shape = y_pos),
      size = 3
    ) +
    # geom_text(
    #   data = events_df, 
    #   aes(x = date, y = y_pos, label = label), 
    #   vjust = -1.2, size = 3, fontface = "bold"
    # ) +
    # Formatting
    scale_fill_manual(
      name = "Phenological Stage",
      values = c(
        "Autumn Transition" = "#E69F00",
        "Winter Dormancy"   = "#56B4E9",
        "Spring Growth"     = "#009E73"
      )
    ) +
    scale_color_manual(
      name = "Activities & Events",
      values = activity_colours,
      limits = activity_levels
    ) +
    scale_shape_manual(
      name = "Activities & Events",
      values = activity_shapes,
      guide = "none"
    ) +
    scale_x_date(limits = x_limits, date_breaks = "3 months", date_labels = "%b %Y") +
    scale_y_discrete(limits = rev(y_levels), drop = FALSE) +
    coord_cartesian(ylim = c(0.5, length(y_levels) + 0.5)) +
    labs(title = object$title, x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_line(color = "grey90", linetype = "dashed"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
    ) +
    # Custom Legend Overrides (displays point shapes for events & lines for duration bars)
    guides(
      fill = guide_legend(order = 1),
      color = guide_legend(
        title = "Activities & Events",
        order = 2,
        override.aes = list(
          color     = unname(activity_colours),
          shape     = c(17, 15, NA, NA, 16),
          linetype  = c("blank", "blank", "solid", "solid", "blank"),
          linewidth = c(0, 0, 2.5, 2.5, 0)
        )
      )
    )

  # Conditional x-axis formatting
  if (!show_x_axis) {
    p <- p + theme(
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    )
  } else {
    p <- p + theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  }

  return(p)
}


# Global x-axis range spanning all experiments
global_x_limits <- as.Date(c("2022-03-01", "2025-09-30"))


# Phenology background data
phenology_df <- data.frame(
  xmin = as.Date(c(
    "2022-03-01", "2022-09-01", "2022-12-01",
    "2023-03-01", "2023-09-01", "2023-12-01", 
    "2024-03-01", "2024-09-01", "2024-12-01",
    "2025-03-01", "2025-09-01", "2025-12-01"
  )),
  xmax = as.Date(c(
    "2022-09-01", "2022-12-01", "2023-03-01",
    "2023-09-01", "2023-12-01", "2024-03-01",
    "2024-09-01", "2024-12-01", "2025-03-01",
    "2025-09-01", "2025-12-01", "2026-03-01"
  )),
  season = factor(
    rep(c("Spring Growth", "Autumn Transition", "Winter Dormancy"), 4),
    levels = c("Spring Growth", "Autumn Transition", "Winter Dormancy")
  )
)


# Experiment timeline
experiment_1 <- experiment_timeline(
  id = 1,
  title = "A) Experiment 1: MM106 — Winter waterlogging duration (2022/2024)",
  date_range = as.Date(c("2022-03-01", "2024-10-31")),
  waterlogging = data.frame(
    y_pos = "Waterlogging",
    start = as.Date(c("2022-12-01", "2024-01-09")),
    end   = as.Date(c("2023-01-26", "2024-02-06"))
  ),
  growth = data.frame(
    y_pos = "Growth Assessment",
    start = as.Date(c("2023-03-01", "2024-03-01")),
    end   = as.Date(c("2023-09-01", "2024-09-01"))
  ),
  events = data.frame(
    y_pos = c("Planting", "Inoculation", "Canker Assessment", "Canker Assessment"),
    date  = as.Date(c("2022-03-01", "2022-11-09", "2023-10-11", "2024-09-23"))
  ),
  phenology = phenology_df
)

experiment_2 <- experiment_timeline(
  id = 2,
  title = "B) Experiment 2: MM106 — Winter waterlogging duration replicate (2023/2025)",
  date_range = as.Date(c("2023-03-01", "2025-10-31")),
  waterlogging = data.frame(
    y_pos = "Waterlogging",
    start = as.Date(c("2023-12-22", "2024-12-03")),
    end   = as.Date(c("2024-02-16", "2025-01-28"))
  ),
  growth = data.frame(
    y_pos = "Growth Assessment",
    start = as.Date(c("2024-03-01", "2025-03-01")),
    end   = as.Date(c("2024-09-01", "2025-09-01"))
  ),
  events = data.frame(
    y_pos = c("Planting", "Inoculation", "Canker Assessment", "Canker Assessment"),
    date  = as.Date(c("2023-03-01", "2023-12-18", "2024-09-24", "2025-09-01"))
  ),
  phenology = phenology_df
)

experiment_3 <- experiment_timeline(
  id = 3,
  title = "C) Experiment 3: MM106 vs M9 — Seasonal waterlogging (2024/2025)",
  date_range = as.Date(c("2024-03-01", "2025-10-31")),
  waterlogging = data.frame(
    y_pos = "Waterlogging",
    start = as.Date(c("2024-10-04", "2024-12-02", "2025-03-31")),
    end   = as.Date(c("2024-11-07", "2025-01-01", "2025-04-30"))
  ),
  growth = data.frame(
    y_pos = "Growth Assessment",
    start = as.Date("2025-03-01"),
    end   = as.Date("2025-09-01")
  ),
  events = data.frame(
    y_pos = c("Planting", "Inoculation", "Canker Assessment"),
    date  = as.Date(c("2024-03-01", "2024-11-11", "2025-09-08"))
  ),
  phenology = phenology_df
)

experiment_4 <- experiment_timeline(
  id = 4,
  title = "D) Experiment 4: Braeburn/M9 — Waterlogging season and duration (2024/2025)",
  date_range = as.Date(c("2024-03-01", "2025-10-31")),
  waterlogging = data.frame(
    y_pos = "Waterlogging",
    start = as.Date(c("2024-12-02", "2025-03-31")),
    end   = as.Date(c("2025-01-27", "2025-05-26"))
  ),
  growth = data.frame(
    y_pos = "Growth Assessment",
    start = as.Date("2025-03-01"),
    end   = as.Date("2025-09-01")
  ),
  events = data.frame(
    y_pos = c("Planting", "Inoculation", "Canker Assessment"),
    date  = as.Date(c("2024-03-01", "2024-11-25", "2025-09-01"))
  ),
  phenology = phenology_df
)


# Render plots with shared limits and selective x-axis labels
p1 <- plot(experiment_1, x_limits = global_x_limits, show_x_axis = FALSE)
p2 <- plot(experiment_2, x_limits = global_x_limits, show_x_axis = FALSE)
p3 <- plot(experiment_3, x_limits = global_x_limits, show_x_axis = FALSE)
p4 <- plot(experiment_4, x_limits = global_x_limits, show_x_axis = TRUE)


# Stack vertically using patchwork
final_figure <- (p1 / p2 / p3 / p4) + 
  plot_layout(guides = "collect") & 
  theme(
    legend.box           = "vertical",
    legend.box.just      = "left",
    legend.justification = "left",
    legend.key.size      = unit(0.4, "cm"),
    legend.position      = "bottom",
    legend.text          = element_text(size = 8),
    legend.title         = element_text(size = 9,  face = "bold"),
    plot.title           = element_text(size = 10, face = "bold")
  )


# Export figure as png
ggsave(
  "waterlogging_timeline.png", final_figure, width = 20, height = 22, units = "cm", dpi = 300
)
