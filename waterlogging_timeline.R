library(dplyr)
library(ggplot2)
library(patchwork)


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
plot.experiment_timeline <- function(object, y_levels = NULL, ...) {
  # Default y axis ordering
  if (is.null(y_levels)) {
    y_levels <- c(
      "Planting", "Inoculation", "Waterlogging", "Growth Assessment", "Canker Assessment"
    )
  } 

  # Enforce factor levels for y positions
  waterlogging_df <- object$waterlogging %>% mutate(y_pos = factor(y_pos, levels = y_levels))
  growth_df       <- object$growth       %>% mutate(y_pos = factor(y_pos, levels = y_levels))
  events_df       <- object$events       %>% mutate(y_pos = factor(y_pos, levels = y_levels))

  # Timeline plot
  ggplot() +
    # Phenology shading
    geom_rect(
      data = object$phenology,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = season),
      alpha = 0.18
    ) +
    # Waterlogging duration bars
    geom_segment(
      data = waterlogging_df,
      aes(x = start, xend = end, y = y_pos, yend = y_pos),
      linewidth = 3, color = "#2b5c8f"
    ) +
    # Growth bars
    geom_segment(
      data = growth_df,
      aes(x = start, xend = end, y = y_pos, yend = y_pos),
      linewidth = 3, color = "#009e73"
    ) +
    # Event points
    geom_point(
      data = events_df,
      aes(x = date, y = y_pos),
      size = 2.5, color = "#d95f02"
    ) +
    # geom_text(
    #   data = events_df, 
    #   aes(x = date, y = y_pos, label = label), 
    #   vjust = -1.2, size = 3, fontface = "bold"
    # ) +
    # Formatting
    scale_fill_manual(values = c(
      "Autumn Transition" = "#e69f00",
      "Winter Dormancy"   = "#56b4e9",
      "Spring Growth"     = "#009e73"
    )) +
    scale_x_date(limits = object$date_range, date_breaks = "3 months", date_labels = "%b %Y") +
    scale_y_discrete(limits = rev(y_levels), drop = FALSE) +
    coord_cartesian(ylim = c(0.5, length(y_levels) + 0.8)) +
    labs(title = object$title, x = NULL, y = NULL, fill = "Phenology") +
    theme_minimal(base_size = 8) +
    theme(
      panel.grid.major.y = element_line(color = "grey90", linetype = "dashed"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}


# Phenology background data
phenology_df <- data.frame(
  xmin = as.Date(c(
    "2022-03-01", "2022-09-01", "2022-12-01",
    "2023-03-01", "2023-09-01", "2023-12-01", 
    "2024-03-01", "2024-09-01", "2024-12-01",
    "2025-03-01", "2025-09-01", "2025-12-01"
  )),
  xmax = as.Date(c(
    "2022-06-01", "2022-12-01", "2023-03-01",
    "2023-06-01", "2023-12-01", "2024-03-01",
    "2024-06-01", "2024-12-01", "2025-03-01",
    "2025-06-01", "2025-12-01", "2026-03-01"
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
    date  = as.Date(c("2022-03-01", "2022-11-01", "2023-10-01", "2024-09-01"))
  ),
  phenology = phenology_df
)

experiment_2 <- experiment_timeline(
  id = 2,
  title = "B) Experiment 2: MM106 — Winter waterlogging duration replicate (2023/2025)",
  date_range = as.Date(c("2023-03-01", "2025-10-31")),
  waterlogging = data.frame(
    y_pos = "Waterlogging",
    start = as.Date(c("2023-12-01", "2024-12-01")),
    end   = as.Date(c("2024-01-30", "2025-01-30"))
  ),
  growth = data.frame(
    y_pos = "Growth Assessment",
    start = as.Date(c("2024-03-01", "2025-03-01")),
    end   = as.Date(c("2024-09-01", "2025-09-01"))
  ),
  events = data.frame(
    y_pos = c("Planting", "Inoculation", "Canker Assessment", "Canker Assessment"),
    date  = as.Date(c("2023-03-01", "2023-12-01", "2024-09-01", "2025-09-01"))
  ),
  phenology = phenology_df
)

experiment_3 <- experiment_timeline(
  id = 3,
  title = "C) Experiment 3: MM106 vs M9 — Seasonal waterlogging (2024/2025)",
  date_range = as.Date(c("2024-03-01", "2025-10-31")),
  waterlogging = data.frame(
    y_pos = "Waterlogging",
    start = as.Date(c("2024-10-01", "2024-12-01", "2025-03-01")),
    end   = as.Date(c("2024-10-31", "2024-12-31", "2025-03-31"))
  ),
  growth = data.frame(
    y_pos = "Growth Assessment",
    start = as.Date("2025-03-01"),
    end   = as.Date("2025-09-01")
  ),
  events = data.frame(
    y_pos = c("Planting", "Inoculation", "Canker Assessment"),
    date  = as.Date(c("2024-03-01", "2024-11-01", "2025-09-01"))
  ),
  phenology = phenology_df
)

experiment_4 <- experiment_timeline(
  id = 4,
  title = "D) Experiment 4: Braeburn/M9 — Waterlogging season and duration (2024/2025)",
  date_range = as.Date(c("2024-03-01", "2025-10-31")),
  waterlogging = data.frame(
    y_pos = "Waterlogging",
    start = as.Date(c("2024-12-01", "2025-03-01")),
    end   = as.Date(c("2025-01-30", "2025-04-30"))
  ),
  growth = data.frame(
    y_pos = "Growth Assessment",
    start = as.Date("2025-03-01"),
    end   = as.Date("2025-09-01")
  ),
  events = data.frame(
    y_pos = c("Planting", "Inoculation", "Canker Assessment"),
    date  = as.Date(c("2024-03-01", "2024-11-01", "2025-09-01"))
  ),
  phenology = phenology_df
)


# Combine timeline plots with patchwork
p1 <- plot(experiment_1)
p2 <- plot(experiment_2)
p3 <- plot(experiment_3)
p4 <- plot(experiment_4)

layout_design_stacked <- "
  AA
  BB
  C#
  D#
"

final_figure <- p1 + p2 + p3 + p4 + 
  plot_layout(design = layout_design_stacked, guides = "collect") & 
  theme(legend.position = "bottom")


# Export figure as png
ggsave("waterlogging_timeline.png", final_figure, width = 20, height = 25, units = "cm", dpi = 300)
