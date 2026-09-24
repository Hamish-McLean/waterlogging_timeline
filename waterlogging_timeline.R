library(dplyr)
library(ggplot2)
library(patchwork)


# 1. Define Phenology Background Data (e.g., recurring seasonal bands)
phenology_df <- data.frame(
  xmin = as.Date(
    c("2022-09-01", "2022-12-01", "2023-03-01", "2023-09-01", "2023-12-01", "2024-03-01")
  ),
  xmax = as.Date(
    c("2022-12-01", "2023-03-01", "2023-06-01", "2023-12-01", "2024-03-01", "2024-06-01")
  ),
  season = factor(
    c(
      "Autumn Transition", "Winter Dormancy", "Spring Growth", 
      "Autumn Transition", "Winter Dormancy", "Spring Growth"
    ),
    levels = c("Autumn Transition", "Winter Dormancy", "Spring Growth"))
)

# 2. Define Treatment Durations (Waterlogging)
waterlog_df <- data.frame(
  start = as.Date(c("2022-12-01", "2024-01-09")),
  end   = as.Date(c("2023-01-26", "2024-02-06")),
  y_pos = "Waterlogging"
)

# 3. Define Single Events (Planting, Inoculation, Assessments)
events_df <- data.frame(
  date  = as.Date(c("2022-03-15", "2022-11-15", "2023-03-15", "2023-10-15", "2024-09-15")),
  y_pos = c("Planting", "Inoculation", "Growth Assessment", "Canker Assessment", "Canker Assessment")
  # event = c("Planting", "Inoculation", "Growth Assessment", "Canker Assessment", "Canker Assessment")
)

# Set factor levels to lock the vertical ordering on the y-axis
y_levels <- c("Planting", "Inoculation", "Waterlogging", "Canker Assessment", "Growth Assessment")
waterlog_df$y_pos <- factor(waterlog_df$y_pos, levels = y_levels)
events_df$y_pos   <- factor(events_df$y_pos, levels = y_levels)

# 4. Build Single Panel Function
build_exp_panel <- function(title_text) {
  ggplot() +
    # Layer 1: Background Phenology Shading
    geom_rect(data = phenology_df, 
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = season), 
              alpha = 0.18) +
    # Layer 2: Waterlogging Duration Bars
    geom_segment(data = waterlog_df, 
                 aes(x = start, xend = end, y = y_pos, yend = y_pos), 
                 linewidth = 3.5, color = "#2b5c8f") +
    # Layer 3: Event Markers
    geom_point(data = events_df, 
               aes(x = date, y = y_pos, shape = y_pos, color = y_pos), 
               size = 3.5) +
    # Formatting & Aesthetics
    scale_fill_manual(values = c("Autumn Transition" = "#e69f00", 
                                 "Winter Dormancy"   = "#56b4e9", 
                                 "Spring Growth"     = "#009e73")) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
    labs(title = title_text, x = NULL, y = NULL, fill = "Phenology", color = "Event", shape = "Event") +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# 5. Combine Panels A–D with patchwork
p1 <- build_exp_panel("A) Exp 1: MM106 — Winter Duration & Repeatability (2022–2024)")
p2 <- build_exp_panel("B) Exp 2: MM106 — Winter Duration Replicate (2023–2025)")

final_figure <- p1 / p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

# 6. Export Vector PDF for Journal Submission
ggsave("waterlogging_timeline.png", final_figure, width = 10, height = 8, units = "in", dpi = 300)
