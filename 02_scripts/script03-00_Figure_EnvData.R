## --------------------------------------------------------------#
## Script name: script03-00_Figure_EnvData.R
##
## Purpose of script:
##    Summaraize and compare efishing data
##    Script naming:
##      - scriptXX-YY format (XX = class 00-99, YY = script 01-99)
##      - higher numbers depend on lower numbers
##      - letters (a,b,c) indicate no dependency between same-numbered scripts
##
## Author: M Croft-White
##
## Date Created:24Mar2026
##
## --------------------------------------------------------------#
## Modification Notes:
##
## --------------------------------------------------------------#


### Metrics through time
temp <- readRDS("01_data/temperature.rds")
cond <- readRDS("01_data/cond.rds")
do   <- readRDS("01_data/DO.rds")

### Raw habitat measurements
hab  <- readRDS("01_data/hab.rds")

library(ggplot2)
library(dplyr)
library(patchwork)
library(cowplot)

# -----------------------------------------
# 1. Colour palette
# -----------------------------------------
pal <- c(
 "Construction Site" = "violetred4",
 "Macassa Bay"       = "steelblue",
 "Piers 5-7"         = "seagreen4"
)

# -----------------------------------------
# 2. Generic plotting function
# -----------------------------------------
plot_habitat_metric <- function(raw_df, mean_df,
                                y_var_raw, y_var_mean,
                                y_label, title_label) {
 
 ggplot() +
  geom_point(
   data = raw_df,
   aes(x = Year, y = .data[[y_var_raw]], color = Area),
   alpha = 0.25, size = 1.5,
   position = position_jitter(width = 0.15, height = 0)
  ) +
  geom_line(
   data = mean_df,
   aes(x = Year, y = .data[[y_var_mean]], color = Area),
   linewidth = 0.6
  ) +
  geom_point(
   data = mean_df,
   aes(x = Year, y = .data[[y_var_mean]], color = Area),
   size = 3
  ) +
  geom_vline(xintercept = 2021,
             linetype = "dashed", color = "grey40", linewidth = 1) +
  
  # ✅ THIS IS THE IMPORTANT FIX:
  scale_color_manual(values = pal, guide = "none") +
  
  labs(
  # title = title_label,
   x = "Year",
   y = y_label
  ) +
  theme_bw(base_size = 15) +
  theme(
   axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
   panel.grid.major = element_blank(),
   panel.grid.minor = element_blank(),
   plot.title = element_text(size = 16, face = "bold")
  )
}


# -----------------------------------------
# 3. Build each panel
# -----------------------------------------

p_temp <- plot_habitat_metric(
 raw_df  = hab,
 mean_df = temp,
 y_var_raw  = "Temperature",
 y_var_mean = "mean_temperature",
 y_label = "Temperature (°C)",
 title_label = "Temperature"
)

p_cond <- plot_habitat_metric(
 raw_df  = hab,
 mean_df = cond,
 y_var_raw  = "Conductivity",
 y_var_mean = "mean_conductivity",
 y_label = "Conductivity (μS/cm)",
 title_label = "Conductivity"
)

p_do <- plot_habitat_metric(
 raw_df  = hab,
 mean_df = do,
 y_var_raw  = "DO",
 y_var_mean = "mean_DO",
 y_label = "Dissolved Oxygen (mg/L)",
 title_label = "Dissolved Oxygen"
)

# -----------------------------------------
# 4. Remove legends properly
# -----------------------------------------
p_temp_nl <- p_temp + guides(color = "none")
p_cond_nl <- p_cond + guides(color = "none")
p_do_nl   <- p_do   + guides(color = "none")

# -----------------------------------------
# 5. Extract shared legend
# -----------------------------------------
legend_bottom_env <- cowplot::get_legend(
 p_do +
  theme(
   legend.position = "bottom",
   legend.title = element_text(size = 18),
   legend.text  = element_text(size = 18),
   legend.key.size = unit(12, "mm")
  ) +
  guides(
   color = guide_legend(
    nrow = 1,
    override.aes = list(size = 8, linewidth = 1.2)
   )
  )
)

legend_bottom_env <- patchwork::wrap_elements(legend_bottom_env)

# -----------------------------------------
# 6. Combine 3 panels vertically
#    ✅ wrap_elements() prevents wrap_dims errors
# -----------------------------------------

panel_grid_env <- wrap_plots(
 p_temp, p_cond, p_do,
 ncol = 1
)


# -----------------------------------------
# 7. Add legend at bottom
# -----------------------------------------

final_env <- panel_grid_env /
 legend_bottom_env +
 plot_layout(heights = c(1,1,1,0.15))

final_env

ggsave("HabitatMetrics_3panel.png",
       final_env,
       width = 10, height = 15, dpi = 300, bg = "white")

