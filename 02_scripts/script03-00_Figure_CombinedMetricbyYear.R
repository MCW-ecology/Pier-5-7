## --------------------------------------------------------------#
## Script name: script03-00_Figure_CombinedMetricbyYear.R
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
## Date Created:13Mar2026
##
## --------------------------------------------------------------#
## Modification Notes:
##
## --------------------------------------------------------------#

### Species Richness file
MeanSpRich <- readRDS("01_data/MeanSpRich.rds")

### CPUE file
mean_CPUE <- readRDS("01_data/mean_abundance_yr_Area.rds")



#### Adult Piscivore file
mean_CPUE_AdPisc <- readRDS("01_data/mean_CPUE_AdPisc.rds")


##### Guild file
mean_CPUE_guild <- readRDS("01_data/mean_CPUE_guild.rds")


##### non-native file
mean_CPUE_nonNative <- readRDS("01_data/mean_CPUE_nonNative.rds")

##### BPUE file
mean_BPUE <- readRDS("01_data/mean_BPUE.rds")

# ------------------------------
# Packages
# ------------------------------
library(ggplot2)
library(dplyr)
library(readr)
library(patchwork)
library(splines)  # not needed for plotting, but loaded elsewhere in your session perhaps

# ------------------------------
# 1) Data: read RDS files
# ------------------------------
MeanSpRich         <- readRDS("01_data/MeanSpRich.rds")                 # Species Richness
mean_CPUE          <- readRDS("01_data/mean_abundance_yr_Area.rds")     # CPUE (all fish)
mean_CPUE_AdPisc   <- readRDS("01_data/mean_CPUE_AdPisc.rds")           # Adult Piscivores
mean_CPUE_guild    <- readRDS("01_data/mean_CPUE_guild.rds")            # Guild CPUE
mean_CPUE_nonNative<- readRDS("01_data/mean_CPUE_nonNative.rds")        # Non-native CPUE
mean_BPUE          <- readRDS("01_data/mean_BPUE.rds")                  # BPUE

mean_CPUE <- mean_CPUE %>%
 rename(
  Mean = mean_abundance_per_year_transect,
 )
mean_CPUE$Year <- as.numeric(as.character(mean_CPUE$Year))
mean_CPUE_AdPisc <- mean_CPUE_AdPisc %>%
 rename(
  Mean = mean_abundance_per_year_transect,
 )

mean_CPUE_guild <- mean_CPUE_guild %>%
 rename(
  Mean = mean_abundance_per_year_transect,
 )

mean_CPUE_nonNative <- mean_CPUE_nonNative %>%
 rename(
  Mean = mean_abundance_per_year_transect,
 )

mean_BPUE <- mean_BPUE %>%
 rename(
  Mean = mean_BPUE,
 )
# ------------------------------
# 2) A helper to standardize column names (only if needed)
#    Call this on any dataset whose column names differ.
# ------------------------------
standardize_cols <- function(df,
                             year = "Year",
                             mean = "Mean",
                             se   = "SE",
                             area = "Area") {
 df %>%
  rename(Year = all_of(year),
         Mean = all_of(mean),
         SE   = all_of(se),
         Area = all_of(area)) %>%
  mutate(
   Area = as.factor(Area),
   Year = as.numeric(as.character(Year))
  )
}

# Example usage if needed:
# mean_CPUE <- standardize_cols(mean_CPUE, year = "year", mean = "mean", se = "se", area = "area")

# ------------------------------
# 3) A reusable plotting function
#    Matches your provided styling exactly, with flexible y-axis settings.
# ------------------------------
plot_panel <- function(dat,
                       y_label,
                       y_breaks = NULL,
                       y_limits = NULL,
                       vline_year = 2021,
                       palette = NULL) {
 
 # if some datasets have missing SE, protect geom_errorbar
 has_se <- "SE" %in% names(dat) && !all(is.na(dat$SE))
 
 p <- ggplot(dat,
             aes(x = Year, y = Mean, color = Area, group = Area)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 3) +
  {if (has_se) geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                             width = 0.2, linewidth = 0.4) } +
  {if (!is.null(y_breaks)) scale_y_continuous(breaks = y_breaks, limits = y_limits) else
   scale_y_continuous(limits = y_limits)} +
  geom_vline(xintercept = vline_year, linetype = "dashed", color = "grey50") +
  labs(y = y_label, color = "Area", x = NULL) +
  theme_bw(base_size = 15) +
  theme(
   axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
   panel.grid.major = element_blank(),
   panel.grid.minor = element_blank(),
   legend.position = "bottom",
   legend.title = element_text(size = 12),
   legend.text  = element_text(size = 11)
  )
 
 if (!is.null(palette)) {
  p <- p + scale_color_manual(values = palette)
 }
 
 p
}

# ------------------------------
# 4) Optional: a consistent color palette across panels
#    Adjust area names & colors to match your data.
# ------------------------------
# Get the set of Areas across datasets (union) to keep colors consistent
areas_all <- Reduce(union, list(
 unique(MeanSpRich$Area),
 unique(mean_CPUE$Area),
 unique(mean_CPUE_AdPisc$Area),
 unique(mean_CPUE_guild$Area),
 unique(mean_CPUE_nonNative$Area),
 unique(mean_BPUE$Area)
))
areas_all <- as.character(areas_all)

# Define a palette (replace with your preferred colors)
pal <- c(
 "Construction Site" = "#0072B2",
 "Macassa Bay"       = "#E69F00",
 "Piers 5-7"         = "#009E73"
)
# If areas_all contains more/different names, expand 'pal' accordingly:
missing_cols <- setdiff(areas_all, names(pal))
if (length(missing_cols) > 0) {
 # Add fallback colors automatically
 extra_cols <- RColorBrewer::brewer.pal(max(3, length(missing_cols)), "Dark2")[seq_along(missing_cols)]
 names(extra_cols) <- missing_cols
 pal <- c(pal, extra_cols)
}

# ------------------------------
# 5) Build each panel
#    Tune y-axis breaks/limits per metric if you want consistent grids.
# ------------------------------

# A) Species Richness (matches your example: 0–13 by 1)
p_rich <- plot_panel(
 MeanSpRich,
 y_label  = "Mean Species Richness",
 y_breaks = seq(0, 13, by = 1),
 y_limits = c(0, 13),
 palette  = pal
)

# B) CPUE (set sensible breaks; adjust to your data range)
range_cpue <- range(mean_CPUE$Mean, na.rm = TRUE)
p_cpue <- plot_panel(
 mean_CPUE,
 y_label  = "Mean CPUE",
 y_breaks = pretty(range_cpue, n = 6),
 y_limits = range(pretty(range_cpue)),
 palette  = pal
)

# C) Adult Piscivore CPUE
range_adp <- range(mean_CPUE_AdPisc$Mean, na.rm = TRUE)
p_adp <- plot_panel(
 mean_CPUE_AdPisc,
 y_label  = "Mean Adult Piscivore CPUE",
 y_breaks = pretty(range_adp, n = 6),
 y_limits = range(pretty(range_adp)),
 palette  = pal
)

# D) Guild CPUE
range_guild <- range(mean_CPUE_guild$Mean, na.rm = TRUE)
p_guild <- plot_panel(
 mean_CPUE_guild,
 y_label  = "Mean Lithophil CPUE",
 y_breaks = pretty(range_guild, n = 6),
 y_limits = range(pretty(range_guild)),
 palette  = pal
)

# E) Non-native CPUE
range_nn <- range(mean_CPUE_nonNative$Mean, na.rm = TRUE)
p_non_native <- plot_panel(
 mean_CPUE_nonNative,
 y_label  = "Mean Non-native CPUE",
 y_breaks = pretty(range_nn, n = 6),
 y_limits = range(pretty(range_nn)),
 palette  = pal
)

# F) BPUE
range_bpue <- range(mean_BPUE$Mean, na.rm = TRUE)
p_bpue <- plot_panel(
 mean_BPUE,
 y_label  = "Mean BPUE",
 y_breaks = pretty(range_bpue, n = 6),
 y_limits = range(pretty(range_bpue)),
 palette  = pal
)



#6) Combine panels (2 x 3 layout) using patchwork
# ------------------------------
# Titles for each panel (optional). If you want internal titles, add + ggtitle("...") to each p_* above.
# Or use patchwork's plot_annotation() for a common title/subtitle/caption.
# 
#install.packages("cowplot")   # if not installed

legend_bottom <- wrap_elements(get_legend(
 p_rich + theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 1))
))

panel_grid <- wrap_plots(
 list(p_rich, p_cpue, p_adp, p_guild, p_non_native, p_bpue),
 ncol = 3, nrow = 2, guides = "collect"
) & theme(legend.position = "bottom")

final <- panel_grid / legend_bottom + plot_layout(heights = c(1, 0.08))
final
ggsave("CPUEbyYear.png", width = 18, height = 10, dpi = 300)


#6) Combine panels (2 x 3 layout) using patchwork
# ------------------------------
# Titles for each panel (optional). If you want internal titles, add + ggtitle("...") to each p_* above.
# Or use patchwork's plot_annotation() for a common title/subtitle/caption.
panel_6 <- (p_rich | p_cpue | p_adp) /
 (p_guild | p_non_native | p_bpue) +
 plot_annotation(
  theme = theme(
   plot.title = element_text(face = "bold", size = 16),
   plot.caption = element_text(size = 10)
  )
 )

# Print to device
panel_6

------------------------------
# 6) Combine panels (2 x 3 layout) using patchwork
# ------------------------------

library(patchwork)

panel_6 <- (p_rich | p_cpue | p_adp) /
 (p_guild | p_non_native | p_bpue) +
 plot_layout(guides = "collect") +                # <-- collect legends from all panels
 plot_annotation(
  theme = theme(
   plot.title   = element_text(face = "bold", size = 16),
   plot.caption = element_text(size = 10)
  )
 ) &
 theme(legend.position = "bottom")                # <-- place the single, collected legend


# Titles for each panel (optional). If you want internal titles, add + ggtitle("...") to each p_* above.
# Or use patchwork's plot_annotation() for a common title/subtitle/caption.

p_rich_l   <- p_rich   + theme(legend.position = "bottom")
p_cpue_n   <- p_cpue   + theme(legend.position = "none")
p_adp_n    <- p_adp    + theme(legend.position = "none")
p_guild_n  <- p_guild  + theme(legend.position = "none")
p_non_nat_n<- p_non_native + theme(legend.position = "none")
p_bpue_n   <- p_bpue   + theme(legend.position = "none")


panel_6 <- (p_rich_1 | p_cpue_n | p_adp_n) /
 (p_guild_n | p_non_native_n | p_bpue_n) +
 plot_annotation(
  theme = theme(
   plot.title = element_text(face = "bold", size = 16),
   plot.caption = element_text(size = 10)
  )
 )

# Print to device
panel_6

# ------------------------------
# 7) Save high-resolution outputs
# ------------------------------
# Width/height in inches; increase dpi for print quality.
ggsave("CPUEbyYear.png", width = 18, height = 10, dpi = 300)
#ggsave("01_data/figure_6panel.png", panel_6, width = 18, height = 10, dpi = 300, bg = "white")
#ggsave("06_figures/figure_6panel.pdf",  panel_6, width = 18, height = 10, device = cairo_pdf)


