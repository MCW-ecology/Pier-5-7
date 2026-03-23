## --------------------------------------------------------------#
## Script name: script03-00_Figure_CombinedModelAdjustedMeans.R
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
## Date Created:20Mar2026
##
## --------------------------------------------------------------#
## Modification Notes:
##
## --------------------------------------------------------------#

### Species Richness file
SpRich_mm <- readRDS("01_data/SpRichModMeans.rds")

### CPUE file
CPUE_mm <- readRDS("01_data/CPUEModMeans.rds")
CPUE_mm <- CPUE_mm %>%
 rename(
  Mean = response,
  LCL = asymp.LCL,
  UCL = asymp.UCL
 )

#### Adult Piscivore file
# Model would not converge


##### Guild file
CPUE_guild_mm <- readRDS("01_data/GuildModMeans.rds")
CPUE_guild_mm <- CPUE_guild_mm %>%
 rename(
  Mean = response,
  LCL = asymp.LCL,
  UCL = asymp.UCL
 )

##### non-native file
CPUE_nonNative_mm <- readRDS("01_data/NonNativeCPUEModMeans.rds")

##### BPUE file
BPUE_mm <- readRDS("01_data/BPUEModMeans.rds")


# ================================================================
#  Packages
# ================================================================
library(ggplot2)
library(dplyr)
library(readr)
library(patchwork)
library(cowplot)
library(RColorBrewer)

# ================================================================
# 1) Load model-adjusted means datasets
# ================================================================
SpRich_mm           <- readRDS("01_data/SpRichModMeans.rds")

CPUE_mm             <- readRDS("01_data/CPUEModMeans.rds") %>%
 rename(Mean = response, LCL = asymp.LCL, UCL = asymp.UCL)

CPUE_guild_mm       <- readRDS("01_data/GuildModMeans.rds") %>%
 rename(Mean = response, LCL = asymp.LCL, UCL = asymp.UCL)

CPUE_nonNative_mm   <- readRDS("01_data/NonNativeCPUEModMeans.rds")

BPUE_mm             <- readRDS("01_data/BPUEModMeans.rds")


# ================================================================
# 2) Standardize column names & ensure factor order
# ================================================================
standardize_modmeans <- function(df) {
 
 # Standardize names where possible
 nm <- names(df)
 
 # Map common alternatives
 rename_map <- list(
  TimePeriod = c("TimePeriod","Period","period","prepost","PrePost"),
  Area       = c("Area","area","Site","site"),
  Mean       = c("Mean","response","fit","Estimate","predicted"),
  LCL        = c("LCL","asymp.LCL","lower.CL","lwr","lower","Lower"),
  UCL        = c("UCL","asymp.UCL","upper.CL","upr","upper","Upper")
 )
 
 for (std in names(rename_map)) {
  for (cand in rename_map[[std]]) {
   if (cand %in% nm) names(df)[names(df) == cand] <- std
  }
 }
 
 # Ensure columns exist
 if (!"LCL" %in% names(df)) df$LCL <- NA_real_
 if (!"UCL" %in% names(df)) df$UCL <- NA_real_
 
 # Ensure TimePeriod factor ordering
 if ("TimePeriod" %in% names(df)) {
  df$TimePeriod <- factor(df$TimePeriod, levels = c("Pre","Post"))
 }
 
 return(df)
}

# Apply to each dataset
SpRich_mm         <- standardize_modmeans(SpRich_mm)
CPUE_mm           <- standardize_modmeans(CPUE_mm)
CPUE_guild_mm     <- standardize_modmeans(CPUE_guild_mm)
CPUE_nonNative_mm <- standardize_modmeans(CPUE_nonNative_mm)
BPUE_mm           <- standardize_modmeans(BPUE_mm)


# ================================================================
# 3) Area colour palette (reuses your previous one)
# ================================================================
pal <- c(
 "Construction Site" = "violetred4",
 "Macassa Bay"       = "steelblue",
 "Piers 5-7"         = "seagreen4"
)

# Expand palette for any new Areas
areas_all <- Reduce(union, list(
 unique(SpRich_mm$Area),
 unique(CPUE_mm$Area),
 unique(CPUE_guild_mm$Area),
 unique(CPUE_nonNative_mm$Area),
 unique(BPUE_mm$Area)
)) |> as.character()

missing_cols <- setdiff(areas_all, names(pal))
if (length(missing_cols) > 0) {
 extra_cols <- brewer.pal(max(3, length(missing_cols)), "Dark2")[seq_along(missing_cols)]
 names(extra_cols) <- missing_cols
 pal <- c(pal, extra_cols)
}

# Lock factor levels for consistent colours
force_area_levels <- function(df) {
 if ("Area" %in% names(df)) df$Area <- factor(df$Area, levels = names(pal))
 df
}

SpRich_mm         <- force_area_levels(SpRich_mm)
CPUE_mm           <- force_area_levels(CPUE_mm)
CPUE_guild_mm     <- force_area_levels(CPUE_guild_mm)
CPUE_nonNative_mm <- force_area_levels(CPUE_nonNative_mm)
BPUE_mm           <- force_area_levels(BPUE_mm)


# ================================================================
# 4) Plotting function for categorical TimePeriod
# ================================================================
plot_panel_mm_time <- function(dat,
                               y_label,
                               palette = pal,
                               dodge_w = 0.25) {
 
 has_ci <- all(c("LCL","UCL") %in% names(dat)) &&
  !(all(is.na(dat$LCL)) | all(is.na(dat$UCL)))
 
 pd <- position_dodge(width = dodge_w)
 
 p <- ggplot(dat, aes(x = TimePeriod, y = Mean, color = Area, group = Area)) +
  #geom_line(position = pd, linewidth = 0.5) +
  geom_point(position = pd, size = 3) +
  { if (has_ci)
   geom_errorbar(aes(ymin = LCL, ymax = UCL),
                 position = pd, width = 0.2, linewidth = 0.4)
  } +
#  scale_x_discrete(drop = FALSE, limits = c("Pre","Post"),
#                   expand = expansion(mult = c(0.2, 0.2))) +
  scale_color_manual(values = palette, drop = FALSE) +
  labs(y = y_label, x = NULL, color = "Area") +
  theme_bw(base_size = 15) +
  theme(
   axis.text.x      = element_text(size = 15, angle = 45, hjust = 1),
   panel.grid.major = element_blank(),
   panel.grid.minor = element_blank(),
   legend.position  = "bottom",
   legend.title     = element_text(size = 12),
   legend.text      = element_text(size = 11)
  )
 
 p
}


# ================================================================
# 5) Build the five panels
# ================================================================
p_rich_mm  <- plot_panel_mm_time(SpRich_mm,         "Species Richness")
p_cpue_mm  <- plot_panel_mm_time(CPUE_mm,           "CPUE")
p_guild_mm <- plot_panel_mm_time(CPUE_guild_mm,     "Lithophil CPUE")
p_nn_mm    <- plot_panel_mm_time(CPUE_nonNative_mm, "Non-native CPUE")
p_bpue_mm  <- plot_panel_mm_time(BPUE_mm,           "BPUE")


# ================================================================
# 6) Layout: 5 panels + single legend at bottom
# ================================================================
panel_grid_mm <- wrap_plots(
 list(
  p_rich_mm  + theme(legend.position = "none"),
  p_cpue_mm  + theme(legend.position = "none"),
  p_guild_mm + theme(legend.position = "none"),
  p_nn_mm    + theme(legend.position = "none"),
  p_bpue_mm  + theme(legend.position = "none"),
  patchwork::plot_spacer()
 ),
 ncol = 3, nrow = 2
)

legend_bottom_mm <- cowplot::get_legend(
 p_rich_mm +
  theme(
   legend.position = "bottom",
   legend.title    = element_text(size = 18),
   legend.text     = element_text(size = 18),
   legend.key.size = unit(12, "mm")
  ) +
  guides(
   color = guide_legend(
    nrow = 1,
    override.aes = list(size = 8, linewidth = 1.2)
   )
  )
)

final_mm <- panel_grid_mm / patchwork::wrap_elements(legend_bottom_mm) +
 plot_layout(heights = c(1, 0.1))

final_mm

ggsave("ModelAdjusted_5panel_TimePeriod.png",
       final_mm, width = 10, height = 10, dpi = 300, bg = "white")


# ================================================================
# End Script
# ================================================================



