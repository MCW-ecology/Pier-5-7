## --------------------------------------------------------------#
## Script name: script03-00_Figure_Paired.R
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
## Date Created:23Mar2026
##
## --------------------------------------------------------------#
## Modification Notes:
##
## --------------------------------------------------------------#


### Metrics though time dfs
MeanSpRich         <- readRDS("01_data/MeanSpRich.rds")                 # Species Richness
mean_CPUE          <- readRDS("01_data/mean_abundance_yr_Area.rds")     # CPUE (all fish)
mean_CPUE_AdPisc   <- readRDS("01_data/mean_CPUE_AdPisc.rds")           # Adult Piscivores
mean_CPUE_guild    <- readRDS("01_data/mean_CPUE_guild.rds")            # Guild CPUE
mean_CPUE_nonNative<- readRDS("01_data/mean_CPUE_nonNative.rds")        # Non-native CPUE
mean_BPUE          <- readRDS("01_data/mean_BPUE.rds")                  # BPUE

mean_CPUE <- mean_CPUE %>%
 rename(Mean = mean_abundance_per_year_transect)
mean_CPUE$Year <- as.numeric(as.character(mean_CPUE$Year))
mean_CPUE_AdPisc <- mean_CPUE_AdPisc %>%
 rename(Mean = mean_abundance_per_year_transect)

mean_CPUE_guild <- mean_CPUE_guild %>%
 rename(Mean = mean_abundance_per_year_transect)

mean_CPUE_nonNative <- mean_CPUE_nonNative %>%
 rename(Mean = mean_abundance_per_year_transect)

mean_BPUE <- mean_BPUE %>%
 rename(Mean = mean_BPUE)

#### Model means dfs
SpRich_mm           <- readRDS("01_data/SpRichModMeans.rds")

CPUE_mm             <- readRDS("01_data/CPUEModMeans.rds") %>%
 rename(Mean = response, LCL = asymp.LCL, UCL = asymp.UCL)

CPUE_guild_mm       <- readRDS("01_data/GuildModMeans.rds") %>%
 rename(Mean = response, LCL = asymp.LCL, UCL = asymp.UCL)

CPUE_nonNative_mm   <- readRDS("01_data/NonNativeCPUEModMeans.rds")

BPUE_mm             <- readRDS("01_data/BPUEModMeans.rds")


#########################################################################
### Means by Year plots

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
 "Construction Site" = "violetred4",
 "Macassa Bay"       = "steelblue",
 "Piers 5-7"         = "seagreen4"
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

##############################################################################
##### Model adjusted Means by TimePeriod

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



#-----------------------------------------------------------------------

library(patchwork)
library(cowplot)

# ----------------------------------------------------------
# 1) Remove legends from all 4 plots
# ----------------------------------------------------------
p1 <- p_rich      + theme(legend.position = "none")
p2 <- p_rich_mm   + theme(legend.position = "none")
p3 <- p_cpue      + theme(legend.position = "none")
p4 <- p_cpue_mm   + theme(legend.position = "none")

# ----------------------------------------------------------
# 2) Build a TRUE 2×2 layout using wrap_plots()
#    (Prevents wrap_dims errors permanently)
# ----------------------------------------------------------
p_grid <- wrap_plots(
 p1, p2,
 p3, p4,
 ncol = 2, nrow = 2
) +
 plot_layout(guides = "collect") &
 theme(legend.position = "none")   # suppress all legends here

# ----------------------------------------------------------
# 3) Extract a single legend
# ----------------------------------------------------------
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

legend_bottom_mm <- patchwork::wrap_elements(legend_bottom_mm)

# ----------------------------------------------------------
# 4) Stack the grid above the legend
# ----------------------------------------------------------
final_SpRichCPUE <- p_grid /
 legend_bottom_mm +
 plot_layout(heights = c(1, 0.12))

final_SpRichCPUE

ggsave("final_SpRichCPUE.png",
       final_SpRichCPUE, width = 10, height = 10, dpi = 300, bg = "white")