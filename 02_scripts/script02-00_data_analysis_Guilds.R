## --------------------------------------------------------------#
## Script name: script02-00_data_analysis_Guilds.R
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
## Date Created:12Feb2026
##
## --------------------------------------------------------------#
## Modification Notes:
##
## --------------------------------------------------------------#

temp_df <- readRDS("01_data/Efish_processed.rds")
guilds <- read.csv("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/01_data/01_raw_files/SpeciesList.csv")
df <- merge(temp_df, guilds, by = "Common_Name")
#### Make a combined column of area and year
df <- df %>% 
 unite(AreaYear, Area,Year, sep = "-", remove = FALSE)
#### Make a combined column of Area and TimePeriod
df <- df %>% 
 unite(AreaTP, Area,TimePeriod, sep = "-", remove = FALSE)


events <- df %>%
 distinct(YMD, Year, Transect, Area, AreaTP, AreaYear, TimePeriod, doy)

df_counts_guild <- df %>%
 dplyr::filter(Repoductive_guild %in% c("Lithophils"))
df_counts_guild <- df_counts_guild %>%
 group_by(YMD, Year, Transect, Area, AreaTP, AreaYear, TimePeriod, doy) %>%
 reframe(CPUE = sum(Count))  # Using reframe to return ungrouped data

GuildCPUE <- events %>%
 left_join(df_counts_guild, by = c("YMD","Year","Transect","Area","AreaTP","AreaYear","TimePeriod","doy")) %>%
 mutate(CPUE = tidyr::replace_na(CPUE, 0))

##### Plot CPUE by year with error bars ######
mean_abundance_yr_Area <- GuildCPUE %>%
 group_by(Year, Area) %>%
 summarise(
  mean_abundance_per_year_transect = mean(CPUE, na.rm = TRUE),
  sd = sd(CPUE, na.rm = TRUE),
  n = n(),
  se = sd / sqrt(n)
 )

mean_abundance_yr_Area$Year <- as.numeric(as.character(mean_abundance_yr_Area$Year))

ggplot(mean_abundance_yr_Area,
       aes(x = Year,
           y = mean_abundance_per_year_transect,
           color = Area)) +
 geom_line(size = .5) +
 geom_point(size = 3) +
 geom_errorbar(
  aes(
   ymin = mean_abundance_per_year_transect - se,
   ymax = mean_abundance_per_year_transect + se
  ),
  width = 0.2,
  linewidth = 0.5
 ) +
 ggtitle("Mean Lithophils CPUE") +
 geom_vline(xintercept = 2021, linetype = "dashed", color = "grey") +
 labs(y = "Mean CPUE", color = "Area") +
 theme_bw(base_size = 15) +
 theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank())

ggsave("fig_guild CPUE by area with error bars.png", width = 8, height = 4, dpi = 300)


#########################################################################################################
####Test difference in Lithophil CPUE between Pre and Post (Negative Binomial GLMM - repeated measures)
#########################################################################################################
###Your response (CPUE) is a count and is usually overdispersed.
###You have repeated measures (same transects sampled repeatedly) → include Transect as a random effect.
###You want to compare Pre vs Post and see if the change differs among Areas → include TimePeriod * Area.
###Multiple years within each TimePeriod - this allows Year to be modelled as a random effect helping to avoid confounding
###Sampling dates vary widely (May to Oct), creates strong seasonal pattern in richness, a spline for DayOfYear controls for this

###Fixed effects:TimePeriod * Area
###Random effects:(1 | Transect) — repeated measures within each transect
###               (1 | Year) — accounts for multi‑year variation within Pre and Post
###Seasonality control:A smooth spline on day‑of‑year because sampling spans May → October
###Family:Negative binomial (richness is a count and likely overdispersed)
###Note: code written by CoPilot

GuildCPUE2 <- GuildCPUE %>%
 dplyr::filter(TimePeriod %in% c("Pre", "Post"))

### Prep a clean dataset


#pisc_ad <- PiscCPUE2Adult
#pisc_ad$CPUE <- as.integer(pisc_ad$CPUE)

####Fit a negative binomial GLMM (primary model)
### no interaction the pre and post does not differ strongly among areas p=0.093
m_guild_full <- glmmTMB(
 CPUE ~ TimePeriod * Area + ns(doy, df = 4) + (1 | Transect) + (1 | Year),
 family = nbinom2(),
 data = GuildCPUE2
)

# Reduced (no interaction) for the LRT:

m_guild_noInt <- update(m_guild_full, . ~ . - TimePeriod:Area)

# Test whether Pre–Post change differs by Area (interaction)
anova(m_guild_full, m_guild_noInt)


# Test overall TimePeriod effect (Pre vs Post averaged over areas)
m_guild_noTP <- update(m_guild_noInt, . ~ . - TimePeriod)
anova(m_guild_noInt, m_guild_noTP)

# Test overall Area effect (averaged over TimePeriod)
m_guild_noArea <- update(m_guild_noInt, . ~ . - Area)
anova(m_guild_noInt, m_guild_noArea)


con_guild <- emmeans(m_guild_full, pairwise ~ TimePeriod | Area, type = "response")
summary(con_guild$contrasts, adjust = "holm")
emmeans(m_guild_full, ~ TimePeriod, type = "response")


##############################################################
#### Plot Lithophils overall model by area

# Additive model you fit:
# m_guild_noInt <- update(m_guild_full, . ~ . - TimePeriod:Area)

emm_tp <- emmeans(m_guild_noInt, ~ TimePeriod, type = "response")
emm_tp_df <- as.data.frame(emm_tp) %>%
 mutate(TimePeriod = factor(TimePeriod, levels = c("Pre","Post")))

lower_col <- if ("asymp.LCL" %in% names(emm_tp_df)) "asymp.LCL" else "lower.CL"
upper_col <- if ("asymp.UCL" %in% names(emm_tp_df)) "asymp.UCL" else "upper.CL"

p_tp <- ggplot(emm_tp_df, aes(TimePeriod, response)) +
 geom_point(size = 3) +
 geom_errorbar(aes(ymin = .data[[lower_col]], ymax = .data[[upper_col]]),
               width = 0.12, linewidth = 0.8) +
 labs(
  x = "Time Period",
  y = "Model-adjusted mean CPUE (Lithophils)",
  title = "Lithophil CPUE (Adjusted Means ± 95% CI): Pre vs Post"
 ) +
 theme_bw() +
 theme(plot.title = element_text(face = "bold"), axis.title = element_text(face = "bold"))

print(p_tp)
ggsave("fig_lithophil_overall_PrePost.png", p_tp, width = 6, height = 4, dpi = 300)

################
#### Plot Lithophils Model by area and TimePeriod

emm_area <- emmeans(m_guild_full, ~ TimePeriod | Area, type = "response")
emm_area_df <- as.data.frame(emm_area) %>%
 mutate(TimePeriod = factor(TimePeriod, levels = c("Pre","Post")))

lower_col <- if ("asymp.LCL" %in% names(emm_area_df)) "asymp.LCL" else "lower.CL"
upper_col <- if ("asymp.UCL" %in% names(emm_area_df)) "asymp.UCL" else "upper.CL"

p_area <- ggplot(emm_area_df, aes(TimePeriod, response)) +
 geom_point(size = 3) +
 geom_errorbar(aes(ymin = .data[[lower_col]], ymax = .data[[upper_col]]),
               width = 0.12, linewidth = 0.8) +
 facet_wrap(~ Area) +
 labs(
  x = "Time Period",
  y = "Model-adjusted mean CPUE (Lithophils)",
  title = "Lithophil CPUE (Adjusted Means ± 95% CI) by Area"
 ) +
 theme_bw() +
 theme(plot.title = element_text(face = "bold"),
       axis.title = element_text(face = "bold"),
       strip.text = element_text(face = "bold"))

print(p_area)
ggsave("fig_lithophil_byArea_PrePost_facet.png", p_area, width = 8, height = 4.8, dpi = 300)

##################
####Plot

# Overall Area means (averaged over TimePeriod)
emm_area_main <- emmeans(m_guild_noInt, ~ Area, type = "response")
emm_area_main_df <- as.data.frame(emm_area_main)

lower_col <- if ("asymp.LCL" %in% names(emm_area_main_df)) "asymp.LCL" else "lower.CL"
upper_col <- if ("asymp.UCL" %in% names(emm_area_main_df)) "asymp.UCL" else "upper.CL"

p_area_main <- ggplot(emm_area_main_df, aes(Area, response)) +
 geom_point(size = 3) +
 geom_errorbar(aes(ymin = .data[[lower_col]], ymax = .data[[upper_col]]),
               width = 0.2, linewidth = 0.8) +
 labs(
  x = "Area",
  y = "Model-adjusted mean CPUE (Lithophils)",
  title = "Lithophil CPUE (Adjusted Means ± 95% CI) — Overall Area Differences"
 ) +
 theme_bw() +
 theme(plot.title = element_text(face = "bold"),
       axis.title = element_text(face = "bold"))

print(p_area_main)
ggsave("fig_lithophil_area_overall.png", p_area_main, width = 6.5, height = 4, dpi = 300)

#############################
####Plot

# 1) Get adjusted means for each Area × TimePeriod from the interaction model
emm_area <- emmeans(m_guild_full, ~ TimePeriod | Area, type = "response")
emm_area_df <- as.data.frame(emm_area) %>%
 mutate(TimePeriod = factor(TimePeriod, levels = c("Pre","Post")),
        Area = factor(Area))

# 2) Handle CI column names (emmeans may return asymp.LCL/UCL or lower.CL/upper.CL)
lower_col <- if ("asymp.LCL" %in% names(emm_area_df)) "asymp.LCL" else "lower.CL"
upper_col <- if ("asymp.UCL" %in% names(emm_area_df)) "asymp.UCL" else "upper.CL"

# 3) Single-panel grouped plot (Areas as colours), dodged within TimePeriod
pd <- position_dodge(width = 0.45)

p_area_grouped <- ggplot(emm_area_df,
                         aes(x = TimePeriod, y = response,
                             colour = Area, group = Area)) +
 geom_point(position = pd, size = 3) +
 geom_errorbar(aes(ymin = .data[[lower_col]], ymax = .data[[upper_col]]),
               position = pd, width = 0.12, linewidth = 0.8) +
 labs(
  x = "Time Period",
  y = "Model-adjusted mean CPUE (Lithophils)",
  colour = "Area",
  title = "Lithophil CPUE — Adjusted Means (±95% CI) by Time Period and Area"
 ) +
 theme_bw() +
 theme(
  plot.title = element_text(face = "bold"),
  axis.title = element_text(face = "bold")
 )

print(p_area_grouped)
ggsave("fig_lithophil_byArea_PrePost_grouped.png",
       p_area_grouped, width = 7.5, height = 4.5, dpi = 300)
``