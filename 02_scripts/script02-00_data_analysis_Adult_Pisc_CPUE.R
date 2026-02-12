## --------------------------------------------------------------#
## Script name: script02-00_data_analysis_Adult_Pisc_CPUE.R
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

df <- readRDS("01_data/Efish_processed.rds")

#### Make a combined column of area and year
df <- df %>% 
 unite(AreaYear, Area,Year, sep = "-", remove = FALSE)
#### Make a combined column of Area and TimePeriod
df <- df %>% 
 unite(AreaTP, Area,TimePeriod, sep = "-", remove = FALSE)

events <- df %>%
 distinct(YMD, Year, Transect, Area, AreaTP, AreaYear, TimePeriod, doy)


###determine juvenile vs adult piscivores species (ref is Scott and Crossman for all, except OFFLHD for
### Chinook and American eel)
df$adult<-ifelse(df$Common_Name=="Smallmouth bass" & df$Length>165,"Y",
                 ifelse(df$Common_Name=="Largemouth bass" & df$Length>254,"Y",
                        ifelse(df$Common_Name=="Largemouth bass" & df$Length<255,"N",
                               ifelse(df$Common_Name=="Northern pike" & df$Length>305,"Y",
                                      ifelse(df$Common_Name=="Northern pike" & df$Length<306,"N",
                                             ifelse(df$Common_Name=="Bowfin" & df$Length>457,"Y",
                                                    ifelse(df$Common_Name=="Bowfin" & df$Length<458,"N",
                                                           ifelse(df$Common_Name=="Chinook salmon" & df$Length>508,"Y",
                                                                  ifelse(df$Common_Name=="Chinook salmon" & df$Length<509,"N",
                                                                         ifelse(df$Common_Name=="American eel" & df$Length>228,"Y",
                                                                                ifelse(df$Common_Name=="American eel" & df$Length<229,"N",
                                                                                       ifelse( df$Common_Name=="Walleye(yellow pickerel)" & df$Length>305,"Y",
                                                                                               # ifelse( df$Common_Name=="White perch" & df$Length>150,"Y",
                                                                                               #      ifelse( df$Common_Name=="White sucker" & df$Length>336,"Y",
                                                                                               #    ifelse( df$Common_Name=="Gizzard shad" & df$Length>273,"Y",  
                                                                                               ifelse(df$Common_Name=="Smallmouth bass" & df$Length<166,"N",
                                                                                                      ifelse( df$Common_Name=="Walleye(yellow pickerel)" & df$Length<306,"N",
                                                                                                              #  ifelse( df$Common_Name=="White perch" & df$Length<151,"N",
                                                                                                              #     ifelse( df$Common_Name=="White sucker" & df$Length<337,"N",
                                                                                                              #   ifelse(df$Common_Name=="Gizzard shad" & df$Length<274,"N"
                                                                                                              ""))))))))))))))









df_counts <- df %>%
 dplyr::filter(adult %in% c("Y"))
df_counts <- df_counts %>%
 group_by(YMD, Year, Transect, Area, AreaTP, AreaYear, TimePeriod, doy) %>%
 reframe(CPUE = sum(Count))  # Using reframe to return ungrouped data


PiscCPUE2Adult <- events %>%
 left_join(df_counts, by = c("YMD","Year","Transect","Area","AreaTP","AreaYear","TimePeriod","doy")) %>%
 mutate(CPUE = tidyr::replace_na(CPUE, 0))


##### Plot CPUE by year with error bars ######
mean_abundance_yr_Area <- PiscCPUE2Adult %>%
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
 ggtitle("Mean Adult Piscivore CPUE") +
 geom_vline(xintercept = 2021, linetype = "dashed", color = "grey") +
 labs(y = "Mean CPUE", color = "Area") +
 theme_bw(base_size = 15) +
 theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank())

ggsave("Adult Pisc CPUE by area with error bars.png", width = 8, height = 4, dpi = 300)

#########################################################################################################
####Test difference in Adult Piscivore CPUE between Pre and Post (Negative Binomial GLMM - repeated measures)
#########################################################################################################
###Your response (CPUE) is a count and is usually overdispersed.
###You have repeated measures (same transects sampled repeatedly) → include Transect as a random effect.
###You want to compare Pre vs Post and see if the change differs among Areas → include TimePeriod * Area.
###Multiple years within each TimePeriod - this allows Year to be modelled as a random effect helping to avoid confounding
###Sampling dates vary widely (May to Oct), creates strong seasonal pattern in richness, a spline for DayOfYear was tried
###    similar to what was done for SpRichness and CPUE, but the model wouldn't converge so month was use instead

###Fixed effects:TimePeriod * Area
###Random effects:(1 | Transect) — repeated measures within each transect
###               (1 | Year) — accounts for multi‑year variation within Pre and Post
###Seasonality control:A smooth spline on day‑of‑year because sampling spans May → October
###Family:Negative binomial (richness is a count and likely overdispersed)
###Note: code written by CoPilot

pisc_ad <- PiscCPUE2Adult %>%
 dplyr::filter(TimePeriod %in% c("Pre", "Post"))

### Prep a clean dataset


#pisc_ad <- PiscCPUE2Adult
#pisc_ad$CPUE <- as.integer(pisc_ad$CPUE)

####Fit a negative binomial GLMM (primary model)
### no interaction the pre and post does not differ strongly among areas p=0.093
m_pisc_full <- glmmTMB(
 CPUE ~ TimePeriod * Area + ns(doy, df = 4) + (1 | Transect) + (1 | Year),
 family = nbinom2(),
 data = pisc_ad
)

# Reduced (no interaction) for the LRT:
# Note: would not converge
m_pisc_noInt <- update(m_pisc_full, . ~ . - TimePeriod:Area)

# Test whether Pre–Post change differs by Area (interaction)
anova(m_pisc_full, m_pisc_noInt)


# Test overall TimePeriod effect (Pre vs Post averaged over areas)
m_pisc_noTP <- update(m_pisc_noInt, . ~ . - TimePeriod)
anova(m_pisc_noInt, m_pisc_noTP)

# Test overall Area effect (averaged over TimePeriod)
m_pisc_noArea <- update(m_pisc_noInt, . ~ . - Area)
anova(m_pisc_noInt, m_pisc_noArea)


con_pisc <- emmeans(m_pisc_full, pairwise ~ TimePeriod | Area, type = "response")
summary(con_pisc$contrasts, adjust = "holm")
emmeans(m_pisc_full, ~ TimePeriod, type = "response")

### Would not converge with the spline so Try without the spline

m_pisc_full_lin <- glmmTMB(
 CPUE ~ TimePeriod * Area + scale(doy) + (1|Transect) + (1|Year),
 family = nbinom2(),
 data = pisc_ad
)


pisc_ad$Month <- factor(format(as.Date(pisc_ad$YMD), "%m"))

m_pisc_full_m <- glmmTMB(
 CPUE ~ TimePeriod * Area + Month + (1|Transect) + (1|Year),
 family = nbinom2(),
 data = pisc_ad
)

m_pisc_noInt_m <- update(m_pisc_full_m, . ~ . - TimePeriod:Area)

# Test whether Pre–Post change differs by Area (interaction)
anova(m_pisc_full_m, m_pisc_noInt_m)

m_pisc_noInt_m <- update(m_pisc_full_m, . ~ . - TimePeriod:Area)

anova(m_pisc_noInt_m, update(m_pisc_noInt_m, . ~ . - TimePeriod))

anova(m_pisc_noInt_m, update(m_pisc_noInt_m, . ~ . - Area))


con_pisc_m <- emmeans(m_pisc_full_m, pairwise ~ TimePeriod | Area, type = "response")
summary(con_pisc_m$contrasts, adjust = "holm")


emmeans(m_pisc_noInt_m, ~ TimePeriod, type = "response")
pairs(emmeans(m_pisc_noInt_m, ~ TimePeriod), type = "response")

###########################
#### Plot of model outputs

# Estimated marginal means on the response scale (back-transformed)
emm_overall <- emmeans(m_pisc_noInt_m, ~ TimePeriod, type = "response")

# Prepare for plotting (ensure Pre before Post)
emm_overall_df <- as.data.frame(emm_overall) %>%
 mutate(TimePeriod = factor(TimePeriod, levels = c("Pre","Post")))

# Column names for CIs can be 'asymp.LCL/UCL' or 'lower.CL/upper.CL' depending on df
lower_col <- if ("asymp.LCL" %in% names(emm_overall_df)) "asymp.LCL" else "lower.CL"
upper_col <- if ("asymp.UCL" %in% names(emm_overall_df)) "asymp.UCL" else "upper.CL"

p_overall <- ggplot(emm_overall_df, aes(x = TimePeriod, y = response)) +
 geom_point(size = 3) +
 geom_errorbar(aes(ymin = .data[[lower_col]], ymax = .data[[upper_col]]),
               width = 0.12, linewidth = 0.8) +
 labs(
  x = "Time Period",
  y = "Model-adjusted mean CPUE (Adult piscivores)",
  title = "Adult Piscivores — CPUE (Adjusted Means ± 95% CI): Pre vs Post"
 ) +
 theme_bw() +
 theme(
  plot.title = element_text(face = "bold"),
  axis.title = element_text(face = "bold")
 )

print(p_overall)
ggsave("fig_pisc_adult_overall_PrePost.png", p_overall, width = 6, height = 4, dpi = 300)

#### Plot of Pre and post by area

# Area-specific adjusted means from the interaction model
emm_area <- emmeans(m_pisc_full_m, ~ TimePeriod | Area, type = "response")
emm_area_df <- as.data.frame(emm_area) %>%
 mutate(TimePeriod = factor(TimePeriod, levels = c("Pre","Post")))

lower_col <- if ("asymp.LCL" %in% names(emm_area_df)) "asymp.LCL" else "lower.CL"
upper_col <- if ("asymp.UCL" %in% names(emm_area_df)) "asymp.UCL" else "upper.CL"

p_area_facet <- ggplot(emm_area_df, aes(x = TimePeriod, y = response)) +
 geom_point(size = 3) +
 geom_errorbar(aes(ymin = .data[[lower_col]], ymax = .data[[upper_col]]),
               width = 0.12, linewidth = 0.8) +
 facet_wrap(~ Area) +
 labs(
  x = "Time Period",
  y = "Model-adjusted mean CPUE (Adult piscivores)",
  title = "Adult Piscivores — CPUE (Adjusted Means ± 95% CI) by Area"
 ) +
 theme_bw() +
 theme(
  plot.title = element_text(face = "bold"),
  axis.title = element_text(face = "bold"),
  strip.text = element_text(face = "bold")
 )

print(p_area_facet)
ggsave("fig_pisc_adult_byArea_PrePost_facet.png", p_area_facet, width = 8, height = 4.8, dpi = 300)

####Plot single panel with colours

pd <- position_dodge(width = 0.45)

p_area_grouped <- ggplot(emm_area_df, aes(x = TimePeriod, y = response,
                                          colour = Area, group = Area)) +
 geom_point(position = pd, size = 3) +
 geom_errorbar(aes(ymin = .data[[lower_col]], ymax = .data[[upper_col]]),
               position = pd, width = 0.12, linewidth = 0.8) +
 labs(
  x = "Time Period",
  y = "Model-adjusted mean CPUE (Adult piscivores)",
  colour = "Area",
  title = "Adult Piscivores — CPUE (Adjusted Means ± 95% CI) by Time Period and Area"
 ) +
 theme_bw() +
 theme(
  plot.title = element_text(face = "bold"),
  axis.title = element_text(face = "bold")
 )

print(p_area_grouped)
ggsave("fig_pisc_adult_byArea_PrePost_grouped.png", p_area_grouped, width = 7.5, height = 4.5, dpi = 300)










####---------------------------------------------------------------------------------------
##################################################################
#### Box Plot of Adult Piscivore CPUE ######################
######################################################
## ===== Box plot: Adult piscivore CPUE by TimePeriod (Pre/Post) and Area =====

df <- df %>% ### Reorders the data
 dplyr::mutate(TimePeriod = factor(TimePeriod, levels = c("Pre", "Post")))


# How many observations and unique values per group?
PiscCPUE2Adult %>%
 group_by(TimePeriod, Area) %>%
 summarise(
  n_nonmiss = sum(!is.na(CPUE)),
  n_unique  = n_distinct(CPUE[!is.na(CPUE)]),
  min_CPUE  = min(CPUE, na.rm = TRUE),
  max_CPUE  = max(CPUE, na.rm = TRUE),
  .groups = "drop"
 ) %>%
 arrange(Area, TimePeriod)


#Box plot grouped by Area within each TimePeriod
pd <- position_dodge(width = 0.8)

p_box <- ggplot(PiscCPUE2Adult, aes(x = TimePeriod, y = CPUE, fill = Area)) +
 geom_boxplot(position = pd, outlier.alpha = 0.4) +
 # Optional (recommended when n is small): show the raw points too
 
 geom_jitter(
  aes(color = Area),
  position = position_jitterdodge(jitter.width = 0.12, dodge.width = 0.8),
  size = 1.6,
  alpha = 0.5
 )+

 labs(
  x = "Time Period",
  y = "CPUE (Adult piscivores)",
  fill = "Area",
  title = "Adult Piscivores — CPUE by Time Period and Area"
 ) +
 theme_bw() +
 theme(
  axis.text.x = element_text(angle = 45, hjust = 1),
  plot.title  = element_text(face = "bold")
 )

print(p_box)
ggsave("fig_cpue_adult_pisc_box_TimePeriod_Area.png", p_box, width = 8, height = 4.5, dpi = 300)

#---------------------------------------------------------------

PiscCPUE2Adult <- PiscCPUE2Adult %>%  ### Reorders Pre and Post on the X axis
 dplyr::filter(Area != "Construction") %>%
 dplyr::mutate(TimePeriod = factor(TimePeriod, levels = c("Pre", "Post")))


ggplot(pisc_counts, aes(x = TimePeriod, y = CPUE, fill = Area)) +
 geom_boxplot(position = position_dodge(width = 0.8, preserve = "single"), outlier.alpha = 0.4) +
 labs(
  x = "Time Period",
  y = "CPUE",
  fill = "Area",
  title = "CPUE Boxplot"
 ) +
 theme_bw() +
 theme(
  plot.title = element_text(face = "bold"),
  axis.title = element_text(face = "bold")
 )


pd2 <- position_dodge2(width = 0.8, preserve = "single")

ggplot(pisc_counts, aes(
 x = TimePeriod,
 y = CPUE,
 fill = Area,
 group = interaction(TimePeriod, Area)
)) +
 geom_boxplot(position = pd2, outlier.shape = NA, alpha = 0.8) +
 geom_jitter(aes(color = Area),
             position = position_jitterdodge(0.12, 0.8),
             alpha = 0.55, size = 1.6, show.legend = FALSE) +
 labs(
  x = "Time Period",
  y = "CPUE (Adult piscivores)",
  title = "Adult Piscivores — CPUE by Time Period and Area"
 ) +
 theme_bw() +
 theme(axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(face = "bold"))


PiscCPUE2Adult |>
 dplyr::filter(TimePeriod == "Post", Area == "Piers 5-7") |>
 dplyr::summarise(
  n_total = dplyr::n(),
  n_nonmiss = sum(!is.na(CPUE) & is.finite(CPUE)),
  n_dropped = n_total - n_nonmiss
 )


unique(PiscCPUE2Adult$Area)  # look for both "Piers 5-7" and "Piers 5–7"

PiscCPUE2Adult <- PiscCPUE2Adult |>
 dplyr::mutate(
  Area = gsub("\u2013", "-", Area, fixed = TRUE),  # en-dash -> hyphen
  Area = stringr::str_squish(Area)
 ) |>
 droplevels()
``

#--------------------------------------------------------------------------------------------------------------------
