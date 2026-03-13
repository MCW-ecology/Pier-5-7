## --------------------------------------------------------------#
## Script name: script02-00_data_analysis_7_non-nativeCPUE.R
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

df <- readRDS("01_data/Efish_processed.rds")
events <- readRDS("01_data/events.rds")

#### Make a combined column of area and year
df <- df %>% 
 unite(AreaYear, Area,Year, sep = "-", remove = FALSE)
#### Make a combined column of Area and TimePeriod
df <- df %>% 
 unite(AreaTP, Area,TimePeriod, sep = "-", remove = FALSE)

#### Summarize the Non-Native CPUE
df_counts <- df %>%
 dplyr::filter(Native %in% c("FALSE"))
df_counts <- df_counts %>%
 group_by(YMD, Year, Transect, Area, AreaTP, AreaYear, TimePeriod, doy) %>%
 reframe(CPUE = sum(Count))  # Using reframe to return ungrouped data

NonNativeCPUE <- events %>%
 left_join(df_counts, by = c("YMD","Year","Transect","Area","AreaYear","TimePeriod", "AreaTP", "doy")) %>%
 mutate(CPUE = tidyr::replace_na(CPUE, 0))
write.csv(NonNativeCPUE,"NonNativeCPUE.csv")

##### Plot NonNativeCPUE by year with error bars ######
mean_abundance_yr_Area <- NonNativeCPUE %>%
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
 #ggtitle("Mean NonNative CPUE") +
 geom_vline(xintercept = 2021, linetype = "dashed", color = "grey") +
 labs(y = "Mean CPUE", color = "Area") +
 theme_bw(base_size = 15) +
 theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank())

ggsave("NonNative CPUE by area with error bars.png", width = 8, height = 4, dpi = 300)

#########################################################################################################
####Test difference in Non-Native CPUE between Pre and Post (Negative Binomial GLMM - repeated measures)
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

NN_ad <- NonNativeCPUE %>%
 dplyr::filter(TimePeriod %in% c("Pre", "Post"))

### Prep a clean dataset


####Fit a negative binomial GLMM (primary model)
### no interaction the pre and post does not differ strongly among areas p=0.093
m_NN_full <- glmmTMB(
 CPUE ~ TimePeriod * Area + ns(doy, df = 4) + (1 | Transect) + (1 | Year),
 family = nbinom2(),
 data = NN_ad
)

# Reduced (no interaction) for the LRT:
# Note: would not converge
m_NN_noInt <- update(m_NN_full, . ~ . - TimePeriod:Area)

# Test whether Pre–Post change differs by Area (interaction)
anova(m_NN_full, m_NN_noInt)


# Test overall TimePeriod effect (Pre vs Post averaged over areas)
#m_NN_noTP <- update(m_NN_noInt, . ~ . - TimePeriod)
#anova(m_NN_noInt, m_NN_noTP)

# Test overall Area effect (averaged over TimePeriod)
#m_NN_noArea <- update(m_NN_noInt, . ~ . - Area)
#anova(m_NN_noInt, m_NN_noArea)


con_NN <- emmeans(m_NN_full, pairwise ~ TimePeriod | Area, type = "response")
summary(con_NN$contrasts, adjust = "holm")
emmeans(m_NN_full, ~ TimePeriod, type = "response")


emm_post <- emmeans(m_NN_full, ~ Area | TimePeriod, type = "response")
summary(pairs(emm_post, by = "TimePeriod"), adjust = "holm")


confint(pairs(emmeans(m_NN_full, ~ TimePeriod | Area, type = "link")))
# (CIs on the log scale; exponentiate to get ratio CIs on the response scale)


#Diagnostics (strongly recommended)
res <- simulateResiduals(m_pisc_full_2)
plot(res)
testDispersion(res)
testZeroInflation(res)

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
