## --------------------------------------------------------------#
## Script name: script02-00_data_analysis_8_BPUE.R
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
EventsYearArea <- readRDS("01_data/EventsYearArea.rds") #from CPUE script

#### Make a combined column of area and year
df <- df %>% 
 unite(AreaYear, Area,Year, sep = "-", remove = FALSE)
#### Make a combined column of Area and TimePeriod
df <- df %>% 
 unite(AreaTP, Area,TimePeriod, sep = "-", remove = FALSE)


####Summarizing here by YMD, Year and Transect because in some years the transects were sampled more than once
TempBPUE_CommonName <- df %>%
 group_by(YMD, Year, Transect, Area, AreaTP, AreaYear, TimePeriod, Common_Name) %>%
 reframe(BPUE = sum(Weight))  # Using reframe to return ungrouped data


TempBPUE <- events %>%
 left_join(TempBPUE_CommonName, by = c("YMD","Year","Transect","Area","AreaYear","TimePeriod", "AreaTP")) %>%
 mutate(BPUE = tidyr::replace_na(BPUE, 0))
write.csv(TempBPUE,"TempBPUE.csv")
mean_abundance_yr_sp_Area <- TempBPUE %>%
 group_by(Year, Area, Common_Name) %>%
 summarise(mean_abundance_sp_per_year_transect = mean(BPUE, na.rm = TRUE))

mean_abundance_TP_sp_Area <- TempBPUE %>%
 group_by(TimePeriod, Common_Name, Area, AreaTP) %>%
 summarise(mean_abundance_sp_per_year_transect = mean(BPUE, na.rm = TRUE))

#### Some summaries ############
SpBiomass <- df %>%
 group_by(Common_Name) %>%
 summarise(Total_Biomass = sum(Weight), .groups = "drop") %>%
 arrange(desc(Total_Biomass))

SpBiomassArea <- df %>%
 group_by(Common_Name, Area) %>%
 summarise(Total_Biomass = sum(Weight), .groups = "drop") %>%
 arrange(desc(Total_Biomass))
PivotSpBiomassArea <- dcast(SpBiomassArea, Common_Name ~ Area, value.var = "Total_Biomass")
PivotSpBiomassArea[is.na(PivotSpBiomassArea)] <- 0

##### BPUE by time period or year and area######
####Summarizing here by YMD, Year and Transect because in some years the transects were sampled more than once
TempBPUE2 <- df %>%
 group_by(YMD, Year, Transect, Area, AreaTP, AreaYear, TimePeriod, doy) %>%
 reframe(BPUE = sum(Weight))  # Using reframe to return ungrouped data


TempBPUE2 <- events %>%
 left_join(TempBPUE2, by = c("YMD","Year","Transect","Area","AreaYear","TimePeriod", "AreaTP", "doy")) %>%
 mutate(BPUE = tidyr::replace_na(BPUE, 0))
write.csv(TempBPUE2,"TempBPUE2.csv")

mean_biomass_yr_Area <- TempBPUE2 %>%
 group_by(Year, Area) %>%
 summarise(mean_BPUE = mean(BPUE, na.rm = TRUE))

#####################################################
#### BPUE by species table for supplemental #########
#####################################################

### Summary by TimePeriod/Area/species
SpeciesBPUE <- df %>% dplyr::group_by(Sp_Code, Common_Name,Area, Year, AreaYear) %>% summarise(TotalWeight =sum(Weight))


SpeciesBPUE2 <- SpeciesBPUE %>%
 left_join(EventsYearArea, by = c("Year","Area","AreaYear")) %>%
 mutate(TotalWeight = tidyr::replace_na(TotalWeight, 0))

SpeciesBPUE2$BPUE <- SpeciesBPUE2$TotalWeight/SpeciesBPUE2$TotalTransects


SpeciesBPUE3 <- SpeciesBPUE2[c("Sp_Code", "Common_Name", "AreaYear", "BPUE")]
PivotBPUEforSup <- dcast(SpeciesBPUE3, Sp_Code+Common_Name ~ AreaYear, value.var = "BPUE")
PivotBPUEforSup[is.na(PivotBPUEforSup)] <- 0
write.csv(PivotBPUEforSup,"PivotBPUEforSup.csv")

##### Plot BPUE with error bars ######

mean_biomass_yr_Area <- TempBPUE2 %>%
 group_by(Year, Area) %>%
 summarise(
  mean_BPUE = mean(BPUE, na.rm = TRUE),
  sd = sd(BPUE, na.rm = TRUE),
  n = n(),
  se = sd / sqrt(n)
 )

mean_biomass_yr_Area$Year <- as.numeric(as.character(mean_biomass_yr_Area$Year))

options(repr.plot.width=8, repr.plot.height=4, repr.plot.res=300)

ggplot(mean_biomass_yr_Area,
       aes(x = Year,
           y = mean_BPUE,
           color = Area)) +
 geom_line(size = .5) +
 geom_point(size = 3) +
 geom_errorbar(
  aes(
   ymin = mean_BPUE - se,
   ymax = mean_BPUE + se
  ),
  width = 0.2,
  linewidth = 0.5
 ) +
 #ggtitle("Mean BPUE") +
 geom_vline(xintercept = 2021, linetype = "dashed", color = "grey") +
 labs(y = "Mean BPUE", color = "Area") +
 theme_bw(base_size = 15) +
 theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank())

ggsave("BPUE by area with error bars.png", width = 8, height = 4, dpi = 300)

library(dplyr)
library(ggplot2)

# Replace `BPUE_df` with your dataset name if different
# Assumes columns: BPUE, TimePeriod, Area, Transect, Year, doy
sum(is.na(TempBPUE2$BPUE))
prop_zero <- mean(TempBPUE2$BPUE == 0, na.rm = TRUE); prop_zero

ggplot(TempBPUE2, aes(BPUE)) + geom_histogram(bins = 30, color = "white") + theme_bw()
ggplot(TempBPUE2, aes(x = TimePeriod, y = BPUE, color = Area)) + geom_point(alpha = 0.5) + theme_bw()

#### Based on the above tests I'm going with a Gamma GLMM

#########################################################################################################
####Test difference in BPUE between Pre and Post (Tweedie GLMM)
#########################################################################################################
###Your response (BPUE) is a continuous (proportion zero = 0.0319
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

library(dplyr)
library(tidyr)

dat_bpue <- TempBPUE2 %>%
 mutate(
  TimePeriod = factor(TimePeriod, levels = c("Pre", "Post")),
  Area       = factor(Area),
  Transect   = factor(Transect),
  Year       = factor(Year)
 )

vars <- c("BPUE", "TimePeriod", "Area", "doy", "Transect", "Year")
dat_cc <- dat_bpue %>% select(all_of(vars)) %>% drop_na()


### Full Model
m_bpue_full <- glmmTMB(
 BPUE ~ TimePeriod * Area + ns(doy, 4) + (1|Transect) + (1|Year),
 family = tweedie(link = "log"),
 data = dat_cc
)
m_bpue_add <- update(m_bpue_full, . ~ . - TimePeriod:Area)
anova(m_bpue_full, m_bpue_add)   # LRT for interaction


# Test overall TimePeriod effect (Pre vs Post averaged over areas)
m_bpue_noTP <- update(m_bpue_add, . ~ . - TimePeriod)
anova(m_bpue_add, m_bpue_noTP)

# Test overall Area effect (averaged over TimePeriod)
m_bpue_noArea <- update(m_bpue_add, . ~ . - Area)
anova(m_bpue_add, m_bpue_noArea)



# Overall
emm_tp <- emmeans(m_bpue_add, ~ TimePeriod, type = "response")
emm_tp
pairs(emm_tp)             # Pre/Post ratio; tests done on log scale

# Per Area (descriptive; or inferential if interaction significant)
emm_area <- emmeans(m_bpue_add, ~ TimePeriod | Area, type = "response")
emm_area
summary(pairs(emm_area), by = NULL, adjust = "holm")  # Holm across Areas

library(ggplot2)

plot_df <- as.data.frame(emm_area) %>%
 mutate(TimePeriod = factor(TimePeriod, levels = c("Pre", "Post"))) %>%
 rename(Mean = response, LCL = asymp.LCL, UCL = asymp.UCL)

pd <- position_dodge(width = 0.45)

ggplot(plot_df, aes(x = TimePeriod, y = Mean, colour = Area, group = Area)) +
 geom_point(position = pd, size = 3) +
 geom_errorbar(aes(ymin = LCL, ymax = UCL), position = pd, width = 0.12, linewidth = 0.8) +
 labs(x = "Time Period", y = "Model-adjusted mean BPUE", colour = "Area") +
 theme_bw() +
 theme(panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       axis.title = element_text(face = "bold"))

ggsave("BPUEbyAreaModelMeans.png", width = 8, height = 4, dpi = 300)

#### Make a table of model adjusted means ####


# Assuming your final inference model is m_noInt
# (additive TimePeriod + Area + spline + random effects)
emm_area <- emmeans(m_bpue_add, ~ TimePeriod | Area, type = "response")

# Convert to a clean data frame
emm_area_BPUE_df <- as.data.frame(emm_area) %>%
 mutate(
  TimePeriod = factor(TimePeriod, levels = c("Pre", "Post"))
 ) %>%
 rename(
  Mean = response,
  LCL  = asymp.LCL,
  UCL  = asymp.UCL
 ) %>%
 select(Area, TimePeriod, Mean, SE, LCL, UCL)


