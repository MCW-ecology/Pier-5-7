## --------------------------------------------------------------#
## Script name: script02-00_data_analysis_habitat.R
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
## Date Created:21Jan2026
##
## --------------------------------------------------------------#
## Modification Notes:
##
## --------------------------------------------------------------#

df <- readRDS("01_data/temp_hab.rds")


hab <- df %>%
 select(Transect, Area, AreaYear, AreaTP, YMD, MonthYear, Month, Year, doy, TimePeriod, Temperature_0_mid, Conductivity_0_mid,
        Do_0_mid)


hab <- hab %>% rename(Temperature = Temperature_0_mid)
hab <- hab %>% rename(Conductivity = Conductivity_0_mid)
hab <- hab %>% rename(DO = Do_0_mid)

hab$DO <- as.numeric(hab$DO)


temperature_year_area <- hab %>% dplyr::group_by(Year,Area) %>% summarise(mean_temperature =mean(Temperature, na.rm = TRUE))
conductivity_year_area <- hab %>% dplyr::group_by(Year,Area) %>% summarise(mean_conductivity =mean(Conductivity, na.rm = TRUE))
do_year_area <- hab %>% dplyr::group_by(Year,Area) %>% summarise(mean_DO =mean(DO, na.rm = TRUE))




library(ggplot2)
library(dplyr)

# Ensure Year is numeric in both data sets
temperature_year_area$Year <- as.numeric(as.character(temperature_year_area$Year))
hab <- hab %>% mutate(Year = as.numeric(Year))

ggplot() +
 # --- Raw observations in the background ---
 geom_point(
  data = hab,
  aes(x = Year, y = Temperature, color = Area),
  alpha = 0.25, size = 1.5,
  position = position_jitter(width = 0.15, height = 0),  # small horizontal jitter
  show.legend = FALSE    # don't duplicate legend
 ) +
 # --- Mean by Year × Area (your summary) ---
 geom_line(
  data = temperature_year_area,
  aes(x = Year, y = mean_temperature, color = Area),
  linewidth = 0.6
 ) +
 geom_point(
  data = temperature_year_area,
  aes(x = Year, y = mean_temperature, color = Area),
  size = 3
 ) +
 geom_vline(xintercept = 2021, linetype = "dashed", color = "grey", linewidth = 1) +
 labs(
  title = "Temperature",
  x = "Year",
  y = "Temperature (°C)",
  color = "Area"
 ) +
 theme_bw(base_size = 15) +
 theme(
  axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
 )

ggsave("Temperature_withRawPoints.png", width = 8, height = 4, dpi = 300)


##### Conductivity



library(ggplot2)

# Make sure Year is numeric in the summary as well
conductivity_year_area$Year <- as.numeric(as.character(conductivity_year_area$Year))

ggplot() +
 # --- Raw observations in the background (colored by Area) ---
 geom_point(
  data = hab,
  aes(x = Year, y = Conductivity, color = Area),
  alpha = 0.25, size = 1.5,
  position = position_jitter(width = 0.15, height = 0),
  show.legend = FALSE
 ) +
 # --- Year × Area means in the foreground ---
 geom_line(
  data = conductivity_year_area,
  aes(x = Year, y = mean_conductivity, color = Area),
  linewidth = 0.6
 ) +
 geom_point(
  data = conductivity_year_area,
  aes(x = Year, y = mean_conductivity, color = Area),
  size = 3
 ) +
 geom_vline(xintercept = 2021, linetype = "dashed", color = "grey", linewidth = 1) +
 labs(
  title = "Conductivity",
  x = "Year",
  y = "Conductivity (µS/cm)",
  color = "Area"
 ) +
 theme_bw(base_size = 15) +
 theme(
  axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
 )

ggsave("Conductivity_withRawPoints.png", width = 8, height = 4, dpi = 300)


#### DO

library(ggplot2)

# Ensure Year is numeric in the summary as well
do_year_area$Year <- as.numeric(as.character(do_year_area$Year))

ggplot() +
 # --- Raw DO observations in the background ---
 geom_point(
  data = hab,
  aes(x = Year, y = DO, color = Area),
  alpha = 0.25, size = 1.5,
  position = position_jitter(width = 0.15, height = 0),
  show.legend = FALSE
 ) +
 # --- Year × Area means in the foreground ---
 geom_line(
  data = do_year_area,
  aes(x = Year, y = mean_DO, color = Area),
  linewidth = 0.6
 ) +
 geom_point(
  data = do_year_area,
  aes(x = Year, y = mean_DO, color = Area),
  size = 3
 ) +
 geom_vline(xintercept = 2021, linetype = "dashed", color = "grey", linewidth = 1) +
 labs(
  title = "Dissolved Oxygen",
  x = "Year",
  y = "Dissolved Oxygen (mg/L)",
  color = "Area"
 ) +
 theme_bw(base_size = 15) +
 theme(
  axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
 )

ggsave("DO_withRawPoints.png", width = 8, height = 4, dpi = 300)


######################################################################################
######### Test Pre Vs Post for YSI Temperature ###########################################################
####Gaussian Linear Mixed Model (LMM) with the same structure:

#Fixed effects: TimePeriod * Area
#Random effects: (1 | Transect) + (1 | Year) (repeated measures)
#Seasonality control: spline for doy (May–Oct sampling)
#Family: gaussian() (continuous temperature)

# Start from your `hab` frame that already has Temperature as numeric
dat_temp <- hab %>%
 # Optional: filter out a site if you did so in other analyses
# filter(!grepl("^construction", tolower(Area))) %>%
 mutate(
  TimePeriod = factor(TimePeriod, levels = c("Pre", "Post")),
  Area      = factor(Area),
  Transect  = factor(Transect),
  Year      = factor(Year)
 )

# Build complete-case dataset for all variables used
vars_needed <- c("Temperature", "TimePeriod", "Area", "doy", "Transect", "Year")
dat_cc <- dat_temp %>%
 select(all_of(vars_needed)) %>%
 drop_na()


# Full interaction model
m_temp_full <- glmmTMB(
 Temperature ~ TimePeriod * Area + ns(doy, df = 4) + (1|Transect) + (1|Year),
 family = gaussian(),
 data = dat_cc
)

# Additive model (no interaction)
m_temp_noInt <- update(m_temp_full, . ~ . - TimePeriod:Area)

# Likelihood-ratio test for interaction
anova(m_temp_full, m_temp_noInt)

# Test overall TimePeriod effect (Pre vs Post averaged over areas)
m_temp_noTP <- update(m_temp_noInt, . ~ . - TimePeriod)
anova(m_temp_noInt, m_temp_noTP)

# Test overall Area effect (averaged over TimePeriod)
m_temp_noArea <- update(m_temp_noInt, . ~ . - Area)
anova(m_temp_noInt, m_temp_noArea)

library(emmeans)

# Overall adjusted means for Pre vs Post (averaged over Areas)
emm_tp <- emmeans(m_temp_noInt, ~ TimePeriod)   # identity link; response scale
emm_tp
pairs(emm_tp)  # Post - Pre difference, test on Gaussian scale

# Area-wise adjusted means (descriptive context)
emm_area <- emmeans(m_temp_noInt, ~ TimePeriod | Area)
emm_area

# If you really want per-area Pre vs Post tests (adjust p across Areas):
con3 <- pairs(emm_area, by = NULL)     # combine all three contrasts into one family
summary(con3, adjust = "holm")         # Holm is preferred over Bonferroni



##### Diagnostics 

res_temp <- simulateResiduals(m_temp_noInt, n = 1000)
plot(res_temp)             # uniformity, outliers
testDispersion(res_temp)   # variance structure



####PLotss ################


emm_area_df <- as.data.frame(emm_area) %>%
 mutate(TimePeriod = factor(TimePeriod, levels = c("Pre","Post"))) %>%
 rename(
  Mean = emmean,
  LCL  = lower.CL,
  UCL  = upper.CL
 )



emm_area_df <- as.data.frame(emm_area) %>%
 mutate(TimePeriod = factor(TimePeriod, levels = c("Pre","Post"))) %>%
 rename(Mean = emmean, LCL = lower.CL, UCL = upper.CL)

pd <- position_dodge(width = 0.45)

ggplot(emm_area_df, aes(x = TimePeriod, y = Mean, colour = Area, group = Area)) +
 geom_point(position = pd, size = 3) +
 geom_errorbar(aes(ymin = LCL, ymax = UCL),
               position = pd, width = 0.12, linewidth = 0.8) +
 labs(
  x = "Time Period",
  y = "Model-adjusted mean temperature (°C)",
  title = "Adjusted Mean Temperature (±95% CI) by Area and Time Period",
  colour = "Area"
 ) +
 theme_bw() +
 theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.title = element_text(face = "bold"),
  axis.title = element_text(face = "bold")
 )



# Standardize emmeans to columns: Mean, LCL, UCL (works across model types)
clean_emm <- function(emm_obj) {
 df <- as.data.frame(emm_obj)
 
 # Decide which mean column to use
 mean_col <- dplyr::case_when(
  "response" %in% names(df) ~ "response",
  "emmean"   %in% names(df) ~ "emmean",
  TRUE ~ NA_character_
 )
 if (is.na(mean_col)) stop("Could not find 'response' or 'emmean' in emmeans output.")
 
 # Find CI columns (try several possibilities)
 lcl_candidates <- c("lower.CL", "asymp.LCL", "lower.HPD")
 ucl_candidates <- c("upper.CL", "asymp.UCL", "upper.HPD")
 
 lcl_col <- lcl_candidates[lcl_candidates %in% names(df)]
 ucl_col <- ucl_candidates[ucl_candidates %in% names(df)]
 
 # Rename whatever is found
 out <- df %>% rename(Mean = !!mean_col)
 if (length(lcl_col) >= 1) out <- out %>% rename(LCL = !!lcl_col[1])
 if (length(ucl_col) >= 1) out <- out %>% rename(UCL = !!ucl_col[1])
 
 out
}


emm_area_df <- clean_emm(emm_area) %>%
 mutate(TimePeriod = factor(TimePeriod, levels = c("Pre","Post")))
library(ggplot2)
pd <- position_dodge(width = 0.45)

ggplot(emm_area_df, aes(x = TimePeriod, y = Mean, colour = Area, group = Area)) +
 geom_point(position = pd, size = 3) +
 geom_errorbar(
  aes(ymin = LCL, ymax = UCL),
  position = pd, width = 0.12, linewidth = 0.8
 ) +
 labs(
  x = "Time Period",
  y = "Model-adjusted mean temperature (°C)",
  title = "Adjusted Mean Temperature (±95% CI) by Area and Time Period",
  colour = "Area"
 ) +
 theme_bw() +
 theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.title = element_text(face = "bold"),
  plot.title = element_text(face = "bold")
 )
ggsave("Temperature by Area.png", width = 8, height = 4, dpi = 300)
######################################################################################
######### Test Pre Vs Post for YSI Conductivity ######################################
####Gaussian Linear Mixed Model (LMM) with the same structure:

#Fixed effects: TimePeriod * Area
#Random effects: (1 | Transect) + (1 | Year) (repeated measures)
#Seasonality control: spline for doy (May–Oct sampling)
#Family: gaussian() (continuous temperature)
#Note: error with the Gausian model because not enough variance in the data, had to use lmer for
#additive model. Still no significant difference. 

library(dplyr)
library(tidyr)

dat_cond <- hab %>%
 mutate(
  TimePeriod = factor(TimePeriod, levels = c("Pre", "Post")),
  Area       = factor(Area),
  Transect   = factor(Transect),
  Year       = factor(Year)
 )

vars_needed <- c("Conductivity", "TimePeriod", "Area", "doy", "Transect", "Year")

dat_cc <- dat_cond %>%
 select(all_of(vars_needed)) %>%
 drop_na()


# Full interaction model (ML for model comparison)
m_cond_full <- lmer(
 Conductivity ~ TimePeriod * Area + ns(doy, df = 4) + 
  (1 | Transect) + (1 | Year),
 data = dat_cc,
 REML = FALSE
)

# Additive model (no interaction)
m_cond_noInt <- update(m_cond_full, . ~ . - TimePeriod:Area)
anova(m_cond_full, m_cond_noInt)   # Chi-square, df, p-value

# If interaction is NOT significant, continue with m_noInt for main effects tests:

# Test overall TimePeriod effect (drop TimePeriod from additive model)
m_cond_noTP <- update(m_cond_noInt, . ~ . - TimePeriod)
anova(m_cond_noInt, m_cond_noTP)   # LRT for TimePeriod

# Test overall Area effect (drop Area from additive model)
m_cond_noArea <- update(m_cond_noInt, . ~ . - Area)
anova(m_cond_noInt, m_cond_noArea) # LRT for Area

model_use <- m_cond_noInt   # <-- change to m_do_noInt_lmer if using lmer

# Overall Pre vs Post (averaged over Areas)
emm_tp <- emmeans(model_use, ~ TimePeriod)
emm_tp
pairs(emm_tp)   # Post - Pre, Gaussian test

# Area-wise adjusted means (descriptive context)
emm_area <- emmeans(model_use, ~ TimePeriod | Area)
emm_area

# If you want per-area Pre vs Post tests with Holm across Areas:
con_area <- pairs(emm_area)                        # Pre vs Post within each Area
summary(con_area, by = NULL, adjust = "holm")      # Holm across the 3 contrasts

######################################################################################
######### Test Pre Vs Post for YSI DO ######################################
####Gaussian Linear Mixed Model (LMM) with the same structure:

#Fixed effects: TimePeriod * Area
#Random effects: (1 | Transect) + (1 | Year) (repeated measures)
#Seasonality control: spline for doy (May–Oct sampling)
#Family: gaussian() (continuous temperature)
#Note: error with the Gausian model because not enough variance in the data, had to use lmer for
#additive model. Still no significant difference. 

library(dplyr)
library(tidyr)

dat_do <- hab %>%
 mutate(
  TimePeriod = factor(TimePeriod, levels = c("Pre", "Post")),
  Area       = factor(Area),
  Transect   = factor(Transect),
  Year       = factor(Year)
 )

vars_needed <- c("DO", "TimePeriod", "Area", "doy", "Transect", "Year")

dat_cc <- dat_do %>%
 select(all_of(vars_needed)) %>%
 drop_na()

library(glmmTMB)
library(splines)

m_do_full <- glmmTMB(
 DO ~ TimePeriod * Area + ns(doy, df = 4) + (1|Transect) + (1|Year),
 family = gaussian(),
 data = dat_cc
)

m_do_noInt <- update(m_do_full, . ~ . - TimePeriod:Area)
anova(m_do_full, m_do_noInt)  # LRT for interaction

# Using glmmTMB:
m_do_noTP   <- update(m_do_noInt, . ~ . - TimePeriod)
anova(m_do_noInt, m_do_noTP)   # overall Pre vs Post

m_do_noArea <- update(m_do_noInt, . ~ . - Area)
anova(m_do_noInt, m_do_noArea) # overall Area differences

# Using lmer (REML for inference; for main effects, you can also use Type III tests from lmerTest)
#anova(m_do_noInt_lmer)  # Satterthwaite p-values per fixed effect (lmerTest)

library(emmeans)

# Pick the fitted additive model object you are using:
#   - For glmmTMB:   model_use <- m_do_noInt
#   - For lmer:      model_use <- m_do_noInt_lmer
model_use <- m_do_noInt   # <-- change to m_do_noInt_lmer if using lmer

# Overall Pre vs Post (averaged over Areas)
emm_tp <- emmeans(model_use, ~ TimePeriod)
emm_tp
pairs(emm_tp)   # Post - Pre, Gaussian test

# Area-wise adjusted means (descriptive context)
emm_area <- emmeans(model_use, ~ TimePeriod | Area)
emm_area

# If you want per-area Pre vs Post tests with Holm across Areas:
con_area <- pairs(emm_area)                        # Pre vs Post within each Area
summary(con_area, by = NULL, adjust = "holm")      # Holm across the 3 contrasts

library(dplyr)
library(ggplot2)

emm_area_df <- as.data.frame(emm_area) %>%
 mutate(TimePeriod = factor(TimePeriod, levels = c("Pre","Post"))) %>%
 rename(Mean = emmean, LCL = asymp.LCL, UCL = asymp.UCL)

pd <- position_dodge(width = 0.45)

ggplot(emm_area_df, aes(x = TimePeriod, y = Mean, colour = Area, group = Area)) +
 geom_point(position = pd, size = 3) +
 geom_errorbar(aes(ymin = LCL, ymax = UCL), position = pd, width = 0.12, linewidth = 0.8) +
 labs(
  x = "Time Period",
  y = "Model-adjusted mean DO (mg/L)",
  title = "Adjusted Mean Dissolved Oxygen (±95% CI) by Area and Time Period",
  colour = "Area"
 ) +
 theme_bw() +
 theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.title = element_text(face = "bold"),
  plot.title = element_text(face = "bold")
 )
ggsave("DO by Area.png", width = 8, height = 4, dpi = 300)