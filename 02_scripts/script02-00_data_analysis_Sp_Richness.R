## --------------------------------------------------------------#
## Script name: script02-00_data_analysis_Sp_Richness.R
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

df <- readRDS("01_data/Efish_processed.rds")

#### Make a combined column of area and year
df <- df %>% 
 unite(AreaYear, Area,Year, sep = "-", remove = FALSE)
#### Make a combined column of Area and TimePeriod
df <- df %>% 
 unite(AreaTP, Area,TimePeriod, sep = "-", remove = FALSE)



### Summary by transect/date/species
SpeciesSumDate <- df %>% dplyr::group_by(Common_Name,YMD,Year, Month, Area, AreaYear, Transect, TimePeriod, doy) %>% summarise(Count =length(Common_Name)) 
write.csv(SpeciesSumDate,"SpeciesSumDate.csv")

### Summary by transect/year/species
SpeciesSumYear <- df %>% dplyr::group_by(Common_Name,Year, Area,AreaYear, Transect) %>% summarise(Count =length(Common_Name)) 
write.csv(SpeciesSumYear,"SpeciesSumYear.csv")

#-------------------------------------------------------------------------------------------------------------------
####Abundance#######

#########################################################################
#### Make Pivot table with abundance by Year/Area as columns #####
#########################################################################

### Summary by transect/year/species
SpeciesSumYearArea <- df %>% dplyr::group_by(Sp_Code, Common_Name,Year, Area, AreaYear) %>% summarise(Count =length(Common_Name)) 
write.csv(SpeciesSumYearArea,"SpeciesSumYearArea.csv")

#### Now for the Pivot table ##########
PivotSpeciesAreaYear <- dcast(SpeciesSumYearArea, Sp_Code+Common_Name ~ AreaYear, value.var = "Count")
PivotSpeciesAreaYear[is.na(PivotSpeciesAreaYear)] <- 0

write.csv(PivotSpeciesAreaYear,"PivotSpeciesAreaYear.csv")

###############################################################################
#### Make Pivot table with abundance by TimePeriod/Area as columns #####
###############################################################################

### Summary by transect/Timeperiod/species
SpeciesSumTPArea <- df %>% dplyr::group_by(Sp_Code, Common_Name,TimePeriod, Area, AreaTP) %>% summarise(Count =length(Common_Name)) 
write.csv(SpeciesSumTPArea,"SpeciesSumTPArea.csv")

#### Now for the Pivot table ##########
PivotSpeciesTP <- dcast(SpeciesSumTPArea, Sp_Code+Common_Name ~ AreaTP, value.var = "Count")
PivotSpeciesTP[is.na(PivotSpeciesTP)] <- 0

write.csv(PivotSpeciesTP,"PivotSpeciesTP.csv") 

#---------------------------------------------------------------------------------------------------------------
####Species Richnesss############

################################
### Mean Species Richness by year ######
################################

TempMeanSpRich <- SpeciesSumDate %>% dplyr::group_by(Transect, Year, YMD, Area, AreaYear) %>% summarise(Count =length(Common_Name)) 
MeanSpRich <- TempMeanSpRich %>% dplyr::group_by(Year, Area, AreaYear) %>% summarise(Mean =mean(Count)) 
write.csv(TempMeanSpRich,"TempMeanSpRich.csv")
MeanSpRich <- TempMeanSpRich %>%
 dplyr::group_by(Year, Area, AreaYear) %>%
 dplyr::summarise(
  Mean = mean(Count),
  SE = sd(Count) / sqrt(dplyr::n()),
  .groups = "drop"
 )

MeanSpRich$Year <- as.numeric(as.character(MeanSpRich$Year))

options(repr.plot.width=8, repr.plot.height=4, repr.plot.res=300)


ggplot(MeanSpRich,
       aes(x = Year,
           y = Mean,
           color = Area)) +
 geom_line(size = .5) +
 geom_point(size = 3) +
 geom_errorbar(aes(ymin = Mean - SE,
                   ymax = Mean + SE),
               width = 0.2,
               linewidth = 0.4) +
 scale_y_continuous(
  breaks = seq(
   from = 0,
   to   = 13,
   by   = 1
  )
 )+
 ggtitle("Mean Species Richness") +
 geom_vline(xintercept = 2021,
            linetype = "dashed",
            color = "grey") +
 labs(y = "Mean Species Richness",
      color = "Area") +
 theme_bw(base_size = 15) +
 theme(axis.text.x = element_text(size = 15,
                                  angle = 45,
                                  hjust = 1),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank())

ggsave("MeanSpRichness.png", width = 8, height = 4, dpi = 300)




#############################################
#### Mean Species Richness by Time Period####
#############################################

TempMeanSpRichTP <- SpeciesSumDate %>% dplyr::group_by(Transect, Year, YMD, Area, AreaYear, TimePeriod, doy) %>% summarise(Count =length(Common_Name)) 
MeanSpRichTP <- TempMeanSpRichTP %>% dplyr::group_by(Area, TimePeriod) %>% summarise(Mean =mean(Count)) 
write.csv(TempMeanSpRichTP,"TempMeanSpRichTP.csv")
MeanSpRichTP <- TempMeanSpRichTP %>%
 dplyr::group_by(Area, TimePeriod) %>%
 dplyr::summarise(
  Mean = mean(Count),
  SE = sd(Count) / sqrt(dplyr::n()),
  .groups = "drop"
 )


# Boxplot (distribution of Count) grouped by Area within TimePeriod


TempMeanSpRichTP_noConst <- TempMeanSpRichTP %>% ### Removes the Construction from TimePeriod
 dplyr::filter(TimePeriod != "Construction")

TempMeanSpRichTP_noConst <- TempMeanSpRichTP_noConst %>%  ### Reorders Pre and Post on the X axis
 dplyr::filter(Area != "Construction") %>%
 dplyr::mutate(TimePeriod = factor(TimePeriod, levels = c("Pre", "Post")))


ggplot(TempMeanSpRichTP_noConst, aes(x = TimePeriod, y = Count, fill = Area)) +
 geom_boxplot(position = position_dodge(width = 0.8), outlier.alpha = 0.4) +
 labs(
  x = "Time Period",
  y = "Species richness (Count)",
  fill = "Area",
  title = "Species Richness by Time Period and Area"
 ) +
 theme_bw() +
 theme(
  axis.text.x = element_text(angle = 45, hjust = 1),
  plot.title = element_text(face = "bold")
 )
ggsave("SpRichBoxPlot.png", width = 8, height = 4, dpi = 300)
#########################################################################################################
####Test difference in Species Richness between Pre and Post (Negative Binomial GLMM - repeated measures)
#########################################################################################################
###Your response (Count = richness) is a count and is usually overdispersed.
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


dat <- TempMeanSpRichTP %>%
 filter(Area != "Construction") %>%
 mutate(
  TimePeriod = factor(TimePeriod, levels = c("Pre","Post")),
  Area = factor(Area),
  Transect = factor(Transect),
  Year = factor(Year),
  doy = yday(as.Date(YMD))
 )

# Final recommended model
m_nb <- glmmTMB(
 Count ~ TimePeriod * Area + ns(doy, df = 4) +
  (1 | Transect) + (1 | Year),
 family = nbinom2(),
 data = dat
)

# Test main effects and interaction
### Note interaction is not significant
m_full <- m_nb
m_noInt <- update(m_full, . ~ . - TimePeriod:Area)
anova(m_full, m_noInt)

### This sets up the dataset so that things line up if NAs are dropped
vars_needed <- c("Count", "TimePeriod", "Area", "doy", "Transect", "Year")
dat_cc <- dat %>%
 dplyr::select(all_of(vars_needed)) %>%
 tidyr::drop_na()  # drops rows with NA in any of these columns

### Can now move on to test the main effects
# Test TimePeriod main effect
# There is an overall Pre-Post difference which is similar across all three areas
m_noInt <- glmmTMB(
 Count ~ TimePeriod + Area + ns(doy, df = 4) + (1|Transect) + (1|Year),
 family = nbinom2(),
 data = dat_cc
)
# Test Area main effect
m_noTP <- glmmTMB(
 Count ~ Area + ns(doy, df = 4) + (1|Transect) + (1|Year),
 family = nbinom2(),
 data = dat_cc
)
anova(m_noInt, m_noTP)


# Area-specific adjusted means and contrasts (richness)
con_rich <- emmeans(m_full, pairwise ~ TimePeriod | Area, type = "response")

# Look at per-area Pre vs Post contrasts (no adjustment first)
con_rich$contrasts

# Apply multiple-comparison adjustment across areas (Holm or Bonferroni)
summary(con_rich$contrasts, adjust = "holm")
# or
summary(con_rich$contrasts, adjust = "bonferroni")

# Overall Pre vs Post effect (averaged across Areas, adjusted for doy)
emm_tp <- emmeans(m_noInt, ~ TimePeriod, type = "response")
emm_tp
pairs(emm_tp)   # Pre vs Post contrast (on response scale)

####Results for contrasts "holm": Piers 5-7 P=0.0574, Construction site P = 0.6145, Macassa P = <0.0001

#### Make Plot of Model ajusted Mean Sp Richness by TimePeriod


# Get model-adjusted means on response scale (back-transformed)
emm_tp <- emmeans(m_noInt, ~ TimePeriod, type = "response")

# Convert to a data frame for ggplot
emm_df <- as.data.frame(emm_tp) %>%
 mutate(
  TimePeriod = factor(TimePeriod, levels = c("Pre", "Post"))
 )


ggplot(emm_df, aes(x = TimePeriod, y = response)) +
 geom_point(size = 3) +
 geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.12, linewidth = 0.8) +
 labs(
  x = "Time Period",
  y = "Model-adjusted mean species richness",
  title = "Species Richness (Adjusted Means ± 95% CI)"
 ) +
 theme_bw() +
 theme(
  plot.title = element_text(face = "bold"),
  axis.title = element_text(face = "bold")
 )
ggsave("SpRichGLMMTimePeriod.png", width = 8, height = 4, dpi = 300)

#### Make Plot of Model ajusted Mean Sp Richness by TimePeriod and Area

# Get adjusted means (back-transformed) for each TimePeriod within each Area
emm_area <- emmeans(m_noInt, ~ TimePeriod | Area, type = "response")

emm_area_df <- as.data.frame(emm_area) %>%
 mutate(
  TimePeriod = factor(TimePeriod, levels = c("Pre", "Post"))
 )

ggplot(emm_area_df, aes(x = TimePeriod, y = response)) +
 geom_point(size = 3) +
 geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
               width = 0.12, linewidth = 0.8) +
 facet_wrap(~ Area) +
 labs(
  x = "Time Period",
  y = "Model-adjusted mean species richness",
  title = "Adjusted Mean Species Richness (±95% CI) by Area and Time Period"
 ) +
 theme_bw() +
 theme(
  plot.title = element_text(face = "bold"),
  axis.title = element_text(face = "bold"),
  strip.text = element_text(face = "bold")
 )
ggsave("SpRichGLMMTimePeriodArea.png", width = 8, height = 4, dpi = 300)

####Same as above with the Spline for DayOfYear #####

#Prep the datatable
dat_cc2 <- dat %>%
 filter(Area != "Construction") %>%
 mutate(
  TimePeriod = factor(TimePeriod, levels = c("Pre", "Post")),
  Area = factor(Area),
  Transect = factor(Transect),
  Year = factor(Year)
 ) %>%
 select(Count, TimePeriod, Area, Transect, Year) %>%
 drop_na()

#Fit the additive model
m_noInt_nodoy <- glmmTMB(
 Count ~ TimePeriod + Area + (1 | Transect) + (1 | Year),
 family = nbinom2(),
 data = dat_cc2
)
#Fit interaction model
m_full_nodoy <- glmmTMB(
 Count ~ TimePeriod * Area + (1 | Transect) + (1 | Year),
 family = nbinom2(),
 data = dat_cc2
)
anova(m_full_nodoy, m_noInt_nodoy)

#Note Pre Post does not differ among areas (interaction not statistically significant)

#Test TimePeriod main effect (overall Pre vs Post)
m_noTP_nodoy <- update(m_noInt_nodoy, . ~ . - TimePeriod)
anova(m_noInt_nodoy, m_noTP_nodoy)

#Test Area main effect (overall area differences)
m_noArea_nodoy <- update(m_noInt_nodoy, . ~ . - Area)
anova(m_noInt_nodoy, m_noArea_nodoy)


emm_tp_nodoy <- emmeans(m_noInt_nodoy, ~ TimePeriod, type = "response")
emm_tp_nodoy
pairs(emm_tp_nodoy)

###Note Pre and Post are significant overall, but ARea is not

#### Plot of Species Richness Model ajusted means stacked by colour

emm_area <- emmeans(m_noInt, ~ TimePeriod | Area, type = "response")
emm_area_df <- as.data.frame(emm_area) %>%
 mutate(
  TimePeriod = factor(TimePeriod, levels = c("Pre", "Post")),
  Area = factor(Area)
 )

pd <- position_dodge(width = 0.45)

ggplot(emm_area_df, aes(x = TimePeriod, y = response, colour = Area, group = Area)) +
 geom_point(position = pd, size = 3) +
 geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
               position = pd, width = 0.12, linewidth = 0.8) +
 labs(
  x = "Time Period",
  y = "Model-adjusted mean species richness",
  colour = "Area",
  title = "Adjusted Mean Species Richness (±95% CI) by Time Period and Area"
 ) +
 theme_bw() +
 theme(
  plot.title = element_text(face = "bold"),
  axis.title = element_text(face = "bold")
 )
ggsave("SpRichGLMMTimePeriodAreaStacked.png", width = 8, height = 4, dpi = 300)

##Plot overall Pre Post without spline

# 1) Get model-adjusted means on response scale (back-transformed)
emm_tp_nodoy <- emmeans(m_noInt_nodoy, ~ TimePeriod, type = "response")

# 2) Convert to a data frame for ggplot
emm_df_nodoy <- as.data.frame(emm_tp_nodoy) %>%
 mutate(TimePeriod = factor(TimePeriod, levels = c("Pre", "Post")))

# 3) Plot: adjusted mean ± 95% CI
ggplot(emm_df_nodoy, aes(x = TimePeriod, y = response)) +
 geom_point(size = 3) +
 geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
               width = 0.12, linewidth = 0.8) +
 labs(
  x = "Time Period",
  y = "Model-adjusted mean species richness",
  title = "Species Richness (Adjusted Means ± 95% CI): Pre vs Post"
 ) +
 theme_bw() +
 theme(
  plot.title = element_text(face = "bold"),
  axis.title = element_text(face = "bold")
 )

# Plot Pre and post by area without the spline


# Adjusted means for Pre vs Post within each Area (back-transformed)
emm_area_nodoy <- emmeans(m_noInt_nodoy, ~ TimePeriod | Area, type = "response")

emm_area_df_nodoy <- as.data.frame(emm_area_nodoy) %>%
  mutate(
    TimePeriod = factor(TimePeriod, levels = c("Pre", "Post")),
    Area = factor(Area)
  )

ggplot(emm_area_df_nodoy, aes(x = TimePeriod, y = response)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.12, linewidth = 0.8) +
  facet_wrap(~ Area) +
  labs(
    x = "Time Period",
    y = "Model-adjusted mean species richness",
    title = "Adjusted Mean Species Richness (±95% CI) by Area and Time Period"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

################################
### Mean Fall Species Richness ######
################################

TempMeanSpRich <- SpeciesSumDate %>% dplyr::group_by(Transect, Year, Month, YMD, Area, AreaYear) %>% summarise(Count =length(Common_Name)) 
TempMeanSpRich <- SpeciesSumDate %>%
 dplyr::filter(Month %in% c(9, 10)) %>%
 dplyr::group_by(Transect, Year, Month, YMD, Area, AreaYear) %>%
 dplyr::summarise(Count = length(Common_Name), .groups = "drop")

MeanSpRich <- TempMeanSpRich %>% dplyr::group_by(Year, Area, AreaYear) %>% summarise(Mean =mean(Count)) 


MeanSpRich <- TempMeanSpRich %>%
 dplyr::group_by(Year, Area, AreaYear) %>%
 dplyr::summarise(
  Mean = mean(Count),
  SE = sd(Count) / sqrt(dplyr::n()),
  .groups = "drop"
 )

MeanSpRich$Year <- as.numeric(as.character(MeanSpRich$Year))

options(repr.plot.width=8, repr.plot.height=4, repr.plot.res=300)


ggplot(MeanSpRich,
       aes(x = Year,
           y = Mean,
           color = Area)) +
 geom_line(size = .5) +
 geom_point(size = 3) +
 geom_errorbar(aes(ymin = Mean - SE,
                   ymax = Mean + SE),
               width = 0.2,
               linewidth = 0.4) +
 scale_y_continuous(
  breaks = seq(
   from = 0,
   to   = 11,
   by   = 1
  )
 )+

 ggtitle("Mean Fall Species Richness") +
 geom_vline(xintercept = 2021,
            linetype = "dashed",
            color = "grey") +
 labs(y = "Mean Species Richness",
      color = "Area") +
 theme_bw(base_size = 15) +
 theme(axis.text.x = element_text(size = 15,
                                  angle = 45,
                                  hjust = 1),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank())

ggsave("MeanFallSpRichness.png", width = 8, height = 4, dpi = 300)

############################
### Total SpRichness Summary by Year/Area ####
#############################
#### Remove Carp x Goldfish hybrid since it is not a species (only found in 2018 in and both Goldfish and Carp were found in the same location and year)
SpeciesSumYearArea <-SpeciesSumYearArea[!(SpeciesSumYearArea$Common_Name=="Carp x Goldfish hybrid"),] ### removes fish code F000

SpeciesRichYearArea <- SpeciesSumYearArea %>% dplyr::group_by(Year, Area, AreaYear) %>% summarise(Count =length(Common_Name)) 
write.csv(SpeciesRichYearArea,"SpeciesRichYearArea.csv")

#### Now for the Pivot table ##########
PivotSpeciesRichAreaYear <- dcast(SpeciesRichYearArea, Area ~ Year, value.var = "Count")
PivotSpeciesRichAreaYear[is.na(PivotSpeciesRichAreaYear)] <- 0

write.csv(PivotSpeciesRichAreaYear,"PivotSpeciesRichAreaYear.csv")

##### Plot Total SpRichness ######
SpeciesRichYearArea$Year <- as.numeric(as.character(SpeciesRichYearArea$Year))

options(repr.plot.width=8, repr.plot.height=4, repr.plot.res=300)


ggplot(SpeciesRichYearArea,
       aes(x = Year,
           y = Count,
           color = Area)) +
 geom_line(size = .5) +
 geom_point(size = 3) +
 ggtitle("Total Species Richness") +
 geom_vline(xintercept = 2021, linetype = "dashed", color = "grey") +
 labs(y = "Total Species Richness", color = "Area") +
 theme_bw(base_size = 15) +
 theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank())
ggsave("SpRichness by area.png", width = 8, height = 4, dpi = 300)

###################################
### Summary by TimePeriod/Area ####
###################################
#### Remove Carp x Goldfish hybrid since it is not a species (only in 2018 in and both Goldfish and Carp were found in the same location and year)
SpeciesSumTPArea <-SpeciesSumTPArea[!(SpeciesSumTPArea$Common_Name=="Carp x Goldfish hybrid"),] ### removes fish code F000
SpeciesRichTPArea <- SpeciesSumTPArea %>% dplyr::group_by(TimePeriod, Area, AreaTP) %>% summarise(Count =length(Common_Name)) 
write.csv(SpeciesRichTPArea,"SpeciesRichTPArea.csv")

#### Now for the Pivot table ##########
PivotSpeciesRichTPArea <- dcast(SpeciesRichTPArea, Area ~ TimePeriod, value.var = "Count")
PivotSpeciesRichTPArea[is.na(PivotSpeciesRichTPArea)] <- 0

write.csv(PivotSpeciesRichTPArea,"PivotSpeciesRichTPArea.csv")

#-----------------------------------------------------------------------------------------------------------------------
