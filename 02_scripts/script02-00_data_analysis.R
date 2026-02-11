## --------------------------------------------------------------#
## Script name: script02-00_data_analysis.R
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
df <- df %>% ### Makes a column with combined name for guilds
 unite(AreaYear, Area,Year, sep = "-", remove = FALSE)
#### Make a combined column of Area and TimePeriod
df <- df %>% ### Makes a column with combined name for guilds
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
###############
#### CPUE #####
###############

##################################################
####Number transects sampled for CPUE ############
##################################################

### Summary by TimePeriod/Area/species/Transect
SpeciesSumTP <- df %>% dplyr::group_by(Common_Name,TimePeriod,Transect, Area) %>% summarise(Count =length(Common_Name))
write.csv(SpeciesSumTP,"SpeciesSumTPTransect.csv")

### Summary by TimePeriod/Area/species
SpeciesSumTP <- df %>% dplyr::group_by(Common_Name,TimePeriod, Area) %>% summarise(Count =length(Common_Name))
write.csv(SpeciesSumTP,"SpeciesSumTP.csv")

### Summary by transect/date/species
TransectSumDate <- df %>% dplyr::group_by(YMD,Year, TimePeriod, Area,Transect) %>% summarise(Count =length(Common_Name))
TransectSumDate2 <- TransectSumDate %>% dplyr::group_by(Year, Area,TimePeriod,Transect) %>% summarise(Count =length(Transect))
TransectSumDate3 <- TransectSumDate2 %>% dplyr::group_by(Area,TimePeriod) %>% summarise(CountofTransects =sum(Count))
TransectSumDate4 <- TransectSumDate2 %>% dplyr::group_by(Year,Area,Transect) %>% summarise(CountofTransects =sum(Count))
write.csv(TransectSumDate,"TransectSumDate.csv")

#### need to "pivot table" 
TransectSumDate4 <- as.data.table(TransectSumDate4)
PivotTransects <- dcast(TransectSumDate4, Transect+Area ~ Year, value.var = "CountofTransects")
write.csv(PivotTransects,"PivotTransects.csv")

##################################################
##### CPUE by species year, time period area######
##################################################

####Summarizing here by YMD, Year and Transect because in some years the transects were sampled more than once
TempCPUE <- df %>%
 group_by(YMD, Year, Transect, Area, AreaTP, AreaYear, TimePeriod, Common_Name) %>%
 reframe(CPUE = sum(Count))  # Using reframe to return ungrouped data
write.csv(TempCPUE,"TempCPUE.csv")

mean_abundance_yr_sp_Area <- TempCPUE %>%
 group_by(Year, Area, Common_Name) %>%
 summarise(mean_abundance_sp_per_year_transect = mean(CPUE, na.rm = TRUE))

mean_abundance_TP_sp_Area <- TempCPUE %>%
 group_by(TimePeriod, Common_Name, Area, AreaTP) %>%
 summarise(mean_abundance_sp_per_year_transect = mean(CPUE, na.rm = TRUE))

##### CPUE by time period or year and area######
####Summarizing here by YMD, Year and Transect because in some years the transects were sampled more than once
TempCPUE2 <- df %>%
 group_by(YMD, Year, Transect, Area, AreaTP, AreaYear, TimePeriod, doy) %>%
 reframe(CPUE = sum(Count))  # Using reframe to return ungrouped data
write.csv(TempCPUE2,"TempCPUE2.csv")

mean_abundance_yr_Area <- TempCPUE2 %>%
 group_by(Year, Area) %>%
 summarise(mean_abundance_per_year_transect = mean(CPUE, na.rm = TRUE))

##### same as above but with error bars #####
mean_abundance_yr_Area <- TempCPUE2 %>%
 group_by(Year, Area) %>%
 summarise(
  mean_abundance_per_year_transect = mean(CPUE, na.rm = TRUE),
  sd = sd(CPUE, na.rm = TRUE),
  n = n(),
  se = sd / sqrt(n)
 )


mean_abundance_TP_Area <- TempCPUE2 %>%
 group_by(TimePeriod, Area, AreaTP) %>%
 summarise(mean_abundance_per_year_transect = mean(CPUE, na.rm = TRUE))

##### Plot CPUE ######
mean_abundance_yr_Area$Year <- as.numeric(as.character(mean_abundance_yr_Area$Year))

options(repr.plot.width=8, repr.plot.height=4, repr.plot.res=300)


ggplot(mean_abundance_yr_Area,
       aes(x = Year,
           y = mean_abundance_per_year_transect,
           color = Area)) +
 geom_line(size = .5) +
 geom_point(size = 3) +
 ggtitle("Mean CPUE") +
 geom_vline(xintercept = 2021, linetype = "dashed", color = "grey") +
 labs(y = "Mean CPUE", color = "Area") +
 theme_bw(base_size = 15) +
 theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank())
ggsave("CPUE by area.png", width = 8, height = 4, dpi = 300)

mean_abundance_yr_Area$Year <- as.numeric(as.character(mean_abundance_yr_Area$Year))

##### Plot CPUE with error bars ######
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
 ggtitle("Mean CPUE") +
 geom_vline(xintercept = 2021, linetype = "dashed", color = "grey") +
 labs(y = "Mean CPUE", color = "Area") +
 theme_bw(base_size = 15) +
 theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank())

ggsave("CPUE by area with error bars.png", width = 8, height = 4, dpi = 300)


##################################################
##### Piscivore CPUE by species year, time period area######
##################################################

df_pisc <- df %>% 
 filter(Piscivore == TRUE)
write.csv(df_pisc,"df_pisc.csv")

CommonNamePisc <- df_pisc %>% dplyr::group_by(Common_Name, Length) %>% summarise(Count =length(Common_Name)) 



####Summarizing here by YMD, Year and Transect because in some years the transects were sampled more than once
PiscCPUE <- df_pisc %>%
 group_by(YMD, Year, Transect, Area, AreaTP, AreaYear, TimePeriod, Common_Name) %>%
 reframe(CPUE = sum(Count))  # Using reframe to return ungrouped data
write.csv(PiscCPUE,"PiscCPUE.csv")

Pisc_mean_abundance_yr_sp_Area <- PiscCPUE %>%
 group_by(Year, Area, Common_Name) %>%
 summarise(mean_abundance_sp_per_year_transect = mean(CPUE, na.rm = TRUE))

Pisc_mean_abundance_TP_sp_Area <- PiscCPUE %>%
 group_by(TimePeriod, Common_Name, Area, AreaTP) %>%
 summarise(mean_abundance_sp_per_year_transect = mean(CPUE, na.rm = TRUE))

##### CPUE by time period or year and area######
####Summarizing here by YMD, Year and Transect because in some years the transects were sampled more than once
PiscCPUE2 <- df_pisc %>%
 group_by(YMD, Year, Transect, Area, AreaTP, AreaYear, TimePeriod) %>%
 reframe(CPUE = sum(Count))  # Using reframe to return ungrouped data
write.csv(PiscCPUE2,"PiscCPUE2.csv")


Pisc_mean_abundance_yr_Area <- PiscCPUE2 %>%
 group_by(Year, Area) %>%
 summarise(mean_abundance_per_year_transect = mean(CPUE, na.rm = TRUE))

#### Same as above but with error bars #####
Pisc_mean_abundance_yr_Area <- PiscCPUE2 %>%
 group_by(Year, Area) %>%
 summarise(
  mean_abundance_per_year_transect = mean(CPUE, na.rm = TRUE),
  sd = sd(CPUE, na.rm = TRUE),
  n = n(),
  se = sd / sqrt(n)
 )

Pisc_mean_abundance_TP_Area <- PiscCPUE2 %>%
 group_by(TimePeriod, Area, AreaTP) %>%
 summarise(mean_abundance_per_year_transect = mean(CPUE, na.rm = TRUE))

##### Plot CPUE ######
Pisc_mean_abundance_yr_Area$Year <- as.numeric(as.character(Pisc_mean_abundance_yr_Area$Year))

options(repr.plot.width=8, repr.plot.height=4, repr.plot.res=300)


ggplot(Pisc_mean_abundance_yr_Area,
       aes(x = Year,
           y = mean_abundance_per_year_transect,
           color = Area)) +
 geom_line(size = .5) +
 geom_point(size = 3) +
 ggtitle("Piscivore Mean CPUE") +
 geom_vline(xintercept = 2021, linetype = "dashed", color = "grey") +
 labs(y = "Mean CPUE", color = "Area") +
 theme_bw(base_size = 15) +
 theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank())
ggsave("Pisc CPUE by area.png", width = 8, height = 4, dpi = 300)

#### Plot Pisc CPUE with error bars #####
Pisc_mean_abundance_yr_Area$Year <- as.numeric(as.character(Pisc_mean_abundance_yr_Area$Year))
ggplot(Pisc_mean_abundance_yr_Area,
       aes(x = Year,
           y = mean_abundance_per_year_transect,
           color = Area)) +
 geom_line(size = .5) +
 geom_point(size = 3) +
 geom_errorbar(aes(ymin = mean_abundance_per_year_transect - se,
                   ymax = mean_abundance_per_year_transect + se),
               width = 0.2, linewidth = 0.5) +
 ggtitle("Mean Piscivore CPUE") +
 geom_vline(xintercept = 2021, linetype = "dashed", color = "grey") +
 labs(y = "Mean CPUE", color = "Area") +
 theme_bw(base_size = 15) +
 theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank())
ggsave("Pisc CPUE by area with error bars.png", width = 8, height = 4, dpi = 300)

##########################
### Adult Piscivores #####
##########################

###determine juvenile vs adult piscivores or lithophilic species (ref is Scott and Crossman for all, except OFFLHD for
### Chinook and American eel)
df$adult<-ifelse(df_pisc$Common_Name=="Smallmouth bass" & df_pisc$Length>165,"Y",
                      ifelse(df_pisc$Common_Name=="Largemouth bass" & df_pisc$Length>254,"Y",
                             ifelse(df_pisc$Common_Name=="Largemouth bass" & df_pisc$Length<255,"N",
                                    ifelse(df_pisc$Common_Name=="Northern pike" & df_pisc$Length>305,"Y",
                                           ifelse(df_pisc$Common_Name=="Northern pike" & df_pisc$Length<306,"N",
                                                  ifelse(df_pisc$Common_Name=="Bowfin" & df_pisc$Length>457,"Y",
                                                         ifelse(df_pisc$Common_Name=="Bowfin" & df_pisc$Length<458,"N",
                                                                ifelse(df_pisc$Common_Name=="Chinook salmon" & df_pisc$Length>508,"Y",
                                                                       ifelse(df_pisc$Common_Name=="Chinook salmon" & df_pisc$Length<509,"N",
                                                                              ifelse(df_pisc$Common_Name=="American eel" & df_pisc$Length>228,"Y",
                                                                                     ifelse(df_pisc$Common_Name=="American eel" & df_pisc$Length<229,"N",
                                                                                            ifelse( df_pisc$Common_Name=="Walleye(yellow pickerel)" & df_pisc$Length>305,"Y",
                                                                                                    # ifelse( df_pisc$Common_Name=="White perch" & df_pisc$Length>150,"Y",
                                                                                                    #      ifelse( df_pisc$Common_Name=="White sucker" & df_pisc$Length>336,"Y",
                                                                                                    #    ifelse( df_pisc$Common_Name=="Gizzard shad" & df_pisc$Length>273,"Y",  
                                                                                                    ifelse(df_pisc$Common_Name=="Smallmouth bass" & df_pisc$Length<166,"N",
                                                                                                           ifelse( df_pisc$Common_Name=="Walleye(yellow pickerel)" & df_pisc$Length<306,"N",
                                                                                                                   #  ifelse( df_pisc$Common_Name=="White perch" & df_pisc$Length<151,"N",
                                                                                                                   #     ifelse( df_pisc$Common_Name=="White sucker" & df_pisc$Length<337,"N",
                                                                                                                   #   ifelse(df_pisc$Common_Name=="Gizzard shad" & df_pisc$Length<274,"N"
                                                                                                                   ""))))))))))))))


#### Selecting just the adult piscivores######
df_piscAdult <- df%>% 
 filter(adult == "Y")
write.csv(df_piscAdult,"df_piscAdult.csv")

####Summarizing here by YMD, Year and Transect because in some years the transects were sampled more than once
PiscCPUEAdult <- df_piscAdult %>%
 group_by(YMD, Year, doy, Transect, Area, AreaTP, AreaYear, TimePeriod, Common_Name) %>%
 reframe(CPUE = sum(Count))  # Using reframe to return ungrouped data
write.csv(PiscCPUEAdult,"PiscCPUEAdult.csv")

#Pisc_mean_CPUE_Adult <- PiscCPUEAdult %>%
# group_by(Year, Area, Common_Name) %>%
# summarise(mean_CPUE_Adult = mean(CPUE, na.rm = TRUE))

#Pisc_mean_abundance_TP_sp_Area <- PiscCPUEAdult %>%
# group_by(TimePeriod, Common_Name, Area, AreaTP) %>%
# summarise(mean_CPUE_Adult = mean(CPUE, na.rm = TRUE))

##### CPUE by time period or year and area######
####Summarizing here by YMD, Year and Transect because in some years the transects were sampled more than once
PiscCPUE2Adult <- df_piscAdult %>%
 group_by(YMD, Year, doy, Transect, Area, AreaTP, AreaYear, TimePeriod) %>%
 reframe(CPUE = sum(Count))  # Using reframe to return ungrouped data
write.csv(PiscCPUE2Adult,"PiscCPUE2Adult.csv")


Pisc_mean_abundance_yr_Area <- PiscCPUE2Adult %>%
 group_by(Year, Area) %>%
 summarise(Pisc_mean_CPUE_Adult = mean(CPUE, na.rm = TRUE))

#### Same as above but with error bars #####
Pisc_mean_abundance_yr_Area_Adult <- PiscCPUE2Adult %>%
 group_by(Year, Area) %>%
 summarise(
  Pisc_mean_CPUE_Adult = mean(CPUE, na.rm = TRUE),
  sd = sd(CPUE, na.rm = TRUE),
  n = n(),
  se = sd / sqrt(n)
 )
###Summary for Time Period
#Pisc_mean_abundance_TP_Area <- PiscCPUE2 %>%
# group_by(TimePeriod, Area, AreaTP) %>%
# summarise(mean_abundance_per_year_transect = mean(CPUE, na.rm = TRUE))

##### Plot CPUE ######
Pisc_mean_abundance_yr_Area_Adult$Year <- as.numeric(as.character(Pisc_mean_abundance_yr_Area_Adult$Year))

options(repr.plot.width=8, repr.plot.height=4, repr.plot.res=300)


ggplot(Pisc_mean_abundance_yr_Area_Adult,
       aes(x = Year,
           y = Pisc_mean_CPUE_Adult,
           color = Area)) +
 geom_line(size = .5) +
 geom_point(size = 3) +
 ggtitle("Mean Adult Piscivore CPUE") +
 geom_vline(xintercept = 2021, linetype = "dashed", color = "grey") +
 labs(y = "Mean CPUE", color = "Area") +
 theme_bw(base_size = 15) +
 theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank())
ggsave("Adult Pisc CPUE by area.png", width = 8, height = 4, dpi = 300)

#### Plot Pisc CPUE with error bars #####
Pisc_mean_abundance_yr_Area_Adult$Year <- as.numeric(as.character(Pisc_mean_abundance_yr_Area_Adult$Year))
ggplot(Pisc_mean_abundance_yr_Area_Adult,
       aes(x = Year,
           y = Pisc_mean_CPUE_Adult,
           color = Area)) +
 geom_line(size = .5) +
 geom_point(size = 3) +
 geom_errorbar(aes(ymin = Pisc_mean_CPUE_Adult - se,
                   ymax = Pisc_mean_CPUE_Adult + se),
               width = 0.2, linewidth = 0.5) +
 ggtitle("Mean Adult Piscivore CPUE") +
 geom_vline(xintercept = 2021, linetype = "dashed", color = "grey") +
 labs(y = "Mean CPUE", color = "Area") +
 theme_bw(base_size = 15) +
 theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank())
ggsave("Adult Pisc CPUE by area with error bars.png", width = 8, height = 4, dpi = 300)


#---------------------------------------------------------------------------------------------------------------------
####Box Plots for CPUE by TimePeriod for the different areas #####

####Some Tests
is.integer(TempCPUE2$CPUE)          # TRUE/FALSE (may return FALSE if stored as numeric)
all(TempCPUE2$CPUE %% 1 == 0, na.rm=TRUE)  # are they all whole numbers?
summary(TempCPUE2$CPUE)
mean(TempCPUE2$CPUE == 0, na.rm=TRUE)      # proportion of zeros


TempCPUE2 <- TempCPUE2 %>% ### Removes the Construction from TimePeriod
 dplyr::filter(TimePeriod != "Construction")

TempCPUE2 <- TempCPUE2 %>%  ### Reorders Pre and Post on the X axis
 dplyr::filter(Area != "Construction") %>%
 dplyr::mutate(TimePeriod = factor(TimePeriod, levels = c("Pre", "Post")))


ggplot(TempCPUE2, aes(x = TimePeriod, y = CPUE, fill = Area)) +
 geom_boxplot(position = position_dodge(width = 0.8), outlier.alpha = 0.4) +
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
ggsave("CPUE boxplot by area with error bars.png", width = 8, height = 4, dpi = 300)
#########################################################################################################
####Test difference in CPUE between Pre and Post (Negative Binomial GLMM - repeated measures)
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

#### Clean and set factor order
TempCPUE2_clean <- TempCPUE2 %>%
 filter(!is.na(TimePeriod)) %>%
 filter(tolower(Area) != "construction") %>% 
 mutate(
  TimePeriod = factor(TimePeriod, levels = c("Pre", "Post")),
  Area = factor(Area),
  Transect = factor(Transect),
  Year = factor(Year)
 )

###Fit full interaction model
m_cpue_full <- glmmTMB(
 CPUE ~ TimePeriod * Area + ns(doy, df = 4) + (1|Transect) + (1|Year),
 family = nbinom2(),
 data = TempCPUE2_clean
)

###Test whether Pre/Post changes differ by Area
m_cpue_noInt <- update(m_cpue_full, . ~ . - TimePeriod:Area)
anova(m_cpue_full, m_cpue_noInt)

#If interaction not supported, test main effect of TimePeriod
m_cpue_noTP <- update(m_cpue_noInt, . ~ . - TimePeriod)
anova(m_cpue_noInt, m_cpue_noTP)

#Effect sizes (Pre vs Post within each Area)
emmeans(m_cpue_full, pairwise ~ TimePeriod | Area, type = "response")

#Diagnostics (strongly recommended)
res <- simulateResiduals(m_cpue_full)
plot(res)
testDispersion(res)
testZeroInflation(res)

#Important next step (recommended):test the overall Pre vs Post effect using the additive model
emm_overall <- emmeans(m_cpue_noInt, ~ TimePeriod, type = "response")
emm_overall
pairs(emm_overall)

#Multiple comparisons note (important if you plan to report "per area" p-values)
con <- emmeans(m_cpue_full, pairwise ~ TimePeriod | Area, type = "response")
con$contrasts
summary(con$contrasts, adjust = "holm")        # or "bonferroni"


###Plot overall CPUE differences by TimePeriod

# Overall adjusted means (averaged across Areas)
emm_cpue_tp <- emmeans(m_cpue_noInt, ~ TimePeriod, type = "response")

emm_cpue_tp_df <- as.data.frame(emm_cpue_tp) %>%
 mutate(TimePeriod = factor(TimePeriod, levels = c("Pre", "Post")))

ggplot(emm_cpue_tp_df, aes(x = TimePeriod, y = response)) +
 geom_point(size = 3) +
 geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
               width = 0.12, linewidth = 0.8) +
 labs(
  x = "Time Period",
  y = "Model-adjusted mean CPUE",
  title = "CPUE (Adjusted Means ± 95% CI): Pre vs Post"
 ) +
 theme_bw() +
 theme(
  plot.title = element_text(face = "bold"),
  axis.title = element_text(face = "bold")
 )
ggsave("CPUE_GLMMTimePeriod.png", width = 8, height = 4, dpi = 300)

###Plot CPUE by TimePeriod and Area
emm_cpue_area <- emmeans(m_cpue_full, ~ TimePeriod | Area, type = "response")

emm_cpue_area_df <- as.data.frame(emm_cpue_area) %>%
 mutate(
  TimePeriod = factor(TimePeriod, levels = c("Pre", "Post")),
  Area = factor(Area)
 )

ggplot(emm_cpue_area_df, aes(x = TimePeriod, y = response)) +
 geom_point(size = 3) +
 geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
               width = 0.12, linewidth = 0.8) +
 facet_wrap(~ Area) +
 labs(
  x = "Time Period",
  y = "Model-adjusted mean CPUE",
  title = "CPUE (Adjusted Means ± 95% CI) by Area and Time Period"
 ) +
 theme_bw() +
 theme(
  plot.title = element_text(face = "bold"),
  axis.title = element_text(face = "bold"),
  strip.text = element_text(face = "bold")
 )
ggsave("CPUE_GLMMTimePeriodArea.png", width = 8, height = 4, dpi = 300)

####Another example plot
pd <- position_dodge(width = 0.45)

ggplot(emm_cpue_area_df, aes(x = TimePeriod, y = response, colour = Area, group = Area)) +
 geom_point(position = pd, size = 3) +
 geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
               position = pd, width = 0.12, linewidth = 0.8) +
 labs(
  x = "Time Period",
  y = "Model-adjusted mean CPUE",
  colour = "Area",
  title = "CPUE (Adjusted Means ± 95% CI) by Time Period and Area"
 ) +
 theme_bw() +
 theme(
  plot.title = element_text(face = "bold"),
  axis.title = element_text(face = "bold")
 )
ggsave("CPUE_GLMMTimePeriodAreaStacked.png", width = 8, height = 4, dpi = 300)
