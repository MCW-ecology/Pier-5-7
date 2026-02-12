## --------------------------------------------------------------#
## Script name: script02-00_data_analysis_CPUE.R
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
