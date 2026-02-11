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

###determine juvenile vs adult piscivores or lithophilic species (ref is Scott and Crossman for all, except OFFLHD for
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



TempCPUE <- df %>%
 group_by(YMD, Year, Transect, Area, AreaTP, AreaYear, TimePeriod, Common_Name, doy) %>%
 reframe(CPUE = sum(Count))  # Using reframe to return ungrouped data

##################################################################
#### Box Plot of Adult CPUE
######################################################

library(dplyr)
library(ggplot2)
library(stringr)

# Use your richness data frame here
dat_box <- TempMeanSpRichTP %>%
 # 1) Exclude "Construction" from the TimePeriod column (case-insensitive & trims spaces)
 mutate(TimePeriod = str_trim(TimePeriod)) %>%
 filter(!is.na(TimePeriod)) %>%
 filter(!str_detect(str_to_lower(TimePeriod), "^construction\\b")) %>%
 # 2) Order TimePeriod as Pre, Post
 mutate(TimePeriod = factor(TimePeriod, levels = c("Pre", "Post"))) %>%
 # Keep all Area values, including "Construction Site"
 droplevels()

# If you're plotting CPUE from a different table, just change y = Count -> y = CPUE and the data frame name above.

# Grouped box plot (boxes per Area within each TimePeriod)
pd <- position_dodge(width = 0.8)

p <- ggplot(dat_box, aes(x = TimePeriod, y = Count, fill = Area)) +
 geom_boxplot(position = pd, outlier.alpha = 0.4) +
 labs(
  x = "Time Period",
  y = "Species richness (Count)",   # If using CPUE, change this to "CPUE"
  fill = "Area",
  title = "Species Richness by Time Period and Area"
 ) +
 theme_bw() +
 theme(
  axis.text.x  = element_text(angle = 45, hjust = 1),
  plot.title   = element_text(face = "bold")
 )

print(p)
ggsave("BoxPlot_TimePeriod_Area.png", p, width = 8, height = 4, dpi = 300)

#########################################################################################################
####Test difference in Adult Piscivore CPUE between Pre and Post (Negative Binomial GLMM - repeated measures)
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

# 1) All sampling events (keeps events even when there are zero adult piscivores)
events <- df %>%
 distinct(YMD, Year, Transect, Area, AreaTP, AreaYear, TimePeriod, doy)

# 2) Sum counts for rows flagged as adult piscivore
pisc_counts <- df %>%
 filter(adult == "Y") %>%      # <-- your boolean column name
 group_by(YMD, Year, Transect, Area, AreaTP, AreaYear, TimePeriod, doy) %>%
 summarise(CPUE = sum(Count), .groups = "drop")

# 3) Join to events and fill missing with 0 (very important for modeling)
PiscCPUE2Adult <- events %>%
 left_join(pisc_counts, by = c("YMD","Year","Transect","Area","AreaTP","AreaYear","TimePeriod","doy")) %>%
 mutate(CPUE = tidyr::replace_na(CPUE, 0))

### Prep a clean dataset

pisc_ad <- PiscCPUE2Adult %>%
 filter(!is.na(TimePeriod)) %>%
 filter(tolower(Area) != "construction") %>% 
 mutate(
  TimePeriod = factor(TimePeriod, levels = c("Pre", "Post")),
  Area = factor(Area),
  Transect = factor(Transect),
  Year = factor(Year)
 )

pisc_ad <- pisc_ad %>%
 filter(!is.na(TimePeriod))


####Fit a negative binomial GLMM (primary model)
####Note, got an error here, model would not converge. Small sample size, low counts, small n per cell
m_pisc_full <- glmmTMB(
 CPUE ~ TimePeriod * Area + ns(doy, df = 4) + (1 | Transect) + (1 | Year),
 family = nbinom2(),
 data = pisc_ad
)

# Reduced (no interaction) for the LRT:
m_pisc_noInt <- update(m_pisc_full, . ~ . - TimePeriod:Area)

# Test whether Pre–Post change differs by Area (interaction)
anova(m_pisc_full, m_pisc_noInt)


# Test overall TimePeriod effect (Pre vs Post averaged over areas)
m_pisc_noTP <- update(m_pisc_noInt, . ~ . - TimePeriod)
anova(m_pisc_noInt, m_pisc_noTP)

# Test overall Area effect (averaged over TimePeriod)
m_pisc_noArea <- update(m_pisc_noInt, . ~ . - Area)
anova(m_pisc_noInt, m_pisc_noArea)







#--------------------------------------------------------------------------------------------------------------------
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