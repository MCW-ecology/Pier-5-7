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
SpeciesSumDate <- df %>% dplyr::group_by(Common_Name,YMD,Year, Area,Transect) %>% summarise(Count =length(Common_Name)) 
write.csv(SpeciesSumDate,"SpeciesSumDate.csv")

### Summary by transect/year/species
SpeciesSumYear <- df %>% dplyr::group_by(Common_Name,Year, Area,Transect) %>% summarise(Count =length(Common_Name)) 
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

### Summary by transect/year/species
SpeciesSumTPArea <- df %>% dplyr::group_by(Sp_Code, Common_Name,TimePeriod, Area, AreaTP) %>% summarise(Count =length(Common_Name)) 
write.csv(SpeciesSumTPArea,"SpeciesSumTPArea.csv")

#### Now for the Pivot table ##########
PivotSpeciesTP <- dcast(SpeciesSumTPArea, Sp_Code+Common_Name ~ AreaTP, value.var = "Count")
PivotSpeciesTP[is.na(PivotSpeciesTP)] <- 0

write.csv(PivotSpeciesTP,"PivotSpeciesTP.csv") 

#---------------------------------------------------------------------------------------------------------------
####Species Richnesss############

############################
### Summary by Year/Area ####
#############################
#### Remove Carp x Goldfish hybrid since it is not a species (only in 2018 in and both Goldfish and Carp were found in the same location and year)
SpeciesSumYearArea <-SpeciesSumYearArea[!(SpeciesSumYearArea$Common_Name=="Carp x Goldfish hybrid"),] ### removes fish code F000
SpeciesRichYearArea <- SpeciesSumYearArea %>% dplyr::group_by(Year, Area, AreaYear) %>% summarise(Count =length(Common_Name)) 
write.csv(SpeciesRichYearArea,"SpeciesRichYearArea.csv")

#### Now for the Pivot table ##########
PivotSpeciesRichAreaYear <- dcast(SpeciesRichYearArea, Area ~ Year, value.var = "Count")
PivotSpeciesRichAreaYear[is.na(PivotSpeciesRichAreaYear)] <- 0

write.csv(PivotSpeciesRichAreaYear,"PivotSpeciesRichAreaYear.csv")

##### Plot SpRichness ######
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
 reframe(Abundance = sum(Count))  # Using reframe to return ungrouped data
write.csv(TempCPUE,"TempCPUE.csv")

mean_abundance_yr_sp_Area <- TempCPUE %>%
 group_by(Year, Area, Common_Name) %>%
 summarise(mean_abundance_sp_per_year_transect = mean(Abundance, na.rm = TRUE))

mean_abundance_TP_sp_Area <- TempCPUE %>%
 group_by(TimePeriod, Common_Name, Area, AreaTP) %>%
 summarise(mean_abundance_sp_per_year_transect = mean(Abundance, na.rm = TRUE))

##### CPUE by time period or year and area######
####Summarizing here by YMD, Year and Transect because in some years the transects were sampled more than once
TempCPUE2 <- df %>%
 group_by(YMD, Year, Transect, Area, AreaTP, AreaYear, TimePeriod) %>%
 reframe(Abundance = sum(Count))  # Using reframe to return ungrouped data
write.csv(TempCPUE2,"TempCPUE2.csv")

mean_abundance_yr_Area <- TempCPUE2 %>%
 group_by(Year, Area) %>%
 summarise(mean_abundance_per_year_transect = mean(Abundance, na.rm = TRUE))

##### same as above but with error bars #####
mean_abundance_yr_Area <- TempCPUE2 %>%
 group_by(Year, Area) %>%
 summarise(
  mean_abundance_per_year_transect = mean(Abundance, na.rm = TRUE),
  sd = sd(Abundance, na.rm = TRUE),
  n = n(),
  se = sd / sqrt(n)
 )


mean_abundance_TP_Area <- TempCPUE2 %>%
 group_by(TimePeriod, Area, AreaTP) %>%
 summarise(mean_abundance_per_year_transect = mean(Abundance, na.rm = TRUE))

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

####Summarizing here by YMD, Year and Transect because in some years the transects were sampled more than once
PiscCPUE <- df_pisc %>%
 group_by(YMD, Year, Transect, Area, AreaTP, AreaYear, TimePeriod, Common_Name) %>%
 reframe(Abundance = sum(Count))  # Using reframe to return ungrouped data
write.csv(PiscCPUE,"PiscCPUE.csv")

Pisc_mean_abundance_yr_sp_Area <- PiscCPUE %>%
 group_by(Year, Area, Common_Name) %>%
 summarise(mean_abundance_sp_per_year_transect = mean(Abundance, na.rm = TRUE))

Pisc_mean_abundance_TP_sp_Area <- PiscCPUE %>%
 group_by(TimePeriod, Common_Name, Area, AreaTP) %>%
 summarise(mean_abundance_sp_per_year_transect = mean(Abundance, na.rm = TRUE))

##### CPUE by time period or year and area######
####Summarizing here by YMD, Year and Transect because in some years the transects were sampled more than once
PiscCPUE2 <- df_pisc %>%
 group_by(YMD, Year, Transect, Area, AreaTP, AreaYear, TimePeriod) %>%
 reframe(Abundance = sum(Count))  # Using reframe to return ungrouped data
write.csv(PiscCPUE2,"PiscCPUE2.csv")


Pisc_mean_abundance_yr_Area <- PiscCPUE2 %>%
 group_by(Year, Area) %>%
 summarise(mean_abundance_per_year_transect = mean(Abundance, na.rm = TRUE))

#### Same as above but with error bars #####
Pisc_mean_abundance_yr_Area <- PiscCPUE2 %>%
 group_by(Year, Area) %>%
 summarise(
  mean_abundance_per_year_transect = mean(Abundance, na.rm = TRUE),
  sd = sd(Abundance, na.rm = TRUE),
  n = n(),
  se = sd / sqrt(n)
 )

Pisc_mean_abundance_TP_Area <- PiscCPUE2 %>%
 group_by(TimePeriod, Area, AreaTP) %>%
 summarise(mean_abundance_per_year_transect = mean(Abundance, na.rm = TRUE))

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
 ggtitle("Piscivore Mean CPUE") +
 geom_vline(xintercept = 2021, linetype = "dashed", color = "grey") +
 labs(y = "Mean CPUE", color = "Area") +
 theme_bw(base_size = 15) +
 theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank())
ggsave("Pisc CPUE by area with error bars.png", width = 8, height = 4, dpi = 300)
