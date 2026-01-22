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

#########################################################################
#### Make Pivot table with species as rows and Year/Area as columns #####
#########################################################################

### Summary by transect/year/species
SpeciesSumYearArea <- df %>% dplyr::group_by(Sp_Code, Common_Name,Year, Area) %>% summarise(Count =length(Common_Name)) 
write.csv(SpeciesSumYearArea,"SpeciesSumYearArea.csv")

#### Now for the Pivot table ##########
PivotSpeciesYear <- dcast(SpeciesSumYearArea2, Sp_Code+Common_Name ~ AreaYear, value.var = "Count")
PivotSpeciesYear[is.na(PivotSpeciesYear)] <- 0

write.csv(PivotSpeciesYear,"PivotSpeciesYear.csv")

#########################################################################
#### Make Pivot table with species as rows and TimePeriod/Area as columns #####
#########################################################################

### Summary by transect/year/species
SpeciesSumTPArea <- df %>% dplyr::group_by(Sp_Code, Common_Name,TimePeriod, AreaTP) %>% summarise(Count =length(Common_Name)) 
write.csv(SpeciesSumTPArea,"SpeciesSumTPArea.csv")

#### Now for the Pivot table ##########
PivotSpeciesTP <- dcast(SpeciesSumTPArea, Sp_Code+Common_Name ~ AreaTP, value.var = "Count")
PivotSpeciesTP[is.na(PivotSpeciesTP)] <- 0

write.csv(PivotSpeciesTP,"PivotSpeciesTP.csv") 
##################################################
####Number transects sampled per year ############
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
