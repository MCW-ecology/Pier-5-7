## --------------------------------------------------------------#
## Script name: script01_import_telem_data.R
##
## Purpose of script:
##    Import efish data and subset desired transects
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



##########################
## Import Data and Prep ##
##########################

#setwd("~/GitHub/Piers-5-7/")


data_telem <- readRDS("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/01_data/03_large_files_LFS/01_raw_files/detections_filtered2_2015-2025.rds")

data_telem$DMYHM <- ymd_hms(data_telem$detection_timestamp_utc)

data_telem$Year<-format(as.Date(data_telem$DMYHM), "%Y")

### Summary by TimePeriod/Area/species/Transect
SpYearHarbour <- data_telem %>% dplyr::group_by(transmitter_codespace,transmitter_id, common_name_e, Year) %>% summarise(Count =length(transmitter_id))

#### Now for the Pivot table ##########
PivotSpeciesYearHarbour <- dcast(SpYearHarbour, common_name_e ~ Year, value.var = "Count")
PivotSpeciesYearHarbour[is.na(PivotSpeciesYearHarbour)] <- 0
write.csv(PivotSpeciesYearHarbour,"PivotSpeciesYearHarbour.csv")

temp_telem <- subset(data_telem, Year %in% c("2018", "2019", "2020", "2021", "2022", "2023"))
cat("Loaded", format(nrow(temp_telem), big.mark = ","), "records\n")

temp_telem2 <- subset(temp_telem, station %in% c("HAM-036", "HAM-011", "HAM-053", "HAM-057", "HAM-054", "HAM-056", "HAM-055", "HAM-052"))
cat("Loaded", format(nrow(temp_telem2), big.mark = ","), "records\n")

saveRDS(temp_telem2, "01_data/03_large_files_LFS/01_raw_files/telem_subset.rds")

data_subset <- readRDS("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/01_data/03_large_files_LFS/01_raw_files/telem_subset.rds")

### Summary by TimePeriod/Area/species/Transect
SpYear <- temp_telem2 %>% dplyr::group_by(transmitter_codespace,transmitter_id, common_name_e, Year) %>% summarise(Count =length(transmitter_id))

### Summary by TimePeriod/Area/species/Transect
SpYear2 <- SpYear %>% dplyr::group_by(common_name_e, Year) %>% summarise(Count =length(common_name_e))

#### Now for the Pivot table ##########
PivotSpeciesYear <- dcast(SpYear, common_name_e ~ Year, value.var = "Count")
PivotSpeciesYear[is.na(PivotSpeciesYear)] <- 0
write.csv(PivotSpeciesYear,"PivotSpeciesYear.csv")

