## --------------------------------------------------------------#
## Script name: script01_import_efish_data.R
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
## Date Created:4Dec2025
##
## --------------------------------------------------------------#
## Modification Notes:
##
## --------------------------------------------------------------#


### Core Data Processing
#----------------------------#

##########################
## Import Data and Prep ##
##########################

#setwd("~/GitHub/Piers-5-7/")

data_efish_lenW <- read.csv("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/01_data/01_raw_files/HH_efish_lengthweights_2018-2023.csv")
data_efish_biomass <- read.csv("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/01_data/01_raw_files/HH_Biomass2023.csv")
data_taxon <- read.csv("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/01_data/01_raw_files/taxon.csv")



cat("Loaded", format(nrow(data_efish_lenW), big.mark = ","), "records\n")

############################
####Selecting Transects#####
############################

temp_lenw <- subset(data_efish_lenW, Transect %in% c("HH50","HH51","HH52","HH53","HH54","HH55","HH56","HH57","HH58","HH59","HH60"))
cat("Loaded", format(nrow(temp_lenw), big.mark = ","), "records\n")


temp_biomass <- subset(data_efish_biomass, Transect %in% c("HH50","HH51","HH52","HH53","HH54","HH55","HH56","HH57","HH58","HH59","HH60"))
cat("Loaded", format(nrow(temp_biomass), big.mark = ","), "records\n")

###get areas
temp_lenw$Area<-ifelse(temp_lenw$Transect %in% c("HH50","HH51","HH52","HH53","HH54"),"Piers 5-7","Macassa Bay")
temp_biomass$Area<-ifelse(temp_biomass$Transect %in% c("HH50","HH51","HH52","HH53","HH54"),"Piers 5-7","Macassa Bay")

###########################
####Formatting and Selecting Years########
##########################

temp_lenw$YMD <- format(as.Date(temp_lenw$Date), format = "%Y-%m-%d")
temp_lenw$MonthYear<-format(as.Date(temp_lenw$YMD), "%b-%Y")
temp_lenw$Month<-format(as.Date(temp_lenw$YMD), "%m")
temp_lenw$Year<-format(as.Date(temp_lenw$YMD), "%Y")


#temp_lenw2 <- temp_lenw %>%
# filter(temp_lenw$Year >= 2018 & temp_lenw$Year <= 2023)

temp_biomass$YMD <- format(as.Date(temp_biomass$Date), format = "%Y-%m-%d")
temp_biomass$MonthYear<-format(as.Date(temp_biomass$YMD), "%b-%Y")
temp_biomass$Month<-format(as.Date(temp_biomass$YMD), "%m")
temp_biomass$Year<-format(as.Date(temp_biomass$YMD), "%Y")

#temp_biomass2 <- temp_biomass %>%
# filter(temp_biomass$Year >= 2018 & temp_biomass$Year <= 2023)

#####Data Cleaning#########################################----
#-------------------------------------------------------------#

### Handle Missing Values
#----------------------------#

# Check and handle NAs
# temp_na_count <- sum(is.na(data_raw))
# cat("Missing values:", temp_na_count, "\n")

#########################################################
####Add in Batch Count to Individual Count ##############
#########################################################
# Note: file is set up with a row for each fish, if there are more than 20 individuals
# then the fish are batched and the batch count and biomass numbers show up in every row 
# for that transect/Date/Species
# Batches need to be subset out and then added back into the total count for the species

batch_data <- temp_lenw[!is.na(temp_lenw$Batch_count), ] #remove rows that don't have batch data
batch_total<- batch_data[batch_data$Individual_id == "1", ] #select out the first instance where the batch was recorded for that Transect/Date/Species
data.table::setnames(batch_total,'Batch_count','Count')  ### Changes column name

### Count of species by transect/Date/Species
temp_lenw['Count']='1' #make a new column with the  

### Add batch back in #####
temp_combined <- dplyr::bind_rows(temp_lenw,batch_total)
### Sum count data by Transect/YMD/Species


#####Data Formatting#######################################----
#-------------------------------------------------------------#

### Create Derived Variables
#----------------------------#

# Add calculated fields
# temp_formatted <- temp_clean %>%
#   mutate(
#     year = lubridate::year(date),
#     month = lubridate::month(date),
#     log_value = log(value + 1)
#   )


### Filter and Select
#----------------------------#

# Keep only relevant observations and variables
# df_final <- temp_formatted %>%
#   filter(year >= 2020) %>%
#   select(id, date, year, month, category, value, log_value)

#cat("Final dataset:", format(nrow(df_final), big.mark = ","), "records\n")


#####Data Validation#######################################----
#-------------------------------------------------------------#

### Check Data Quality
#----------------------------#

# Verify data integrity
# temp_duplicates <- df_final %>%
#   group_by(id, date) %>%
#   filter(n() > 1)
#
# if(nrow(temp_duplicates) > 0) {
#   warning("Found ", nrow(temp_duplicates), " duplicate records")
# }


#####Export Processed Data (Optional)######################----
#-------------------------------------------------------------#

# Save processed data for future use (commented out - manual export only)
# saveRDS(df_final, "01 - Data/processed_data.rds")


#####Cleanup###############################################----
#-------------------------------------------------------------#

# Remove temporary objects
#rm(list = ls(pattern = "^temp_"))
#cat("Cleanup complete.\n")


## End of script

