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



##########################
## Import Data and Prep ##
##########################

#setwd("~/GitHub/Piers-5-7/")

data_efish_lenW <- read.csv("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/01_data/01_raw_files/elecfish_lengthweight_HH20182023_NEW.csv")
data_efish_biomass <- read.csv("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/01_data/01_raw_files/HH_Biomass2023.csv")
data_taxon <- read.csv("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/01_data/01_raw_files/taxon.csv")
data_metrics <-read.csv("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/01_data/01_raw_files/Lk Ont full sp list 126 2Feb2023.csv")

### Prep Taxon file ####
data_taxon <- data_taxon %>% select(1:4)
data_taxon <- data_taxon %>% rename(Sp_Code = ACCESS_CODE)
data_taxon <- data_taxon %>% rename(Common_Name = COMMON_NAME)

### Prep length weight file ####
###Biomass column and Number.Individuals in the length weight file is confusing. 
###Biomass refers to total biomass of that species for a transect/date, so if 
### there are two YelPerch each weight 45 g, biomass recorded
### in each column would be 90g and Number.Individuals would be 2
data_efish_lenW$Biomass <- NULL ### removes the Biomass column
data_efish_lenW$Number.Individuals <- NULL ### removes the Biomass column

#### Prep Metrics file #####
data_metrics <- data_metrics %>% select(1,16)
data_metrics <- data_metrics %>% rename(Sp_Code = MNR.Fish.ID)

cat("Loaded", format(nrow(data_efish_lenW), big.mark = ","), "records\n")

############################
####Selecting Transects#####
############################

temp_lenw <- subset(data_efish_lenW, Transect %in% c("HH50","HH51","HH52","HH53","HH54","HH55","HH56","HH57","HH58","HH59","HH60"))
cat("Loaded", format(nrow(temp_lenw), big.mark = ","), "records\n")
temp_lenw <- temp_lenw %>% rename(Sp_Code = Species)



temp_biomass <- subset(data_efish_biomass, Transect %in% c("HH50","HH51","HH52","HH53","HH54","HH55","HH56","HH57","HH58","HH59","HH60"))
cat("Loaded", format(nrow(temp_biomass), big.mark = ","), "records\n")

#########################
###Assign areas #########
#########################
temp_lenw$Area <- ifelse(temp_lenw$Transect %in% c("HH50","HH51","HH53","HH54"),"Piers 5-7",
        ifelse(temp_lenw$Transect %in% c("HH52"),"Construction Site",
               "Macassa Bay"))


temp_biomass$Area <- ifelse(temp_biomass$Transect %in% c("HH50","HH51","HH53","HH54"),"Piers 5-7",
                         ifelse(temp_biomass$Transect %in% c("HH52"),"Construction Site",
                                "Macassa Bay"))
##########################################
####Formatting and Selecting Years########
##########################################

temp_lenw$YMD <- format(as.Date(temp_lenw$Date), format = "%Y-%m-%d")
temp_lenw$MonthYear<-format(as.Date(temp_lenw$YMD), "%b-%Y")
temp_lenw$Month<-format(as.Date(temp_lenw$YMD), "%m")
temp_lenw$Year<-format(as.Date(temp_lenw$YMD), "%Y")


temp_lenw <- temp_lenw %>%
 mutate(
  Date = ymd(YMD),     # convert YMD → Date
  doy  = yday(Date)    # extract day of year (1–366)
 )



#temp_lenw2 <- temp_lenw %>%
# filter(temp_lenw$Year >= 2018 & temp_lenw$Year <= 2023)

temp_biomass$YMD <- format(as.Date(temp_biomass$Date), format = "%Y-%m-%d")
temp_biomass$MonthYear<-format(as.Date(temp_biomass$YMD), "%b-%Y")
temp_biomass$Month<-format(as.Date(temp_biomass$YMD), "%m")
temp_biomass$Year<-format(as.Date(temp_biomass$YMD), "%Y")

#######################################
####Selecting night data###############
#######################################

temp_lenw <- subset(temp_lenw, Time.Period %in% c("2"))
cat("Loaded", format(nrow(temp_lenw), big.mark = ","), "records\n")

temp_biomass <- subset(temp_biomass, Time.Period %in% c("2"))
cat("Loaded", format(nrow(temp_lenw), big.mark = ","), "records\n")
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

batch_total$Weight <- NULL ### removes the weight column
batch_total <- batch_total %>% rename(Weight = Batch_weight)

#data.table::setnames(batch_total,'Batch_weight','Weight')  ### Changes column name

### Make a count column
temp_lenw['Count']='1' #make a new column with the  
temp_lenw$Count <- as.numeric(temp_lenw$Count)

### Add batch back in #####
temp_combined <- dplyr::bind_rows(temp_lenw,batch_total)
### Sum count data by Transect/YMD/Species

### Add Taxon info in #####
combinedA <- merge(temp_combined, data_taxon, by = "Sp_Code")

### Add Metrics info in ####
combined <- merge(combinedA, data_metrics, by = "Sp_Code")

### Add Pre and Post Construction labels

combined$TimePeriod <- as.factor(
 ifelse(combined$Year <= 2020, "Pre",
        ifelse(combined$Year == 2021, "Construction",
               "Post")))

#####Export Processed Data #################################----
#-------------------------------------------------------------#
write.csv(combined,"Efish_Table1.csv")
saveRDS(combined, "01_data/Efish_processed.rds")




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



# Save processed data for future use (commented out - manual export only)
# saveRDS(df_final, "01 - Data/processed_data.rds")


#####Cleanup###############################################----
#-------------------------------------------------------------#

# Remove temporary objects
#rm(list = ls(pattern = "^temp_"))
#cat("Cleanup complete.\n")


## End of script

