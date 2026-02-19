## --------------------------------------------------------------#
## Script name: script00-00_user_interface.R
##
## Purpose of script:
##    A central location to order and source project scripts
##    Script naming:
##      - scriptXX-YY format (XX = class 00-99, YY = script 01-99)
##      - higher numbers depend on lower numbers
##      - letters (a,b,c) indicate no dependency between same-numbered scripts
##
## Author:M Croft-White
##
## Date Created:Dec 2025
##
## --------------------------------------------------------------#
## Modification Notes:
##
## --------------------------------------------------------------#


### Core Data Processing
#----------------------------#

R.version.string

setwd("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/")
source("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/02_scripts/script00-01_load_packages.R") 

source("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/02_scripts/script01-00_import_efish_data.R") 

source("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/02_scripts/script02-00_data_analysis_Sp_Richness.R")

source("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/02_scripts/script02-00_data_analysis_CPUE.R") 

source("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/02_scripts/script02-00_data_analysis_Adult_Pisc_CPUE.R")

source("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/02_scripts/script02-00_data_analysis_Guilds.R")
