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

##### Load packages
source("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/02_scripts/script00-01_load_packages.R") 

#### Import data

source("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/02_scripts/script01-00_import_efish_data.R")

source("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/02_scripts/script01-00_import_telem_data.R")

#### Analysis

source("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/02_scripts/script02-00_data_analysis_1_habitat.R")

source("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/02_scripts/script02-00_data_analysis_2_Sp_Richness.R")

source("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/02_scripts/script02-00_data_analysis_3_CPUE.R") 

source("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/02_scripts/script02-00_data_analysis_4_Adult_Pisc_CPUE.R")

source("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/02_scripts/script02-00_data_analysis_5_Guilds.R")

source("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/02_scripts/script02-00_data_analysis_7_non-nativeCPUE.R")

source("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/02_scripts/script02-00_data_analysis_8_BPUE.R")

##### Figures
source("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/02_scripts/script02-00_Figure_EnvData.R")

source("C:/Users/croftwhitem/Documents/GitHub/Pier-5-7/02_scripts/script02-00_Figure_Paired.R")
