## --------------------------------------------------------------#
## Script name: script03-00_Figure_CombinedMetricbyYear.R
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
## Date Created:13Mar2026
##
## --------------------------------------------------------------#
## Modification Notes:
##
## --------------------------------------------------------------#

### Species Richness file

MeanSpRich <- readRDS("01_data/MeanSpRich.rds")

### CPUE file

mean_abundance_yr_Area <- readRDS("01_data/mean_abundance_yr_Area.rds")

#### Adult Piscivore file

mean_CPUE_AdPisc.rds <- readRDS("01_data/mean_CPUE_AdPisc.rds")

##### Guild

mean_CPUE_guild.rds <- readRDS("01_data/mean_CPUE_guild.rds")
