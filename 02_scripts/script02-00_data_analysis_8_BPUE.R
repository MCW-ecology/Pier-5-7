## --------------------------------------------------------------#
## Script name: script02-00_data_analysis_8_BPUE.R
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

df <- readRDS("01_data/Efish_processed.rds")
events <- readRDS("01_data/events.rds")
