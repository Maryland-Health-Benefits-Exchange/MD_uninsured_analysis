# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # #   Maryland Health Benefit Exchange  # # # # # # # # # # # # 
# # # # # # # # # #                                     # # # # # # # # # # # # 
# # # # # # # # # #  Program Name: net_enrt_analysis.R  # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # Analysis of uninsured pre and post COVID-19-related loss of ESI # # # # 
# # # # # # # # #   adj. to account for unauth. immigrants  # # # # # # # # # #
# # # # # # # # # ######################################### # # # # # # # # # # 
# # # # # # # # # #######....###################    ####### # # # # # # # # # # 
# # # # # # # # # #######........##########         ####### # # # # # # # # # # 
# # # # # # # # # ########..........#####          ######## # # # # # # # # # # 
# # # # # # # # # ##########.........###         ########## # # # # # # # # # # 
# # # # # # # # # ############........#       ############# # # # # # # # # # # 
# # # # # # # # # ######################################### # # # # # # # # # # 
# # # # # # # # # ###############     #.....############### # # # # # # # # # # 
# # # # # # # # # ###########         #.........########### # # # # # # # # # # 
# # # # # # # # # #########          ###..........######### # # # # # # # # # # 
# # # # # # # # # #######          #######.........######## # # # # # # # # # # 
# # # # # # # # # #######       #############.......####### # # # # # # # # # # 
# # # # # # # # # ######################################### # # # # # # # # # # 

################################################################################
############### Pre-processing: Load libs, read in data, format ################
################################################################################

# Data files were obtained from multiple sources. IPUMS was used to generate the 
# data and DDI files for ACS, the unemployment map was imported using the ESRI2df
# r library, and BLS CES data was downloaded via the API in the Urban Institute
# modified script
# #
# IPUMS extract request: 2018 ACS 5yr (sample), Variables [YEAR, MULTYEAR, SAMPLE, 
# SERIAL, HHWT, CLUSTER, STRATA, COUNTYFIP, STATEFIP, METRO, PUMA, GQ, OWNERSHP, 
# OWNERSHPD, PERNUM, PERWT, FAMSIZE, RELATE, RELATED, SEX, AGE, RACE, RACED, 
# HISPAN, HISPAND, BPL, BPLD, CITIZEN, YRIMMIG, HCOVANY, HCOVPRIV, HCOVPUB, 
# HINSEMP, EMPSTAT, EMPSTATD, OCC, EDUC, INCSS, INCWELFR, INCSUPP, VETSTAT, IND, 
# POVERTY, MIGRATE1, MIGRATE1D, MARST] - characteristics attached: citizenship
# and employment status of parents, spouse

# change to whatever the working directory is for your files
setwd("C:/Documents/COVID19_Uninsured_Analysis")
#
#library(tidycensus) # can be used to import census data in tidy format
#library(pivottabler) # makes pivot tables
#library(survey) # construct survey objs for variance estimation
#library(mapview) # saves the image of the map for exporting
#library(mapedit) # more map tools
suppressPackageStartupMessages({
  library(ipumsr) # import ACS microdata from IPUMS.org
  library(acs) # useful functions for working w/ ACS data
  library(tidyverse) # many useful syntax and QOL functions
  library(labelled) # labels variables/values
  library(janitor) # cleans up column names
  library(stringr) # for working with strings
  library(knitr) # for RMarkdown to HTML
  library(rmarkdown) # to generate Rmd final document
  library(flexdashboard) # to generate Rmd dashboard
  library(sf) # simple features for mapping
  library(leaflet) # creates interactive maps
  library(testit) # to run assertions to verify data
  library("esri2sf") # import ESRI ArcGIS maps as shapefiles
  library(tigris) # import TIGRIS shapefile data
  library(htmlwidgets) # export leaflet maps to HTML
  library(DT) # display HTML tables from dataframes/matrices
  library(shiny) # dashboard interactivity
  library(ggthemes) # more themes for ggplot2
})
###################################################### net enrollment analysis
# this section imports enrollment data from MHC that is not publicly available
# any replication of this analysis will need to use state-specific data

# import enrollment totals file
library(readxl)
Net_Enrollments <- read_excel("data/private/Net_Enrollments.xlsx", 
                              sheet = "Net_Enrt_Cnty")
enrt_df <- Net_Enrollments %>% clean_names()

# drop last row which contains total for each column
enrt_df <- enrt_df[-25,]
# use geocorr to convert from ZipCode to PUMA
# read in ZIP to PUMA crosswalk
geocorrP2C <- read_csv("data/raw/geocorr_cnty_to_PUMA.csv") %>% 
  mutate(puma12 = sprintf("%05d", as.numeric(puma12)))
# join crosswalk to totals df by county
enrt_df <- left_join(geocorrP2C, enrt_df, by = c("cntyname"="county"))
# drop first row which contains labels for each column
enrt_df <- enrt_df[-1,]
# calculate totals per PUMA using allocation factor
enrt_df <- enrt_df %>% mutate(ma.afact = net_ma*as.numeric(afact),
                              qhp.afact = net_qhp*as.numeric(afact),
                              grand_total.afact = net_total*as.numeric(afact))
# aggregate up to puma level, summing the allocated totals
puma.enrt.df <- enrt_df %>% group_by(puma12) %>% summarise(ma = sum(ma.afact, 
                                                                    na.rm = TRUE),
                                                           qhp = sum(qhp.afact,
                                                                     na.rm = TRUE),
                                                           grand_total = sum(grand_total.afact,
                                                                             na.rm = TRUE))
# read in detailed enrollment files

# drop last row which contains column totals


# use geocorr to convert from ZipCode to PUMA for each enrollment df, following
# same steps as enrt_df above


# transform from wide to long format for ease of calculation


# create output datasets for use in adjusting specific subgroupings of later tables