# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # #   Maryland Health Benefit Exchange  # # # # # # # # # # # # 
# # # # # # # # # #                                     # # # # # # # # # # # # 
# # # # # # # #  #  Program Name: sensitivity_analysis.R  # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # Analysis of take up of spousal ESI after loss of ESI due to COVID # # # # 
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
# POVERTY, MIGRATE1, MIGRATE1D]

# change to whatever the working directory is for your files
setwd("H:/Documents/Uninsured_by_zip/R Studio/COVID19_Uninsured_Analysis")
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
  library(tigris) # import TIGRIS shapefile data
  library(htmlwidgets) # export leaflet maps to HTML
})

# read in data dictionary from xml file produced by IPUMS job this will be used
# as needed to label variables or produce IPUMS citations as modifying the df
# causes the ipums labels and metadata to be dropped
acs_ddi_sa <- read_ipums_ddi("data/raw/usa_00019.xml")
# read in data output using ddi to label and format
acs_data_sa <- read_ipums_micro(acs_ddi_sa) %>% clean_names()

# need to first run the modified Urban Institute script available at:
# https://github.com/eleoMHBE/covid-neighborhood-job-analysis in the "sensitivity
# analysis" directory
ipums_data_merge_sa <- readRDS("data/processed/ipums-data-merge-sensitivity-analysis.Rds")
acs_df_sa <- ipums_data_merge_sa %>% clean_names()

# if you encounter RAM/memory errors, ipumsr has a vignette on working with big
# IPUMS data: 
# vignette(package="ipumsr",topic = "ipums-bigdata")
# generate grouping variables, filters, geo-joining variable
acs_df_sa <- acs_df_sa %>%
  # remove institutional inmates, 6090 records
  # new total: 290829 records
  filter(relate != 13) %>%
  # create age grouping
  mutate(agegroup = case_when(age >= 0 & age < 19 ~ "0 - 18",
                              age >= 19 & age < 35 ~ "19 - 34",
                              age >= 35 & age < 65 ~ "35 - 64",
                              age >= 65 ~ "65 +")) %>%
  # create 2nd age grouping
  mutate(agegrp2 = case_when(age >= 0 & age < 19 ~ "0 - 18",
                             age >= 19 & age < 35 ~ "19 - 34",
                             age >= 35 & age < 45 ~ "35 - 44",
                             age >= 45 & age < 55 ~ "45 - 54",
                             age >= 55 & age < 65 ~ "55 - 64",
                             age >= 65 ~ "65+")) %>% 
  # FPL groupings for ease of mapping
  mutate(pov_group = case_when(poverty >= 0 & poverty < 133 ~ "Less than 133% of FPL",
                               poverty >= 133 & poverty < 138 ~ "133-138% of FPL",
                               poverty >= 139 & poverty <= 150 ~ "139-150% of FPL",
                               poverty >= 151 & poverty <= 200 ~ "151-200% of FPL",
                               poverty >= 201 & poverty <= 250 ~ "201-250% of FPL",
                               poverty >= 251 & poverty <= 299 ~ "251-299% of FPL",
                               poverty >= 300 & poverty <= 400 ~ "300-400% of FPL",
                               poverty > 400 ~ "More than 400% FPL")) %>%
  # create race/ethnicity mappings
  mutate(race = case_when(race==1 ~ "White",
                          race==2 ~ "Black",
                          race==3 ~ "Native",
                          race==4 ~ "Chinese",
                          race==5 ~ "Japanese",
                          race==6 ~ "Asian",
                          race==7 ~ "Other",
                          race==8 ~ "Biracial",
                          race==9 ~ "Multiracial")) %>%
  mutate(race = ifelse(hispan %in% c(1:4),"Hispanic",race)) %>%
  # create a GEOID variable to join to shape file for PUMA 2010
  mutate(puma = as.numeric(puma)) %>%
  mutate(puma = sprintf("%05d", puma)) %>%
  mutate(GEOID10 = paste(statefip, puma, sep = "")) %>%
  # rename citizen parent/spouse variables for readability
  rename(citmom = citizen_mom) %>%
  rename(citmom2 = citizen_mom2) %>%
  rename(citpop = citizen_pop) %>%
  rename(citpop2 = citizen_pop2) %>%
  rename(citsp = citizen_sp)


# identify unins indiv likely to be lawfully present
# Assumption: Ppl that get public assistance, are veterans, or gov't employees,
# are less likely to be unlawfully present migrants

# obj w/ occupation codes associated to jobs likely to require legal status
# source list provided by CMSNY, kept private at their request
occupations <- readRDS("data/private/occupations.Rds")

# create variable that captures whether rec'd public assistance
acs_df_sa <- acs_df_sa %>% mutate_at(vars(incss,incwelfr,incsupp),
                                     funs(as.numeric(.))) %>%
  mutate_at(vars(incss, incwelfr, incsupp),
                               funs(case_when(
                                 . > 0 & . < 99999 ~ 1,
                                 . == 99999 ~ 0,
                                 TRUE ~ .
                               ))
  ) %>% 
    mutate(pubassist = ifelse((incss + incwelfr + incsupp) > 0, 1, 0)) %>%
    # public administration IND codes 9370:9590, military IND codes 9670:9870
    mutate(pubserv = ifelse(ind %in% c(9370:9590,9670:9870),1,0)) %>%
    # create new var to identify occupations likely to be lawfully present
    mutate(legalocc = ifelse(occ %in% occupations, 1, 0)) %>%
    # create new var for indiv likely to be lawfully present
    mutate(lawful = case_when(pubserv ==1 | # are public emps or military
                                pubassist == 1 | # have rec'd public assistance
                                vetstatd %in% c(12,13,20) | # are vets/active duty
                                citizen %in% c(0:2) | # are citizens/naturalized
                                yrimmig < 1982 | # IRCA of 1986 legal status
                                hcovpub == 2 ~ 1, # have public health ins
                              legalocc == 1 ~ 1,
                              # uncomment to filter >60 yrs @ year of immigration
                              (age - (2018 - yrimmig)) > 59 ~ 1,
                              citmom %in% c(0:2) | # mom is citizen/naturalized
                                citmom2 %in% c(0:2) |
                                citpop %in% c(0:2) | # dad is citizen/naturalized
                                citpop2 %in% c(0:2) |
                                citsp %in% c(0:2) ~ 1, # spouse is cit/naturalized
                              # uncomment to filter for BA/BS or higher educated
                              # chose not to use since some undoc have degrees
                              #educd %in% c(101,114:116) & # Bachelors or higher
                              TRUE ~ 0))
  # drop income vars as they are no longer needed
  acs_df_sa <- subset(acs_df_sa, select = -c(incss, incwelfr, incsupp))

# perform sensitivity analysis, suggestion from Hilltop: "imagine the outcome 
# as a binary choice for each household in the denominator defined as having [...] 
# an employed member who loses coverage tied to their job: would the individual 
# switch to another source of employer coverage through their family member 
# (lets call this probability p1, which is what the abstract estimates as 32%) 
# or not (lets call this probability p2, which would be 68%. i.e. all other 
# outcomes). This approximates a binomial distribution B(n,p1) where n is the 
# denominator/number of observations. Using these parameters, you can simulate 
# random outcomes for each individual/household in the denominator using your 
# national ACS data set, then aggregate to the county level, and finally restrict 
# to just MD counties."
  
# check to see if results match with MD only dataset
  df.sum.age.sa <- acs_df_sa %>%
    # uncomment to remove unauthorized immigrants
    filter(lawful == 1, statefip==24) %>%
    group_by(puma, agegroup) %>%
    summarize(
      tpop = sum(perwt),
      insured = sum(ifelse(hcovany==2, perwt, 0)),
      uninsured = sum(ifelse(hcovany==1, perwt, 0)),
      ins_emp = sum(ifelse(hinsemp==2, perwt, 0)),
      # uncomment to calc public/private insurance coverage
      #ins_pub = sum(ifelse(hcovpub==2, perwt,0)),
      #ins_priv = sum(ifelse(hcovpriv==2, perwt, 0)),
      unweight_per = n()
    ) %>% 
    mutate(pct_uninsured = uninsured/tpop)
  
  
# Create new binary var of whether they enroll in spousal insurance or not  
acs_df_sa <- acs_df_sa %>%
  summarise(ins_emp = sum(ifelse(hinsemp==2,perwt,0)),
            total_emp_pre = sum(total_employment),
            total_unemp_post = sum(total_disemployment)) %>%
  mutate(pct_change_imputed = ifelse(total_emp_pre == 0,
                                     0,
                                     -total_unemp_post / total_emp_pre)) %>%
  mutate(esi_loss = -ins_emp * pct_change_imputed) %>%
  # convert negative esi_loss to 0 for ease of calculations
  mutate(esi_loss = ifelse(esi_loss<0, 0, esi_loss))  %>%
  mutate(sp_ins = dbinom(n=esi_loss,size=1,prob = .32))
         
# limit to just MD and save as RDS for other scripts
acs_df <- acs_df_sa %>% filter(statefip==24)

