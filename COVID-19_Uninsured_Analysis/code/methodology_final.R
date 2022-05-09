# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # #   Maryland Health Benefit Exchange  # # # # # # # # # # # # 
# # # # # # # # # #                                     # # # # # # # # # # # # 
# # # # # # # # # #  Program Name: methodology_final.R  # # # # # # # # # # # # 
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

# read in data dictionary from xml file produced by IPUMS job this will be used
# as needed to label variables or produce IPUMS citations as modifying the df
# causes the ipums labels and metadata to be dropped
acs_ddi <- read_ipums_ddi("data/raw/usa_00038.xml")
# read in data output using ddi to label and format
acs_data <- read_ipums_micro(acs_ddi) %>% clean_names()

# need to first run the modified Urban Institute script available at:
# https://github.com/Maryland-Health-Benefits-Exchange/covid-neighborhood-job-analysis
# update 2v4d update to use alt lines listed under "for 5-year:" comment
# rename to "ipums-data-merge-puma.Rds" to differentiate
# from statewide level analysis input file
# then read in the output file "ipums-data-merge-puma.Rds"
ipums_data_merge <- readRDS("data/processed/ipums-data-merge-puma.Rds")
acs_df <- ipums_data_merge %>% clean_names()

# verify that every PUMA in Maryland exists in the ACS data extract, PUMAs for
# other states can be found here: 
# https://www2.census.gov/geo/pdfs/reference/puma/2010_PUMA_Names.pdf 
assert("IPUMS extract has all PUMA's in Maryland",
       acs_df %>% select(state, puma) %>% distinct() %>% nrow() == 44)

# generate grouping variables, filters, geo-joining variable
acs_df <- acs_df %>%
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
  mutate(puma = sprintf("%05d", puma)) %>%
  # recode hours worked variable to be a flag for full- (2) or part- (1) time work
  #mutate(uhrswork = ifelse(uhrswork<40,1,2)) %>%
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
# Note: checked DHS totals for LPRs for Maryland (260,000 or 2%) and verified
# num of LPRs using below methodology (lawful==1 & citizen==3) which was about
# equivalent (110187 of 5937673 or ~2%). Therefore, unlikely that controlling
# by DHS estimates would improve estimation of lawful presence.
# ref: https://www.dhs.gov/sites/default/files/publications/lpr_population_estimates_january_2015.pdf


# obj w/ occupation codes associated to jobs likely to require legal status
# source list provided by CMSNY, kept private at their request
occupations <- readRDS("data/private/occupations.Rds")

# create variable that captures whether rec'd public assistance
acs_df <- acs_df %>% mutate_at(vars(incss, incwelfr, incsupp),
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
                              #age > 18 ~ 1, # older than 18
                             TRUE ~ 0))
# drop income vars as they are no longer needed
acs_df <- subset(acs_df, select = -c(incss, incwelfr, incsupp)) 

saveRDS(acs_df, file = "data/processed/acs_df.Rds") # save to processed folder

################################################################################
# next steps:
# determine eligibility of ESI-loss uninsured pop & add those ineligible for 
# Medicaid/QHP to uninsured totals for each PUMA
# aggregate job loss imputations from modified Urban Institute analysis to PUMA,
# industry

################################################################################
####### Analysis: Health Insurance Breakdowns by PUMA and Demographics #########
################################################################################

###################################### pre-COVID uninsured population
# summary df w/ all health ins vars, grouped by age and puma
df.sum.age.2019 <- acs_df %>%
  # uncomment to remove unauthorized immigrants
  filter(lawful == 1) %>%
  group_by(puma, agegroup) %>%
  summarize(
    tpop = sum(perwt),
    insured = sum(ifelse(hcovany==2, perwt, 0)),
    uninsured = sum(ifelse(hcovany==1, perwt, 0)),
    ins_emp = sum(ifelse(hinsemp==2, perwt, 0)),
    # uncomment to calc public/private insurance coverage
    #ins_pub = sum(ifelse(hcovpub==2, perwt,0)),
    #ins_priv = sum(ifelse(hcovpriv==2, perwt, 0)),
    #unweight_per = n()
  ) %>% 
  mutate(pct_uninsured = uninsured/tpop)
saveRDS(df.sum.age, file = "data/processed/df-sum-age.Rds") # save to processed folder

# summary df of HI vars, grouped by puma only
df.summary <- acs_df %>%
  # uncomment to remove unauthorized immigrants
   filter(lawful == 1) %>%
  group_by(puma) %>%
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
saveRDS(df.summary, file = "data/processed/df-summary.Rds") # save to processed folder

# summary df of HI vars, grouped by puma only, unfiltered
df.summ.unfilt <- acs_df %>%
  # uncomment to remove unauthorized immigrants
  #filter(lawful == 1) %>%
  group_by(puma) %>%
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
saveRDS(df.summ.unfilt, file = "data/processed/df-summ-unfilt.Rds") # save to processed folder

# summary df w/ all health ins vars, grouped by industry and puma
df.sum.ind <- acs_df %>%
  # uncomment to remove unauthorized immigrants
  filter(lawful == 1) %>%
  group_by(puma, ind) %>%
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
saveRDS(df.sum.ind, file = "data/processed/df-sum-ind.Rds")

###################################################### net enrollment analysis
# this section imports enrollment data from MHC that is not publicly available
# any replication of this analysis will need to use state-specific data
# use separate script "net_enrollment_analysis.R" to perform necessary analysis
source("code - mine/net_enrollment_analysis.R", encoding = "UTF-8")

###################################### estimating uptake of spousal insurance
# this section seeks to tabulate how many people who lost ESI will enroll in
# spousal insurance based on: presence of spouse/partner (sp), sp employment status,
# sp ESI status. Conditional statements identify eligible proportion which is 
# then converted to a percent of total ESI loss for each row
# use separate script "household_analysis.R" to perform necessary analysis
source("code - mine/household_analysis.R", encoding = "UTF-8")


###################################### eligibility breakdowns

# characteristics of employer-sponsored insurance covered population
df.esi.pop <- acs_df %>%
  # uncomment to remove unauthorized immigrants
  filter(lawful == 1) %>%
  group_by(puma) %>%
  # summarize the ESI covered population by eligibility for QHP, Medicare/caid
  summarize(tpop_esi = sum(ifelse(hinsemp==2,perwt,0)), # all below have ESI
            # those eligible for medicaid due to income or age
            caid_elig = sum(ifelse(hinsemp==2 & (poverty<138 | age<19), perwt, 0)),
            # young adults (19-34) eligible for subsidized QHP due to income
            ya_qhp_elig = sum(ifelse(hinsemp==2 & agegroup=="19 - 34" & 
                                       poverty>138 & poverty<400, perwt, 0)),
            # older adults (35-64) eligible for subsidized QHP due to income
            a_qhp_elig = sum(ifelse(hinsemp==2 & agegroup=="35 - 64" & 
                                      poverty>138 & poverty<400, perwt, 0)),
            # all adults (18-64) eligible for unsubsidized QHP due to income
            unsub_qhp_elig = sum(ifelse(hinsemp==2 & age>18 & age<65 & 
                                          poverty>399, perwt, 0)),
            # elderly adults eligible for medicare
            care_elig = sum(ifelse(hinsemp==2 & agegroup=="65 +", perwt, 0))
  )
saveRDS(df.esi.pop, file = "data/processed/df-esi-pop.Rds")

# characteristics of the uninsured population, unadjusted
df.unins.pop <- acs_df %>%
  # uncomment to remove unauthorized immigrants
  #filter(lawful == 1) %>%
  group_by(puma) %>%
  # summarize the uninsured population by eligibility for QHP, Medicare/caid
  summarize(tpop = sum(perwt),
            uninsured = sum(ifelse(hcovany==1,perwt,0)), # all below are unins
            # only lawfully present uninsured
            unins_mhc = sum(ifelse((hcovany==1 & lawful==1),perwt,0)),
            # those eligible for medicaid due to income or age
            caid_elig = sum(ifelse(hcovany==1 & (poverty<138 | age<19), perwt, 0)),
            # young adults (19-34) eligible for subsidized QHP due to income
            ya_qhp_elig = sum(ifelse(hcovany==1 & agegroup=="19 - 34" & 
                                       poverty>138 & poverty<400, perwt, 0)),
            # older adults (35-64) eligible for subsidized QHP due to income
            a_qhp_elig = sum(ifelse(hcovany==1 & agegroup=="35 - 64" & 
                                      poverty>138 & poverty<400, perwt, 0)),
            # all adults (18-64) eligible for unsubsidized QHP due to income
            unsub_qhp_elig = sum(ifelse(hcovany==1 & age>18 & age<65 & 
                                          poverty>399, perwt, 0)),
            # elderly adults eligible for medicare
            care_elig = sum(ifelse(hcovany==1 & agegroup=="65 +", perwt, 0)),
            # employment stats
            ins_emp = sum(ifelse(hinsemp==2,perwt,0)),
            total_emp_pre = sum(total_employment),
            total_unemp_post = sum(total_disemployment),
            
  ) %>% 
  mutate(pct_uninsured = uninsured/tpop) %>%
  ungroup() %>%
  # calc the % change in employment
  mutate(pct_change_imputed = ifelse(total_emp_pre == 0,
                                         0,
                                         total_unemp_post / total_emp_pre)) %>%
  group_by(puma) %>%
  # calc the number of ppl potentially losing ESI coverage
  mutate(esi_loss = ins_emp * pct_change_imputed) %>%
  # convert positive esi_loss to 0 for ease of calculations
  mutate(esi_loss = ifelse(esi_loss<0, 0, esi_loss)) %>%
  # the new uninsured rate including all the folks who lost esi less those w/ spousal/parent
  # ESI available (uncomment to adjust for uptake of spousal insurance)
  # join to spousal uptake df
  left_join(acs_df_hh_puma[,c("puma","pct.uptake")], by = "puma") %>%
  # adjust by pct that will enroll in spousal ins
  mutate(esi_loss = esi_loss-(esi_loss*pct.uptake)) %>%
  mutate(new_unins = uninsured + esi_loss) %>%
  # join to enrollment totals and subtract net enrollment
  # (uncomment to adjust for enrollment)
  left_join(puma.enrt.df,by=c("puma"="puma12")) %>%
  mutate(new_unins = new_unins - grand_total) %>%
  mutate(new_pct_unins = new_unins/tpop) %>%
  # drop employment vars for readability
  select(-c(total_emp_pre, total_unemp_post))

saveRDS(df.unins.pop, file = "data/processed/df-unins-pop.Rds")

# characteristics of the uninsured population, filtered to remove MHC inelig
df.unins.pop.filt <- acs_df %>%
  # uncomment to remove unauthorized immigrants
  filter(lawful == 1) %>%
  group_by(puma) %>%
  # summarize the uninsured population by eligibility for QHP, Medicare/caid
  summarize(tpop = sum(perwt),
            uninsured = sum(ifelse(hcovany==1,perwt,0)), # all below are unins
            # only lawfully present uninsured
            #unins_mhc = sum(ifelse((hcovany==1 & lawful==1),perwt,0)),
            # those eligible for medicaid due to income or age
            caid_elig = sum(ifelse(hcovany==1 & (poverty<138 | age<19), perwt, 0)),
            # young adults (19-34) eligible for subsidized QHP due to income
            ya_qhp_elig = sum(ifelse(hcovany==1 & agegroup=="19 - 34" & 
                                       poverty>138 & poverty<400, perwt, 0)),
            # older adults (35-64) eligible for subsidized QHP due to income
            a_qhp_elig = sum(ifelse(hcovany==1 & agegroup=="35 - 64" & 
                                      poverty>138 & poverty<400, perwt, 0)),
            # all adults (18-64) eligible for unsubsidized QHP due to income
            unsub_qhp_elig = sum(ifelse(hcovany==1 & age>18 & age<65 & 
                                          poverty>399, perwt, 0)),
            # elderly adults eligible for medicare
            care_elig = sum(ifelse(hcovany==1 & agegroup=="65 +", perwt, 0)),
            # employment stats
            ins_emp = sum(ifelse(hinsemp==2,perwt,0)),
            total_emp_pre = sum(total_employment),
            total_unemp_post = sum(total_disemployment)
  ) %>% 
  mutate(pct_uninsured = uninsured/tpop) %>%
  ungroup() %>%
  # calc the % change in employment
  mutate(pct_change_imputed = ifelse(total_emp_pre == 0,
                                     0,
                                     total_unemp_post / total_emp_pre)) %>%
  group_by(puma) %>%
  # calc the number of ppl potentially losing ESI coverage
  mutate(esi_loss = ins_emp * pct_change_imputed) %>%
  # convert positive esi_loss to 0 for ease of calculations
  mutate(esi_loss = ifelse(esi_loss<0, 0, esi_loss))  %>%
  # join to spousal uptake df
  left_join(acs_df_hh_puma[,c("puma","pct.uptake")], by = "puma") %>%
  mutate(esi_loss = esi_loss-(esi_loss*pct.uptake)) %>%
  # the new uninsured rate including newly uninsured due to esi loss
  mutate(new_unins = uninsured + esi_loss) %>%
  # join to enrollment totals and subtract net enrollment
  left_join(puma.enrt.df,by=c("puma"="puma12")) %>%
  mutate(new_unins = new_unins - grand_total) %>%
  mutate(new_pct_unins = new_unins/tpop) %>%
  mutate(new_pct_unins = new_unins/tpop) %>%
  # drop enrollment vars for readability
  select(-c(ma, qhp, grand_total))

saveRDS(df.unins.pop.filt, file = "data/processed/df-unins-pop-filt.Rds")


################################################################################
########################## Maps, plots, and figures ############################
################################################################################

# basic uninsured map
# uncomment to generate map shape from file generated at https://www.nhgis.org/
# puma.shape <- read_ipums_sf("data/nhgis0001_shapefile_tl2018_us_puma_2018.zip", 
#                             verbose = F)
# puma.shape <- puma.shape %>% filter(puma.shape$STATEFP10=="24")
# 
# decided to instead use pre-formatted and MD-only shapefile from TIGRIS
# create puma shapefile using Tigris library
puma.sf <- pumas(state = "Maryland", cb = FALSE)
puma.sf <- st_transform(puma.sf, 4326) # convert to projection used by leaflet
saveRDS(puma.sf, file = "data/processed/puma-sf.Rds")

###################################### basic summary map
# join data to shapefile by puma
joined <- ipums_shape_inner_join(df.unins.pop.filt, puma.sf, by=c("puma"="PUMACE10"))
# define palette to use, use range of uninsured values for scale of colors
pal <- colorNumeric(palette = "YlGnBu", domain = 3000:10000)
# create simple popup with PUMA name and total uninsured estimate
popup <- paste0("Area: ", joined$NAMELSAD10, "<br>", "Uninsured: ", round(joined$uninsured,0))
# generate leaflet object, define basemap
Unins_PUMA_map <- leaflet() %>% addProviderTiles("CartoDB.Positron") %>% 
  # add shapes for uninsured totals, define color fill palette
  addPolygons(data=joined, fillColor = ~pal(uninsured), 
              # outline color, fill opacity, outline weight
              color = "#444444", fillOpacity = 0.7, weight = 1, 
              # white outline on hover or click
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              # smooth out shapes for better performance, define popup params
              smoothFactor = 0.2, popup = popup, label = joined$NAMELSAD10
  ) %>% 
  # add simple legend with color scale and labels in bottom right corner
  addLegend(pal = pal, values = joined$uninsured, position = "bottomright", 
            title = "Uninsured by PUMA")
# save as widget for easy access
saveWidget(widget = Unins_PUMA_map,
           file = "Unins_PUMA_map.html",
           selfcontained = TRUE)


###################################### basic ESI map
joined <- ipums_shape_inner_join(df.unins.pop.filt, puma.sf, by=c("puma"="PUMACE10"))
# create color palette based on ESI coverage bins
pal <- colorNumeric(palette = "YlGnBu", domain = joined$ins_emp)
# specify what displays on the popup when the PUMA is clicked
popup <- paste0("Area: ", joined$NAMELSAD10, "<br>", "Employer-Sponsored Insurance: ", 
                joined$ins_emp)
# generate interactive leaflet map w/ generic basemap
ESI.puma.map <- leaflet() %>% addProviderTiles("CartoDB.Positron") %>% 
  # specify data and color palette
  addPolygons(data=joined, fillColor = ~pal(ins_emp), 
              # outline color specifications
              color = "#444444", fillOpacity = 0.7, weight = 1, 
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              # assign smoothness and popup specifications
              smoothFactor = 0.2, popup = popup, label = joined$NAMELSAD10) %>% 
  # set up legend to show value-color combos based on ESI totals
  addLegend(pal = pal, values = joined$ins_emp, position = "bottomright", 
            title = "ESI by PUMA")
saveWidget(widget = ESI.puma.map,
           file = "ESI-puma-map.html",
           selfcontained = TRUE)

###################################### uninsured by county map
# read in PUMA to County crosswalk
geocorrP2C <- read_csv("data/raw/geocorr2018.csv")
geocorrP2C <- geocorrP2C[-1,]
geocorrP2C <- geocorrP2C %>% mutate(puma = sprintf("%05d", as.numeric(puma12)))
# join crosswalk to unins df by PUMA
map3 <- left_join(df.unins.pop.filt, geocorrP2C, by = "puma")
# calculate unins per PUMA using allocation factor
map3 <- map3 %>% mutate(unins.cnty = uninsured*as.numeric(afact))
# aggregate up to puma level, summing the allocated claims
cnty.map.unins <- map3 %>% group_by(county) %>% summarise(uninsured.cnty = sum(unins.cnty, na.rm = TRUE))
cnty.sf <- tigris::counties(state = "Maryland", cb = FALSE, year=2019)
cnty.sf <- st_transform(cnty.sf, 4326) # convert to projection used by leaflet
saveRDS(cnty.sf, file = "data/processed/cnty-sf.Rds")
joined.cnty <- ipums_shape_inner_join(cnty.map.unins, cnty.sf, by=c("county"="GEOID"))
pal <- colorNumeric(palette = "YlGnBu", domain = joined.cnty$uninsured.cnty)
popup <- paste0("Area: ", joined.cnty$NAMELSAD, "<br>", "Uninsured: ", joined.cnty$uninsured.cnty)

# interactive map
leaflet(joined.cnty) %>% 
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.2,
              fillOpacity = 0.7, fillColor = ~pal(uninsured.cnty),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~popup) %>%
  addLegend("bottomright", pal = pal, values = ~uninsured.cnty,
            title = "Uninsured by County",
            opacity = 1)

# pre-COVID
# static map
static.map.cnty.pre <- ggplot(joined.cnty) +
  geom_sf(color="white", aes(fill = uninsured.cnty)) +
  theme(panel.grid.major = element_line(colour = "transparent")) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, name = "Estimate") +
  labs(title = "Estimated Uninsured in Maryland by County", caption = "Source: IPUMS USA 5-year ACS 2019") +
  geom_sf_label(aes(label = round(joined.cnty$uninsured.cnty,0)),
             label.padding = unit(1,"mm"))
# post-COVID
# join crosswalk to unins df by ZIP
map3 <- left_join(df.unins.pop.filt, geocorrP2C, by = "puma")
# calculate unins per PUMA using allocation factor
map3 <- map3 %>% mutate(unins.cnty = new_unins*as.numeric(afact))
# aggregate up to puma level, summing the allocated claims
cnty.map.unins <- map3 %>% group_by(county) %>% 
  summarise(uninsured.cnty = sum(unins.cnty, na.rm = TRUE))
# join shapefile to data
joined.cnty <- ipums_shape_inner_join(cnty.map.unins, cnty.sf, by=c("county"="GEOID"))
# define palette and popups
bins <- c(0,1000,5000,10000,20000,40000,60000,80000)
pal <- colorBin(palette = "YlGnBu", domain = joined.cnty$uninsured.cnty, bins = bins)
popup <- paste0("Area: ", joined.cnty$NAMELSAD, "<br>", "Uninsured: ", 
                format(round(joined.cnty$uninsured.cnty,0), big.mark = ",", trim=TRUE))

leaflet(joined.cnty, height = 400) %>% 
  addProviderTiles("CartoDB.Positron", group = "CartoDB (Default)") %>% 
  addProviderTiles("Esri.WorldStreetMap", group = "Esri") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "CartoDB Dark") %>%
  addPolygons(data=joined.cnty, fillColor = ~pal(uninsured.cnty),
              # outline color, fill opacity, outline weight
              color = "#444444", fillOpacity = 0.6, weight = 1, 
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              smoothFactor = 0.2, popup = ~popup, label = joined.cnty$NAMELSAD) %>%
  addLegend("bottomright", pal = pal, values = ~uninsured.cnty,
            title = "Eligible Uninsured by County") %>%
  addLayersControl(
    baseGroups = c("CartoDB (Default)", "Esri", "CartoDB Dark"),
    options = layersControlOptions(collapsed = FALSE)
  )
# static map
static.map.cnty.post <- ggplot(joined.cnty) +
  geom_sf(color="white", aes(fill = uninsured.cnty)) +
  theme(panel.grid.major = element_line(colour = "transparent")) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, name = "Estimate") +
  labs(title = "Potential Estimated Uninsured in Maryland by County, Post COVID ESI Losses", 
       caption = "Source: IPUMS USA 5-year ACS 2019, BLS 2020 CES") +
  geom_sf_label(aes(label = round(joined.cnty$uninsured.cnty,0)),
                label.padding = unit(1,"mm"))



###################################### Unemployment by ZIP code map
# Can use UI claims that occured during the pandemic (3/1-9/12) as another proxy
# for job loss to provide another measure of ESI loss
# read in Unemployment Claims from 3/1-9/12 by ZIP code, created by Ajani Pierce
# at Maryland Dept. of Labor (LABOR)
# using ArcGIS URL ensures up-to-date data
url <- paste0("https://services.arcgis.com/njFNhDsUCentVYJW/ArcGIS/rest/services/",
              "Regular_Claims_08222020/FeatureServer/1")
df <- esri2sf(url)
saveRDS(df, file = "data/processed/df.Rds")
pal = colorNumeric("YlOrRd", domain = df$Claims)
# Setting up the pop up text
popup <- paste0("Area: ", df$ZIPName, "<br>", "Total UI Claims: ", 
                df$Claims)
# map UI claims by ZIP
UI_ZIP_map <- leaflet(df) %>% 
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~pal(Claims),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~popup, label = df$ZIPName) %>%
  addLegend("bottomright", pal = pal, values = ~Claims,
            title = "UI by ZIP code",
            opacity = 1
  )
# saveWidget(widget = UI_ZIP_map,
#            file = "code/output/UI_ZIP_map.html",
#            selfcontained = TRUE)

# isolate just the claims as a dataframe
claims <- as.data.frame(select(df, c("ZIPCODE1", "ZIPName", 
                                     "Claim_Type", "ZipCode", "Claims", "County")
                               ))
# drop the geo metadata
claims <- claims[,-7]
# read in ZIP to PUMA crosswalk
geocorr2018 <- read_csv("data/raw/geocorr_zip_to_PUMA.csv")
# join crosswalk to claims df by ZIP
map <- left_join(claims, geocorr2018, by = c("ZIPCODE1"="zcta5"))
# calculate claims per PUMA using allocation factor
map <- map %>% mutate(puma.claims = Claims*as.numeric(afact))
# aggregate up to puma level, summing the allocated claims
puma.map <- map %>% group_by(puma12) %>% summarise(claims = sum(puma.claims, 
                                                                na.rm = TRUE))

###################################### Unemployment by PUMA map
# join puma level data to puma shapefile
my.map <- geo_join(puma.sf, puma.map, "PUMACE10", "puma12", how="left")
pal = colorNumeric("YlOrRd", domain = my.map$claims)
# Setting up the pop up text
popup <- paste0("Area: ", my.map$NAMELSAD10, "<br>", "Total UI Claims: ", 
                round(my.map$claims,0))
# map UI by PUMA
UI_PUMA_map <- leaflet(my.map) %>% 
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~pal(claims),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~popup, label = my.map$NAMELSAD10) %>%
  addLegend("bottomright", pal = pal, values = ~claims,
            title = "UI by PUMA",
            opacity = 1
  )
saveWidget(widget = UI_PUMA_map,
           file = "UI_PUMA_map.html",
           selfcontained = TRUE)

###################################### Tables and Graphs
########################### Pre-COVID job losses
# eligibility table
# pre-COVID job losses
# characteristics of the uninsured population
df.elig.pre <- acs_df %>%
  # uncomment to remove unauthorized immigrants
  # filter(lawful == 1) %>%
  mutate(puma = case_when( puma=="00100" ~ "Allegany & Garrett Counties--Cumberland City",
                               puma=="00200" ~ "Washington County--Hagerstown City",
                               puma=="00301" ~ "Frederick County (Outside Greater Frederick City)",
                               puma=="00302" ~ "Frederick County (Central)--Greater Frederick City",
                               puma=="00400" ~ "Carroll County",
                               puma=="00501" ~ "Baltimore County (Outer)",
                               puma=="00502" ~ "Baltimore County--Randallstown (East), Owings Mills, Milford Mill & Reisterstown",
                               puma=="00503" ~ "Baltimore County--Pikesville (South), Lochearn, Cockeysville & Mays Chapel",
                               puma=="00504" ~ "Baltimore County--Towson (East & Central), Parkville & Carney",
                               puma=="00505" ~ "Baltimore County--Perry Hall, Middle River & Rosedale",
                               puma=="00506" ~ "Baltimore County--Dundalk, Essex & Edgemere",
                               puma=="00507" ~ "Baltimore County--Catonsville, Woodlawn & Arbutus",
                               puma=="00601" ~ "Harford County (North & West)--Bel Air Town, Fallston & Jarrettsville",
                               puma=="00602" ~ "Harford County (South & East)--Aberdeen & Havre de Grace Cities",
                               puma=="00700" ~ "Cecil County",
                               puma=="00801" ~ "Baltimore City--Sandtown-Winchester, Ashburton & Mount Washington",
                               puma=="00802" ~ "Baltimore City--Guilford, Roland Park & Druid Lake",
                               puma=="00803" ~ "Baltimore City--Frankford, Belair-Edison & Loch Raven",
                               puma=="00804" ~ "Baltimore City--Inner Harbor, Canton & Bayview",
                               puma=="00805" ~ "Baltimore City--Irvington, Ten Hills & Cherry Hill",
                               puma=="00901" ~ "Howard County (West)--Columbia (West) & Ellicott City (Northwest)",
                               puma=="00902" ~ "Howard County (East)--Columbia (East), Ellicott City (Southeast) & Elkridge",
                           puma=="01001" ~ "Montgomery County (North & West)--Olney, Damascus, Clarksburg & Darnestown",
                           puma=="01002" ~ "Montgomery County (West Central)--Germantown & Montgomery Village",
                           puma=="01003" ~ "Montgomery County (Central)--Rockville, Gaithersburg Cities & North Potomac",
                           puma=="01004" ~ "Montgomery County (South)--Bethesda, Potomac & North Bethesda",
                           puma=="01005" ~ "Montgomery County (East Central)--Wheaton, Aspen Hill & Glenmont",
                           puma=="01006" ~ "Montgomery County (East)--Fairland, Calverton, White Oak & Burtonsville",
                           puma=="01007" ~ "Montgomery County (Southeast)--Takoma Park City & Silver Spring",
                           puma=="01101" ~ "Prince George's County (Northwest)--College Park City & Langley Park",
                           puma=="01102" ~ "Prince George's County (North)--Laurel, Greenbelt (North & East) Cities & Beltsville",
                           puma=="01103" ~ "Prince George's County (Northwest)--New Carrollton & Hyattsville (Southeast) Cities",
                           puma=="01104" ~ "Prince George's County (Central)--Seat Pleasant City, Capitol Heights Town & Landover",
                           puma=="01105" ~ "Prince George's County (East)--Bowie City, Kettering, Largo, Mitchellville & Lanham",
                           puma=="01106" ~ "Prince George's County (South)--Clinton, Fort Washington (South), Rosaryville & Croom",
                           puma=="01107" ~ "Prince George's County (Southwest)--Oxon Hill, Hillcrest Heights & Temple Hills",
                           puma=="01201" ~ "Anne Arundel County (Northwest)--Severn, Odenton, Crofton, Maryland City & Fort Meade",
                           puma=="01202" ~ "Anne Arundel County (North)--Glen Burnie, Pasadena, Ferndale & Brooklyn Park",
                           puma=="01203" ~ "Anne Arundel County (Central)--Severna Park, Arnold & Lake Shore",
                           puma=="01204" ~ "Anne Arundel County (Southeast)--Annapolis City, Parole, Annapolis Neck & Edgewater",
                           puma=="01300" ~ "Queen Anne's, Talbot, Caroline, Dorchester & Kent Counties",
                           puma=="01400" ~ "Wicomico, Worcester & Somerset Counties--Salisbury City",
                           puma=="01500" ~ "St. Mary's & Calvert Counties",
                           puma=="01600" ~ "Charles County--La Plata Town & Waldorf "
  )) %>%
  group_by(puma) %>%
  # summarize the uninsured population by eligibility for QHP, Medicare/caid
  summarize(# elig for financial assistance (APTC)
            # those eligible for medicaid due to income or age
            MA = sum(ifelse(hcovany==1 & lawful==1 & poverty<138 & age>18,
                            perwt, 0)),
            # adults eligible for subsidized QHP due to income
            aQHP = sum(ifelse(hcovany==1 & lawful==1 & age>18 & age<65 & 
                                       poverty>138 & poverty<400, perwt, 0)),
            # children eligible for either MA or subsidized QHP
            cQHP = sum(ifelse(hcovany==1 & lawful==1 & age<18 & 
                                          poverty<400, perwt, 0)),
            # total uninsured eligible for financial assistance
            # change to just sum the other categories to avoid confusion
            tot.sub = sum(MA, aQHP, cQHP),
            # ineligible for financial assistance
            # income above 400% FPL
            uQHP = sum(ifelse(hcovany==1 & lawful==1 & poverty>399, 
                                    perwt, 0)),
            # offer of employer-sponsored insurance (8.9% per Urban Institute)
            unins.esi = round(sum(ifelse(hcovany==1 & lawful==1, 
                                         perwt, 0))*0.089,0),
            tot.unsub = round(uQHP+unins.esi,0)
            # ineligible to enroll through MHBE due to undocumented status
            #unins.inelig = sum(ifelse(hcovany==1 & lawful==0, perwt, 0))
  )
saveRDS(df.elig.pre, file = "data/processed/df-elig-pre.Rds")

############################# Post-COVID job losses
# eligibility table
# pre-COVID job losses
# characteristics of the uninsured population
df.elig.post <- acs_df %>%
  # uncomment to remove unauthorized immigrants
  # filter(lawful == 1) %>%
  mutate(puma = case_when( puma=="00100" ~ "Allegany & Garrett Counties--Cumberland City",
                           puma=="00200" ~ "Washington County--Hagerstown City",
                           puma=="00301" ~ "Frederick County (Outside Greater Frederick City)",
                           puma=="00302" ~ "Frederick County (Central)--Greater Frederick City",
                           puma=="00400" ~ "Carroll County",
                           puma=="00501" ~ "Baltimore County (Outer)",
                           puma=="00502" ~ "Baltimore County--Randallstown (East), Owings Mills, Milford Mill & Reisterstown",
                           puma=="00503" ~ "Baltimore County--Pikesville (South), Lochearn, Cockeysville & Mays Chapel",
                           puma=="00504" ~ "Baltimore County--Towson (East & Central), Parkville & Carney",
                           puma=="00505" ~ "Baltimore County--Perry Hall, Middle River & Rosedale",
                           puma=="00506" ~ "Baltimore County--Dundalk, Essex & Edgemere",
                           puma=="00507" ~ "Baltimore County--Catonsville, Woodlawn & Arbutus",
                           puma=="00601" ~ "Harford County (North & West)--Bel Air Town, Fallston & Jarrettsville",
                           puma=="00602" ~ "Harford County (South & East)--Aberdeen & Havre de Grace Cities",
                           puma=="00700" ~ "Cecil County",
                           puma=="00801" ~ "Baltimore City--Sandtown-Winchester, Ashburton & Mount Washington",
                           puma=="00802" ~ "Baltimore City--Guilford, Roland Park & Druid Lake",
                           puma=="00803" ~ "Baltimore City--Frankford, Belair-Edison & Loch Raven",
                           puma=="00804" ~ "Baltimore City--Inner Harbor, Canton & Bayview",
                           puma=="00805" ~ "Baltimore City--Irvington, Ten Hills & Cherry Hill",
                           puma=="00901" ~ "Howard County (West)--Columbia (West) & Ellicott City (Northwest)",
                           puma=="00902" ~ "Howard County (East)--Columbia (East), Ellicott City (Southeast) & Elkridge",
                           puma=="01001" ~ "Montgomery County (North & West)--Olney, Damascus, Clarksburg & Darnestown",
                           puma=="01002" ~ "Montgomery County (West Central)--Germantown & Montgomery Village",
                           puma=="01003" ~ "Montgomery County (Central)--Rockville, Gaithersburg Cities & North Potomac",
                           puma=="01004" ~ "Montgomery County (South)--Bethesda, Potomac & North Bethesda",
                           puma=="01005" ~ "Montgomery County (East Central)--Wheaton, Aspen Hill & Glenmont",
                           puma=="01006" ~ "Montgomery County (East)--Fairland, Calverton, White Oak & Burtonsville",
                           puma=="01007" ~ "Montgomery County (Southeast)--Takoma Park City & Silver Spring",
                           puma=="01101" ~ "Prince George's County (Northwest)--College Park City & Langley Park",
                           puma=="01102" ~ "Prince George's County (North)--Laurel, Greenbelt (North & East) Cities & Beltsville",
                           puma=="01103" ~ "Prince George's County (Northwest)--New Carrollton & Hyattsville (Southeast) Cities",
                           puma=="01104" ~ "Prince George's County (Central)--Seat Pleasant City, Capitol Heights Town & Landover",
                           puma=="01105" ~ "Prince George's County (East)--Bowie City, Kettering, Largo, Mitchellville & Lanham",
                           puma=="01106" ~ "Prince George's County (South)--Clinton, Fort Washington (South), Rosaryville & Croom",
                           puma=="01107" ~ "Prince George's County (Southwest)--Oxon Hill, Hillcrest Heights & Temple Hills",
                           puma=="01201" ~ "Anne Arundel County (Northwest)--Severn, Odenton, Crofton, Maryland City & Fort Meade",
                           puma=="01202" ~ "Anne Arundel County (North)--Glen Burnie, Pasadena, Ferndale & Brooklyn Park",
                           puma=="01203" ~ "Anne Arundel County (Central)--Severna Park, Arnold & Lake Shore",
                           puma=="01204" ~ "Anne Arundel County (Southeast)--Annapolis City, Parole, Annapolis Neck & Edgewater",
                           puma=="01300" ~ "Queen Anne's, Talbot, Caroline, Dorchester & Kent Counties",
                           puma=="01400" ~ "Wicomico, Worcester & Somerset Counties--Salisbury City",
                           puma=="01500" ~ "St. Mary's & Calvert Counties",
                           puma=="01600" ~ "Charles County--La Plata Town & Waldorf "
  )) %>%
  group_by(puma) %>%
  # summarize the uninsured population by eligibility for QHP, Medicare/caid
  summarize(# elig for financial assistance (APTC)
    # those eligible for medicaid due to income or age
    MA = sum(ifelse(hcovany==1 & lawful==1 & poverty<138 & age>18,
                    perwt, 0)),
    # same group, number of ppl w/ esi
    MA.esi = sum(ifelse(hinsemp==2 & lawful==1 & poverty<138 & age>18,
                        perwt,0)),
    # adults eligible for subsidized QHP due to income
    aQHP = sum(ifelse(hcovany==1 & lawful==1 & age>18 & age<65 & 
                        poverty>138 & poverty<400, perwt, 0)),
    aQHP.esi = sum(ifelse(hinsemp==2 & lawful==1 & age>18 & age<65 & 
                        poverty>138 & poverty<400, perwt, 0)),
    # children eligible for either MA or subsidized QHP
    cQHP = sum(ifelse(hcovany==1 & lawful==1 & age<18 & 
                        poverty<400, perwt, 0)),
    cQHP.esi = sum(ifelse(hinsemp==2 & lawful==1 & age<18 & 
                        poverty<400, perwt, 0)),
    # total uninsured eligible for financial assistance
    tot.sub = sum(ifelse(hcovany==1 & lawful==1 & poverty<400 & age<65, perwt, 0)),
    tot.sub.esi = sum(ifelse(hinsemp==2 & lawful==1 & poverty<400, perwt, 0)),
    # ineligible for financial assistance
    # income above 400% FPL
    uQHP = sum(ifelse(hcovany==1 & lawful==1 & poverty>399, 
                      perwt, 0)),
    uQHP.esi = sum(ifelse(hinsemp==2 & lawful==1 & poverty>399, 
                      perwt, 0)),
    # offer of employer-sponsored insurance (8.9% per Urban Institute)
    unins.emp = round(sum(ifelse(hcovany==1 & lawful==1, 
                                 perwt, 0))*0.089,0),
    # ineligible to enroll through MHBE due to undocumented status
    unins.inelig = sum(ifelse(hcovany==1 & lawful==0, perwt, 0)),
    unins.inelig.esi = sum(ifelse(hinsemp==2 & lawful==0, perwt, 0)),
    # calculate employment/disemployment due to COVID-19
    total_emp_pre = sum(total_employment),
    total_unemp_post = sum(total_disemployment)
  ) %>%
  # calculate percent change in employment for each PUMA
  mutate(pct_change_imputed = ifelse(total_emp_pre == 0,
                                     0,
                                     total_unemp_post / total_emp_pre)) %>%
  # calculate the new figures for each category
  mutate(MA.esi = MA.esi * pct_change_imputed) %>%
  mutate(MA.esi = ifelse(MA.esi<0, 0, MA.esi))  %>%
  mutate(MA = round(MA+MA.esi,0)) %>%
  mutate(aQHP.esi = aQHP.esi * pct_change_imputed) %>%
  mutate(aQHP.esi = ifelse(aQHP.esi<0, 0, aQHP.esi))  %>%
  mutate(aQHP = round(aQHP+aQHP.esi,0)) %>%
  mutate(cQHP.esi = cQHP.esi * pct_change_imputed) %>%
  mutate(cQHP.esi = ifelse(cQHP.esi<0, 0, cQHP.esi))  %>%
  # mutate(cQHP = round(cQHP+cQHP.esi,0)) %>%
  # mutate(tot.sub.esi = tot.sub.esi * pct_change_imputed) %>%
  # mutate(tot.sub.esi = ifelse(tot.sub.esi<0, 0, tot.sub.esi))  %>%
  # change to just sum the other categories to avoid confusion
  mutate(tot.sub = MA+aQHP+cQHP) %>%
  mutate(uQHP.esi = uQHP.esi * pct_change_imputed) %>%
  mutate(uQHP.esi = ifelse(uQHP.esi<0, 0, uQHP.esi))  %>%
  mutate(uQHP = round(uQHP+uQHP.esi,0)) %>%
  mutate(unins.inelig.esi = unins.inelig.esi * pct_change_imputed) %>%
  mutate(unins.inelig.esi = ifelse(unins.inelig.esi>0, 0, unins.inelig.esi))  %>%
  mutate(unins.inelig = round(unins.inelig+unins.inelig.esi,0)) %>%
  mutate(tot.unsub = uQHP+unins.emp) %>%
  select(puma,MA,aQHP,cQHP,tot.sub,uQHP,unins.emp,tot.unsub)
  
  
saveRDS(df.elig.post, file = "data/processed/df-elig-post.Rds")

################################################################################
# statewide analysis using 1-year ACS file
# need to first run the modified Urban Institute script available at:
# https://github.com/Maryland-Health-Benefits-Exchange/covid-neighborhood-job-analysis
# update 2v4d update to use alt lines listed under "for 1-year:" comment
# rename to "ipums-data-merge-statewide.Rds" to differentiate
# from PUMA level analysis input file
# then read in the output file "ipums-data-merge-statewide.Rds"
source("code - mine/methodology_statewide.R", encoding = "UTF-8")

################################################################################
# use this to knit dashboard, normal knit button doesn't work
rmarkdown::render("COVID_Uninsured_Analysis_Dashboard.Rmd","flexdashboard::flex_dashboard")
