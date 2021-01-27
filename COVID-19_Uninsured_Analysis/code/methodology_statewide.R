# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # #   Maryland Health Benefit Exchange  # # # # # # # # # # # # 
# # # # # # # # # #                                     # # # # # # # # # # # # 
# # # # # # # # #  Program Name: methodology_statewide.R  # # # # # # # # # # # 
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
# IPUMS extract request: 2018 ACS 1yr (sample), Variables [YEAR, SAMPLE, 
# SERIAL, HHWT, CLUSTER, STRATA, COUNTYFIP, STATEFIP, METRO, PUMA, GQ, OWNERSHP, 
# OWNERSHPD, PERNUM, PERWT, FAMSIZE, RELATE, RELATED, SEX, AGE, RACE, RACED, 
# HISPAN, HISPAND, BPL, BPLD, CITIZEN, YRIMMIG, HCOVANY, HCOVPRIV, HCOVPUB, 
# HINSEMP, EMPSTAT, EMPSTATD, OCC, EDUC, INCSS, INCWELFR, INCSUPP, VETSTAT, IND, 
# POVERTY, MIGRATE1, MIGRATE1D, MARST] - limited to Maryland (STATEFIP==24)

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
# read in 1-year ACS data extract for statewide totals
acs_ddi_state <- read_ipums_ddi("data/raw/usa_00025.xml")
# read in data output using ddi to label and format
acs_data_state <- read_ipums_micro(acs_ddi_state) %>% clean_names()

# need to first run the modified Urban Institute script available at:
# https://github.com/Maryland-Health-Benefits-Exchange/covid-neighborhood-job-analysis
# update 2v4d update to use alt lines listed under "for 1-year:" comment
# rename to "ipums-data-merge-statewide.Rds" to differentiate
# from PUMA level analysis input file
# then read in the output file "ipums-data-merge-statewide.Rds"
ipums_data_merge_statewide <- readRDS("data/processed/ipums-data-merge-statewide.Rds")
acs_df_state <- ipums_data_merge_statewide %>% clean_names()

# verify that every PUMA in Maryland exists in the ACS data extract, PUMAs for
# other states can be found here: 
# https://www2.census.gov/geo/pdfs/reference/puma/2010_PUMA_Names.pdf 
assert("IPUMS extract has all PUMA's in Maryland",
       acs_df_state %>% select(state, puma) %>% distinct() %>% nrow() == 44)

# generate grouping variables, filters, geo-joining variable
acs_df_state <- acs_df_state %>%
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
acs_df_state <- acs_df_state %>% 
  mutate(incss = as.numeric(incss),
         incwelfr = as.numeric(incwelfr),
         incsupp = as.numeric(incsupp)) %>%
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
acs_df_state <- subset(acs_df_state, select = -c(incss, incwelfr, incsupp)) 

# potentially unlawfully present uninsured individuals
unlawful_unins <- acs_df_state %>% filter(lawful == 0 & hcovany == 1)
# potentially unlawfully present insured individuals
unlawful_ins <- acs_df_state %>% filter(lawful == 0 & hcovany == 2)

# output total unauthorized immigrants in sample for verification purposes
totUU <- unlawful_unins %>% summarize(sum(perwt)) # unauth uninsured
totUI <- unlawful_ins %>% summarize(sum(perwt)) # unauth insured
totU <- totUU + totUI # total unauthorized population
pct_Unins_Unauth <- totUU/totU # percent uninsured of the unauthorized pop

saveRDS(acs_df_state, file = "data/processed/acs_df_state.Rds") # save to processed folder

################################################################################
####### Analysis: Health Insurance Breakdowns by ESI and Demographics ##########
################################################################################

# characteristics of the uninsured population, filtered to only MHC eligible &
# number of folks that will lose esi overall, excluding those who could get ESI 
# through spouse employer and the net gain in Medicaid/QHP enrollments from March
# to September, obtained from MHBE data reports: 
# https://www.marylandhbe.com/news-and-resources/reportsdata/ 
# QHP: 148357 (Mar) - 158329 (Sep) = 9972 enrolled
# MA: 1105114 (Mar) - 1159076 (Sep) = 53962 enrolled
# Total Enrolled from March to September: 63,934
df.unins.pop.state.filt <- acs_df_state %>%
  # uncomment to remove unauthorized immigrants
  filter(lawful == 1) %>%
  group_by(state) %>%
  # summarize the uninsured population by eligibility for QHP, Medicare/caid
  summarize(tpop = sum(perwt),
            uninsured = sum(ifelse(hcovany==1,perwt,0)), # all below are unins
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
  ungroup() %>%
  mutate(pct_change_imputed = ifelse(total_emp_pre == 0,
                                     0,
                                     -total_unemp_post / total_emp_pre)) %>%
  group_by(state) %>%
  mutate(esi_loss = ins_emp * pct_change_imputed) %>%
  mutate(esi_loss = ifelse(esi_loss>0, 0, esi_loss))  %>%
  # the new uninsured rate including all the folks who lost esi (worst case)
  mutate(new_unins = uninsured - esi_loss + (esi_loss*.32)) %>%
  # remove mar-sep enrolled
  mutate(new_unins = new_unins - 63934) %>%
  mutate(pct_uninsured = uninsured/tpop) %>%
  mutate(new_pct_unins = new_unins/tpop) %>%
  # drop employment vars for readability
  select(-c(ins_emp, total_emp_pre, total_unemp_post))
saveRDS(df.unins.pop.state.filt, file = "data/processed/df-unins-pop-state-filt.Rds")

# job loss and insurance loss, by industry
df.ind.esi.loss <- acs_df_state %>%
  # change led_code to industry name for readability
  mutate(led_code = case_when( led_code==11 ~ "Agriculture, Forestry, Fishing, and Hunting",
                               led_code==21 ~ "Mining, Quarrying, and Oil and Gas Extractions",
                               led_code==22 ~ "Utilities",
                               led_code==23 ~ "Construction",
                               led_code==31 ~ "Manufacturing",
                               led_code==42 ~ "Wholesale Trade",
                               led_code==44 ~ "Retail Trade",
                               led_code==45 ~ "Retail Trade",
                               led_code==48 ~ "Transportation and Warehousing",
                               led_code==49 ~ "Transportation and Warehousing",
                               led_code==51 ~ "Information",
                               led_code==52 ~ "Finance and Insurance",
                               led_code==53 ~ "Real Estate and Rental and Leasing",
                               led_code==54 ~ "Professional, Scientific, and Technical Services",
                               led_code==55 ~ "Management of Companies and Enterprises",
                               led_code==56 ~ "Administrative and Support and Waste Management and Remediation Services",
                               led_code==61 ~ "Educational Services",
                               led_code==62 ~ "Health Care and Social Assistance",
                               led_code==71 ~ "Arts, Entertainment, and Recreation",
                               led_code==72 ~ "Accomodation and Food Services",
                               led_code==81 ~ "Other Services",
                               led_code==92 ~ "Public Administration"
  )) %>%
  # uncomment to remove unauthorized immigrants
  filter(lawful == 1) %>%
  group_by(led_code) %>%
  summarise(tpop = sum(perwt),
            insured = sum(ifelse(hcovany==2,perwt,0)),
            uninsured = sum(ifelse(hcovany==1,perwt,0)),
            ins_emp = sum(ifelse(hinsemp==2,perwt,0)),
            total_emp_pre = sum(total_employment),
            total_unemp_post = sum(total_disemployment)) %>%
  ungroup() %>%
  mutate(pct_change_imputed = ifelse(total_emp_pre == 0,
                                     0,
                                     -total_unemp_post / total_emp_pre)) %>%
  group_by(led_code) %>%
  mutate(esi_loss = ins_emp * pct_change_imputed) %>%
  mutate(esi_loss = ifelse(esi_loss>0, 0, esi_loss))  %>%
  # drop industries with NAs
  filter(!is.na(led_code))
saveRDS(df.ind.esi.loss, file = "data/processed/df-ind-esi-loss.Rds")

# number of folks that will lose esi overall, excluding those who could get ESI 
# through spouse employer and the net gain in Medicaid/QHP enrollments from March
# to September, obtained from MHBE data reports: 
# https://www.marylandhbe.com/news-and-resources/reportsdata/ 
# QHP: 148357 (Mar) - 158329 (Sep) = 9972 enrolled
# MA: 1105114 (Mar) - 1159076 (Sep) = 53962 enrolled
# Total Enrolled from March to September: 63,934
# this is unfiltered (includes unauthorized immigrants)

df.esi.loss.state.unfilt  <- acs_df_state %>%
  group_by(state) %>%
  summarise(tpop = sum(perwt),
            insured = sum(ifelse(hcovany==2,perwt,0)),
            uninsured = sum(ifelse(hcovany==1,perwt,0)),
            ins_emp = sum(ifelse(hinsemp==2,perwt,0)),
            total_emp_pre = sum(total_employment),
            total_unemp_post = sum(total_disemployment)) %>%
  ungroup() %>%
  mutate(pct_change_imputed = ifelse(total_emp_pre == 0,
                                     0,
                                     -total_unemp_post / total_emp_pre)) %>%
  group_by(state) %>%
  mutate(esi_loss = ins_emp * pct_change_imputed) %>%
  mutate(esi_loss = ifelse(esi_loss>0, 0, esi_loss))  %>%
  # adjust by est 32% that will enroll in spousal ESI
  mutate(new_unins = uninsured - esi_loss + (esi_loss*.32)) %>%
  # remove mar-sep net enrolled
  mutate(new_unins = new_unins - 63934) %>%
  mutate(new_pct_unins = new_unins/tpop) %>%
  mutate(new_ins_emp = ins_emp+(esi_loss-esi_loss*.32)) %>%
  mutate(new_insured = insured+(esi_loss-esi_loss*.32)+63934)
saveRDS(df.esi.loss.state.unfilt, file =  "data/processed/df-esi-loss-state-unfilt.Rds")

###################################### undocumented population
# not used in dashboard, just created for reference
# total unauthorized population
unauth.pop <- rbind(unlawful_ins,unlawful_unins)

# uninsurance, job loss, and esi loss experienced by unauthorized population
df.esi.loss.state.unauth <- unauth.pop %>%
  group_by(state) %>%
  summarise(tpop = sum(perwt),
            insured = sum(ifelse(hcovany==2,perwt,0)),
            uninsured = sum(ifelse(hcovany==1,perwt,0)),
            ins_emp = sum(ifelse(hinsemp==2,perwt,0)),
            total_emp_pre = sum(total_employment),
            total_unemp_post = sum(total_disemployment)) %>%
  ungroup() %>%
  mutate(pct_change_imputed = ifelse(total_emp_pre == 0,
                                     0,
                                     -total_unemp_post / total_emp_pre)) %>%
  group_by(state) %>%
  mutate(esi_loss = ins_emp * pct_change_imputed) %>%
  mutate(esi_loss = ifelse(esi_loss>0, 0, esi_loss))  %>%
  mutate(new_unins = uninsured - esi_loss) %>%
  mutate(new_pct_unins = new_unins/tpop)
saveRDS(df.esi.loss.state.unauth, file = "data/processed/df-esi-loss-state-unauth.Rds")

###################################### Tables and Graphs
########################### Pre-COVID job losses
# breakdown of uninsured by age groupings (0-18, 19-34, 35-64, 65+)
pct_uninsured_by_age <- acs_df_state %>%
  # uncomment to remove unauthorized immigrants
  filter(lawful == 1) %>%
  group_by(agegroup) %>%
  summarize(uninsured = sum(ifelse(hcovany==1, perwt, 0)),
            tpop = sum(perwt),
            pct_uninsured = uninsured/tpop,
  )
saveRDS(pct_uninsured_by_age, file = "data/processed/pct_uninsured_by_age.Rds")

# breakdown of uninsured by race
pct_unins_by_race <- acs_df_state %>%
  # uncomment to remove unauthorized immigrants
  filter(lawful == 1) %>%
  group_by(race) %>%
  summarize(uninsured = sum(ifelse(hcovany==1, perwt, 0)),
            tpop = sum(perwt),
            pct_uninsured = uninsured/tpop)
saveRDS(pct_unins_by_race, file = "data/processed/pct_unins_by_race.Rds")

# breakdown of ESI by age groupings (0-18, 19-34, 35-64, 65+)
pct_ESI_by_age <- acs_df_state %>%
  # uncomment to remove unauthorized immigrants
  filter(lawful == 1) %>%
  group_by(agegroup) %>%
  summarize(ESI = sum(ifelse(hinsemp==2, perwt, 0)),
            tpop = sum(perwt),
            pct_ESI = ESI/tpop)
saveRDS(pct_ESI_by_age, file = "data/processed/pct_ESI_by_age.Rds")

# breakdown of ESI by age groupings (0-18, 19-34, 35-64, 65+)
pct_ESI_by_race <- acs_df_state %>%
  # uncomment to remove unauthorized immigrants
  filter(lawful == 1) %>%
  group_by(race) %>%
  summarize(ESI = sum(ifelse(hinsemp==2, perwt, 0)),
            tpop = sum(perwt),
            pct_ESI = ESI/tpop)
saveRDS(pct_ESI_by_race, file = "data/processed/pct_ESI_by_race.Rds")

# basic bar plot showing age breakdowns for uninsured
x_label <- ipums_var_label(acs_data_state, age)
source_info <- ipums_file_info(acs_ddi, "ipums_project")
unins.age.plot <- ggplot(pct_uninsured_by_age, aes(x=agegroup, y=pct_uninsured)) +
  geom_bar(stat = "identity", fill = "#00263a") +
  scale_x_discrete(x_label) +
  scale_y_continuous("Percent", labels = scales::percent) +
  labs(
    title = "Uninsured by Age",
    subtitle = "Civilian, non-institutionalized population",
    caption = paste0("Source: ", source_info)
  ) +
  theme_economist() +
  scale_fill_economist()
saveRDS(unins.age.plot, file = "data/processed/unins-age-plot.Rds")

# basic bar plot showing age breakdowns for ESI
x_label <- ipums_var_label(acs_data_state, age)
source_info <- ipums_file_info(acs_ddi, "ipums_project")
esi.age.plot <- ggplot(pct_ESI_by_age, aes(x=agegroup, y=pct_ESI)) +
  geom_bar(stat = "identity", fill = "#00263a") +
  scale_x_discrete(x_label) +
  scale_y_continuous("Percent", 
                     labels = scales::percent) +
  labs(
    title = "ESI, by Age",
    subtitle = "Civilian, non-institutionalized population",
    caption = paste0("Source: ", source_info)
  ) +
  theme_economist() +
  scale_fill_economist()
saveRDS(esi.age.plot, file = "data/processed/esi-age-plot.Rds")

# basic bar plot showing race breakdown
x_label <- "Race/Ethnicity"
source_info <- ipums_file_info(acs_ddi_state, "ipums_project")
unins.race.plot <- ggplot(pct_unins_by_race, aes(x=race, y=pct_uninsured)) +
  geom_bar(stat = "identity", fill = "#00263a") +
  scale_x_discrete(x_label) +
  scale_y_continuous("Percent", labels = scales::percent) +
  labs(
    title = "Uninsured by Race/Ethnicity",
    subtitle = "Civilian, non-institutional population",
    caption = paste0("Source: ", source_info)
  ) +
  theme_economist() +
  scale_fill_economist()
saveRDS(unins.race.plot, file = "data/processed/unins-race-plot.Rds")

# basic bar plot showing race breakdown
x_label <- "Race/Ethnicity"
source_info <- ipums_file_info(acs_ddi_state, "ipums_project")
esi.race.plot <- ggplot(pct_ESI_by_race, aes(x=race, y=pct_ESI)) +
  geom_bar(stat = "identity", fill = "#00263a") +
  scale_x_discrete(x_label) +
  scale_y_continuous("Percent", 
                     labels = scales::percent) +
  labs(
    title = "ESI, by Race",
    subtitle = "Civilian, non-institutional population",
    caption = paste0("Source: ", source_info)
  ) +
  theme_economist() +
  scale_fill_economist()
saveRDS(esi.race.plot, file = "data/processed/esi-race-plot.Rds")

# create a stacked barplot of age and fpl of uninsured population
fpl.df <- acs_df_state %>%
  filter(relate != 13 & lawful==1) %>%
  group_by(agegrp2) %>%
  summarize(
    "401+% of FPL" = sum(ifelse(hcovany==1 & (poverty > 400), perwt, 0)),
    "301-400% of FPL" = sum(ifelse(hcovany==1 & (poverty >= 301 & poverty <= 400), perwt, 0)),
    "139-300% of FPL" = sum(ifelse(hcovany==1 & (poverty >= 139 & poverty <= 300), perwt, 0)),
    "<139% of FPL" = sum(ifelse(hcovany==1 & poverty < 139, perwt, 0))
  ) %>%
  mutate(agegroup = as.factor(agegrp2)) %>%
  subset(select = -c(agegrp2))


fpl.df.long <- gather(fpl.df, fpl_grp, est, "401+% of FPL":"<139% of FPL", factor_key = TRUE)


age.fpl.plot <- ggplot(fpl.df.long, aes(fill=fpl_grp, y=est, x=agegroup, label=est)) +
  geom_bar(position="stack", stat="identity") +
  geom_text(size = 4, position = position_stack(vjust = 0.5), colour = "white") +
  labs( title = "Eligibile Uninsured by Age Group and Income as a Percent of the Federal Poverty Level", caption = 
          "Breakdown of the uninsured population by age and percent of income to the federal poverty level (FPL)") +
  ylab("Number of Uninsured") +
  xlab("Age Group") +
  theme_economist() +
  scale_fill_economist()
saveRDS(age.fpl.plot, file = "data/processed/age-fpl-plot.Rds")

############################# Post-COVID job losses

# breakdown of uninsured by age groupings (0-18, 19-34, 35-64, 65+)
post_pct_uninsured_by_age <- acs_df_state %>%
  # uncomment to remove unauthorized immigrants
  filter(lawful == 1) %>%
  group_by(agegroup) %>%
  summarize(uninsured = sum(ifelse(hcovany==1, perwt, 0)),
            tpop = sum(perwt),
            pct_uninsured = uninsured/tpop,
            ins_emp = sum(ifelse(hinsemp==2, perwt, 0)),
            total_emp_pre = sum(total_employment),
            total_unemp_post = sum(total_disemployment)) %>%
  mutate(pct_change_imputed = ifelse(total_emp_pre == 0,
                                     0,
                                     -total_unemp_post / total_emp_pre)) %>%
  mutate(esi_loss = ins_emp * pct_change_imputed) %>%
  mutate(esi_loss = ifelse(esi_loss>0, 0, esi_loss))  %>%
  mutate(new_unins = uninsured - esi_loss) %>%
  mutate(new_pct_unins = new_unins/tpop)
saveRDS(post_pct_uninsured_by_age, file = "data/processed/post_pct_uninsured_by_age.Rds")

# breakdown of uninsured by race
post_pct_unins_by_race <- acs_df_state %>%
  # uncomment to remove unauthorized immigrants
  filter(lawful == 1) %>%
  group_by(race) %>%
  summarize(uninsured = sum(ifelse(hcovany==1, perwt, 0)),
            tpop = sum(perwt),
            pct_uninsured = uninsured/tpop,
            ins_emp = sum(ifelse(hinsemp==2, perwt, 0)),
            total_emp_pre = sum(total_employment),
            total_unemp_post = sum(total_disemployment)) %>%
  mutate(pct_change_imputed = ifelse(total_emp_pre == 0,
                                     0,
                                     -total_unemp_post / total_emp_pre)) %>%
  mutate(esi_loss = ins_emp * pct_change_imputed) %>%
  mutate(esi_loss = ifelse(esi_loss>0, 0, esi_loss))  %>%
  mutate(new_unins = uninsured - esi_loss) %>%
  mutate(new_pct_unins = new_unins/tpop)
saveRDS(post_pct_unins_by_race, file = "data/processed/post_pct_unins_by_race.Rds")

# breakdown of ESI by age groupings (0-18, 19-34, 35-64, 65+)
post_pct_ESI_by_age <- acs_df_state %>%
  # uncomment to remove unauthorized immigrants
  filter(lawful == 1) %>%
  group_by(agegroup) %>%
  summarize(ESI = sum(ifelse(hinsemp==2, perwt, 0)),
            tpop = sum(perwt),
            pct_ESI = ESI/tpop,
            total_emp_pre = sum(total_employment),
            total_unemp_post = sum(total_disemployment)) %>%
  mutate(pct_change_imputed = ifelse(total_emp_pre == 0,
                                     0,
                                     -total_unemp_post / total_emp_pre)) %>%
  mutate(esi_loss = ESI * pct_change_imputed) %>%
  mutate(esi_loss = ifelse(esi_loss>0, 0, esi_loss))  %>%
  mutate(new_esi = ESI + esi_loss) %>%
  mutate(new_pct_esi = new_esi/tpop)
saveRDS(post_pct_ESI_by_age, file = "data/processed/post_pct_ESI_by_age.Rds")

# breakdown of ESI by age groupings (0-18, 19-34, 35-64, 65+)
post_pct_ESI_by_race <- acs_df_state %>%
  # uncomment to remove unauthorized immigrants
  filter(lawful == 1) %>%
  group_by(race) %>%
  summarize(ESI = sum(ifelse(hinsemp==2, perwt, 0)),
            tpop = sum(perwt),
            pct_ESI = ESI/tpop,
            total_emp_pre = sum(total_employment),
            total_unemp_post = sum(total_disemployment)) %>%
  mutate(pct_change_imputed = ifelse(total_emp_pre == 0,
                                     0,
                                     -total_unemp_post / total_emp_pre)) %>%
  mutate(esi_loss = ESI * pct_change_imputed) %>%
  mutate(esi_loss = ifelse(esi_loss>0, 0, esi_loss))  %>%
  mutate(new_esi = ESI + esi_loss) %>%
  mutate(new_pct_esi = new_esi/tpop)
saveRDS(post_pct_ESI_by_race, file = "data/processed/post_pct_ESI_by_race.Rds")

# basic bar plot showing age breakdowns for uninsured
x_label <- ipums_var_label(acs_data, age)
source_info <- ipums_file_info(acs_ddi, "ipums_project")
post.unins.age.plot <- ggplot(post_pct_uninsured_by_age, aes(x=agegroup, y=new_pct_unins)) +
  geom_bar(stat = "identity", fill = "#00263a") +
  scale_x_discrete(x_label) +
  scale_y_continuous("Percent", labels = scales::percent) +
  labs(
    title = "Potential Uninsured by Age",
    subtitle = "MHC eligible population",
    caption = paste0("Source: ", source_info)
  ) +
  theme_economist() +
  scale_fill_economist()
saveRDS(post.unins.age.plot, file = "data/processed/post_unins-age-plot.Rds")

# basic bar plot showing age breakdowns for ESI
x_label <- ipums_var_label(acs_data, age)
source_info <- ipums_file_info(acs_ddi, "ipums_project")
post.esi.age.plot <- ggplot(post_pct_ESI_by_age, aes(x=agegroup, y=new_pct_esi)) +
  geom_bar(stat = "identity", fill = "#00263a") +
  scale_x_discrete(x_label) +
  scale_y_continuous("Percent of total population", 
                     labels = scales::percent) +
  labs(
    title = "Potential ESI, by Age",
    subtitle = "MHC eligible population",
    caption = paste0("Source: ", source_info)
  ) +
  theme_economist() +
  scale_fill_economist()
saveRDS(post.esi.age.plot, file = "data/processed/post-esi-age-plot.Rds")

# basic bar plot showing race breakdown
x_label <- "Race/Ethnicity"
source_info <- ipums_file_info(acs_ddi, "ipums_project")
post.unins.race.plot <- ggplot(post_pct_unins_by_race, aes(x=race, y=new_pct_unins)) +
  geom_bar(stat = "identity", fill = "#00263a") +
  scale_x_discrete(x_label) +
  scale_y_continuous("Percent of total population", labels = scales::percent) +
  labs(
    title = "Potential Uninsured by Race",
    subtitle = "MHC eligible population",
    caption = paste0("Source: ", source_info)
  ) +
  theme_economist() +
  scale_fill_economist()
saveRDS(post.unins.race.plot, file = "data/processed/post-unins-race-plot.Rds")

# basic bar plot showing race breakdown
x_label <- "Race/Ethnicity"
source_info <- ipums_file_info(acs_ddi, "ipums_project")
post.esi.race.plot <- ggplot(post_pct_ESI_by_race, aes(x=race, y=new_pct_esi)) +
  geom_bar(stat = "identity", fill = "#00263a") +
  scale_x_discrete(x_label) +
  scale_y_continuous("Percent of total population", 
                     labels = scales::percent) +
  labs(
    title = "Potential ESI, by Race",
    subtitle = "MHC eligible population",
    caption = paste0("Source: ", source_info)
  ) +
  theme_economist() +
  scale_fill_economist()
saveRDS(post.esi.race.plot, file = "data/processed/post-esi-race-plot.Rds")

# create a stacked barplot of age and fpl of uninsured population
post.fpl.df <- acs_df_state %>%
  filter(relate != 13 & lawful==1) %>%
  group_by(agegrp2) %>%
  summarize(
    pre139 = sum(ifelse(hcovany==1 & poverty < 139, perwt, 0)),
    ins_emp_139 = sum(ifelse(hinsemp==2 & poverty < 139, perwt, 0)),
    emp_139 = sum(ifelse(hcovany==1 & poverty <139, total_employment, 0)),
    unemp_139 = sum(ifelse(hcovany==1 & poverty <139, total_disemployment, 0)),
    pre300 = sum(ifelse(hcovany==1 & (poverty >= 139 & poverty <= 300), perwt, 0)),
    ins_emp_300 = sum(ifelse(hinsemp==2 & poverty < 139, perwt, 0)),
    emp_300 = sum(ifelse(hcovany==1 & (poverty >= 139 & poverty <= 300), total_employment, 0)),
    unemp_300 = sum(ifelse(hcovany==1 & (poverty >= 139 & poverty <= 300), total_disemployment, 0)),
    pre400 = sum(ifelse(hcovany==1 & (poverty >= 301 & poverty <= 400), perwt, 0)),
    ins_emp_400 = sum(ifelse(hinsemp==2 & poverty < 139, perwt, 0)),
    emp_400 = sum(ifelse(hcovany==1 & (poverty >= 301 & poverty <= 400), total_employment, 0)),
    unemp_400 = sum(ifelse(hcovany==1 & (poverty >= 301 & poverty <= 400), total_disemployment, 0)),
    pre401 = sum(ifelse(hcovany==1 & (poverty > 400), perwt, 0)),
    ins_emp_401 = sum(ifelse(hinsemp==2 & poverty < 139, perwt, 0)),
    emp_401 = sum(ifelse(hcovany==1 & poverty > 400, total_employment, 0)),
    unemp_401 = sum(ifelse(hcovany==1 & poverty > 400, total_disemployment, 0))
  ) %>%
  mutate(pct_chg_401 = ifelse(emp_401 == 0, 0,
                              -unemp_401 / emp_401)) %>%
  mutate(esi_loss_401 = ins_emp_401 * pct_chg_401) %>%
  mutate(esi_loss_401 = ifelse(esi_loss_401>0, 0, esi_loss_401))  %>%
  mutate("401+% of FPL" = round(pre401 - esi_loss_401,0)) %>%
  mutate(pct_chg_400 = ifelse(emp_400 == 0, 0,
                              -unemp_400 / emp_400)) %>%
  mutate(esi_loss_400 = ins_emp_400 * pct_chg_400) %>%
  mutate(esi_loss_400 = ifelse(esi_loss_400>0, 0, esi_loss_400))  %>%
  mutate("301-400% of FPL" = round(pre400 - esi_loss_400,0)) %>%
  mutate(pct_chg_300 = ifelse(emp_300 == 0, 0,
                              -unemp_300 / emp_300)) %>%
  mutate(esi_loss_300 = ins_emp_300 * pct_chg_300) %>%
  mutate(esi_loss_300 = ifelse(esi_loss_300>0, 0, esi_loss_300))  %>%
  mutate("139-300% of FPL" = round(pre300 - esi_loss_300,0)) %>%
  mutate(pct_chg_139 = ifelse(emp_139 == 0, 0,
                              -unemp_139 / emp_139)) %>%
  mutate(esi_loss_139 = ins_emp_139 * pct_chg_139) %>%
  mutate(esi_loss_139 = ifelse(esi_loss_139>0, 0, esi_loss_139))  %>%
  mutate("<139% of FPL" = round(pre139 - esi_loss_139,0)) %>%
  
  mutate(agegroup = as.factor(agegrp2)) %>%
  subset(select = c(20,23,26,29,30))


post.fpl.df.long <- gather(post.fpl.df, fpl_grp, est, "401+% of FPL":"<139% of FPL", factor_key = TRUE)


post.age.fpl.plot <- ggplot(post.fpl.df.long, aes(fill=fpl_grp, y=est, x=agegroup, label=est)) +
  geom_bar(position="stack", stat="identity") +
  geom_text(size = 4, position = position_stack(vjust = 0.5), colour = "white") +
  labs( title = "Uninsured by Age Group and Percent FPL, Post COVID", caption = 
          "Breakdown of the uninsured population by age and percent of income to the federal poverty level (FPL)
        post COVID-19 related loss of employer-sponsored insurance") +
  ylab("Number of Uninsured") +
  xlab("Age Group") +
  theme_economist() +
  scale_fill_economist()
saveRDS(post.age.fpl.plot, file = "data/processed/post-age-fpl-plot.Rds")

################################################################################
########################## standalone statewide stats ##########################
################################################################################

# produced these just to verify totals agaisnt other published analysis, these
# are not saved as Rds and imported to the Rmarkdown Dashboard

# percent uninsured, total non-institutionalized population
pct.unins.state <- acs_df_state %>% summarize(pct_unins = (sum(ifelse(hcovany==1 & relate != 13, 
                                                                perwt, 0)))/(sum(ifelse(relate != 13, 
                                                                                        perwt, 0))))
# number of uninsured, non-institutionalized population
unins.state <- acs_df_state %>% summarize(uninsured = sum(ifelse(hcovany==1 & relate != 13, perwt, 0)))
# percent uninsured, lawfully present non-institutionalized population
pct.unins.state.filt <- acs_df_state %>% summarize(pct_unins = (sum(ifelse(hcovany==1 & lawful==1 & relate != 13, 
                                                                     perwt, 0)))/(sum(ifelse(lawful==1 & 
                                                                                               relate != 13, 
                                                                                             perwt, 0))))
# percent insured, lawfully present non-institutionalized population
pct.ins.state.filt <- acs_df_state %>% summarize(pct_ins = (sum(ifelse(hcovany==2 & lawful==1 & relate != 13, 
                                                                 perwt, 0)))/(sum(ifelse(lawful==1 & 
                                                                                           relate != 13, 
                                                                                         perwt, 0))))
# number of uninsured who are lawfully present and not institutionalized
unins.state <- acs_df_state %>% summarize(uninsured = sum(ifelse(hcovany==1 & lawful==1 & relate != 13, 
                                                           perwt, 0)))

# number of uninsured, lawfully present, non-institutionalized, minus those w/
# offer of ESI (8.9% based on Urban Institute analysis: 
# https://www.urban.org/research/publication/characteristics-remaining-uninsured-update)
# decided against excluding these individuals, they could still enroll in non-
# marketplace plans
# acs_df_state %>% summarize(uninsured = (sum(ifelse(hcovany==1 & lawful==1 & relate != 13, 
#                                              perwt, 0)))) %>%
#   mutate(uninsured = uninsured-(uninsured*.089))

# number of uninsured if all who lose ESI become uninsured
acs_df_state %>% summarize(uninsured = (sum(ifelse(hcovany==1 & lawful==1 & relate != 13, 
                                             perwt, 0)))) %>%
  mutate(uninsured = uninsured-(df.unins.pop.state.filt$esi_loss))

acs_df_state <- acs_df_state %>% mutate(fpl_grp = case_when(poverty >= 0 & poverty < 138 ~ 1,
                                                poverty >= 139 & poverty <= 300 ~ 2,
                                                poverty >= 301 & poverty <= 400 ~ 3,
                                                poverty > 400 ~ 4))

# total statewide population that is lawfully present and non-institutionalized
acs_df_state %>% summarize(population = sum(ifelse(lawful==1 & relate != 13, perwt, 0)))

################################################################################
# further examination of the 18-34 age group, statewide breakdowns
# breakdown of uninsured by race
pct_unins_by_race_18to34 <- acs_df_state %>%
  # uncomment to remove unauthorized immigrants
  filter(lawful == 1 & age>17 & age<35) %>%
  group_by(race) %>%
  summarize(uninsured = sum(ifelse(hcovany==1, perwt, 0)),
            insured = sum(ifelse(hcovany==2, perwt, 0)),
            tpop = sum(perwt),
            "Percent Uninsured" = uninsured/tpop,
            "Percent Insured" = insured/tpop)

ins_status_by_race_18to34 <- gather(pct_unins_by_race_18to34, Status, est, 
                                   "Percent Uninsured":"Percent Insured", factor_key = TRUE)

# basic bar plot showing race breakdown of uninsured between 18-34 years
x_label <- "Race/Ethnicity"
source_info <- ipums_file_info(acs_ddi_state, "ipums_project")
unins.race.plot.18to34 <- ggplot(pct_unins_by_race_18to34, aes(x=race, y=uninsured)) +
  geom_bar(stat = "identity", fill = "#00263a") +
  geom_text(data=subset(pct_unins_by_race_18to34, uninsured != 0),
            aes(label = uninsured, y = ifelse(uninsured<100,uninsured+500,uninsured+1400)), size=4, colour="black") +
  scale_x_discrete(x_label) +
  scale_y_continuous("Number of Uninsured, 18 to 34 years") +
  labs(
    title = "Uninsured 18 to 34 years, by Race/Ethnicity",
    subtitle = "Maryland Health Connection Eligible Population",
    caption = paste0("Source: ", source_info)
  ) + 
  theme_economist() +
  scale_fill_economist() +
  coord_flip()


# basic stacked bar plot showing race breakdown by insured status
x_label <- "Race/Ethnicity"
source_info <- ipums_file_info(acs_ddi_state, "ipums_project")
ins.status.race.plot.18to34 <- ggplot(ins_status_by_race_18to34, 
                                 aes(fill=Status, x=race, y=est, label=scales::percent(est %>% round(2)))) +
  geom_bar(position="stack", stat = "identity") +
  geom_text(aes(label=paste0(ifelse(Status=="Percent Uninsured",uninsured,insured)," (",scales::percent(est %>% round(2)),")")), 
            size = 4, colour = "white", position = position_stack(vjust = 0.5)) +
  scale_x_discrete(x_label) +
  scale_y_continuous("Percent of total population, 18 to 34 years", 
                     labels = scales::percent) +
  labs(
    title = "Health Insurance Status, 18 to 34 years",
    subtitle = "Maryland Health Connection Eligible Population",
    caption = paste0("Source: ", source_info)
  ) +
  theme_economist() +
  scale_fill_economist() +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE))

# create a stacked barplot of detailed fpl of 18 to 24 yrs uninsured population
fpl.df.18to24 <- acs_df_state %>%
  filter(relate != 13 & lawful==1 & age>17 & age<25) %>%
  mutate(FPL = case_when(poverty < 138 ~ "0-138% of FPL",
                              poverty >= 138 & poverty <= 150 ~ "138-150% of FPL",
                               poverty >= 151 & poverty <= 200 ~ "151-200% of FPL",
                               poverty >= 201 & poverty <= 250 ~ "201-250% of FPL",
                               poverty >= 251 & poverty <= 300 ~ "251-300% of FPL",
                               poverty >= 301 & poverty <= 350 ~ "301-350% of FPL",
                               poverty >= 351 & poverty <= 400 ~ "351-400% of FPL",
                               poverty >= 401 & poverty <= 500 ~ "401-500% of FPL",
                         poverty >= 501  ~ "More than 500% of FPL")) %>%
  group_by(FPL) %>%
  summarize(
    uninsured = sum(ifelse(hcovany==1, perwt, 0)),
    insured = sum(ifelse(hcovany==2, perwt, 0)),
    tpop = sum(perwt),
    "Percent Uninsured" = uninsured/tpop,
    "Percent Insured" = insured/tpop
  )
# create a stacked barplot of detailed fpl of 25 to 34 yrs uninsured population
fpl.df.25to34 <- acs_df_state %>%
  filter(relate != 13 & lawful==1 & age>24 & age<35) %>%
  mutate(FPL = case_when(poverty < 138 ~ "0-138% of FPL",
                         poverty >= 138 & poverty <= 150 ~ "138-150% of FPL",
                         poverty >= 151 & poverty <= 200 ~ "151-200% of FPL",
                         poverty >= 201 & poverty <= 250 ~ "201-250% of FPL",
                         poverty >= 251 & poverty <= 300 ~ "251-300% of FPL",
                         poverty >= 301 & poverty <= 350 ~ "301-350% of FPL",
                         poverty >= 351 & poverty <= 400 ~ "351-400% of FPL",
                         poverty >= 401 & poverty <= 500 ~ "401-500% of FPL",
                         poverty >= 501  ~ "More than 500% of FPL")) %>%
  group_by(FPL) %>%
  summarize(
    uninsured = sum(ifelse(hcovany==1, perwt, 0)),
    insured = sum(ifelse(hcovany==2, perwt, 0)),
    tpop = sum(perwt),
    "Percent Uninsured" = uninsured/tpop,
    "Percent Insured" = insured/tpop
  )

fpl.df.18to24.long <- gather(fpl.df.18to24, Status, est, 
                             "Percent Uninsured":"Percent Insured", factor_key = TRUE) 
fpl.df.25to34.long <- gather(fpl.df.25to34, Status, est, 
                             "Percent Uninsured":"Percent Insured", factor_key = TRUE)



ins.status.fpl.plot.18to24 <- ggplot(fpl.df.18to24.long, 
                                     aes(fill=Status, y=est, x=FPL)) +
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label=paste0(ifelse(Status=="Percent Uninsured",uninsured,insured)," (",scales::percent(est %>% round(2)),")")), 
            size = 4, colour = "white", position = position_stack(vjust = 0.5)) +
  labs( title = "Health Insurance Status, 18 to 24 years",
        subtitle = "by Income as a Percent of the Federal Poverty Level",
        caption = paste0("Source: ", source_info)) +
  scale_y_continuous("Percent of total population, 18 to 24 years", 
                     labels = scales::percent) +
  xlab("Federal Poverty Level") +
  theme_economist() +
  scale_fill_economist() +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE))
ins.status.fpl.plot.25to34 <- ggplot(fpl.df.25to34.long, 
                                     aes(fill=Status, y=est, x=FPL)) +
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label=paste0(ifelse(Status=="Percent Uninsured",uninsured,insured)," (",scales::percent(est %>% round(2)),")")), 
            size = 4, colour = "white", position = position_stack(vjust = 0.5)) +
  labs( title = "Health Insurance Status, 25 to 34 years",
        subtitle = "by Income as a Percent of the Federal Poverty Level",
        caption = paste0("Source: ", source_info)) +
  scale_y_continuous("Percent of total population, 25 to 34 years", 
                     labels = scales::percent) +
  xlab("Federal Poverty Level") +
  theme_economist() +
  scale_fill_economist() +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE))

# basic bar plot showing race breakdown of uninsured between 18-24 years
source_info <- ipums_file_info(acs_ddi_state, "ipums_project")
unins.fpl.plot.18to24 <- ggplot(fpl.df.18to24, aes(x=FPL, y=uninsured)) +
  geom_bar(stat = "identity", fill = "#00263a") +
  geom_text(aes(label = uninsured, y = uninsured - 300), size=4, colour="white") +
  labs( title = "Maryland Health Connection Eligible Uninsured 18 to 24",
        subtitle = "by Income as a Percent of Federal Poverty Level",
        caption = paste0("Source: ", source_info),
        x="Federal Poverty Level") +
  scale_y_continuous("Number of Uninsured, 18 to 24 years") +
  theme_economist() +
  scale_fill_economist() +
  coord_flip()
# basic bar plot showing race breakdown of uninsured between 25-34 years
source_info <- ipums_file_info(acs_ddi_state, "ipums_project")
unins.fpl.plot.25to34 <- ggplot(fpl.df.25to34, aes(x=FPL, y=uninsured)) +
  geom_bar(stat = "identity", fill = "#00263a") +
  geom_text(aes(label = uninsured, y = uninsured - 500), size=4, colour="white") +
  labs( title = "Maryland Health Connection Eligible Uninsured 25 to 34",
        subtitle = "by Income as a Percent of Federal Poverty Level",
        caption = paste0("Source: ", source_info),
        x="Federal Poverty Level") +
  scale_y_continuous("Number of Uninsured, 25 to 34 years") +
  theme_economist() +
  scale_fill_economist() +
  coord_flip()
