![MHBE logo](https://raw.githubusercontent.com/Maryland-Health-Benefits-Exchange/MD_uninsured_analysis/main/MHBE_color_transparent.png) 

# MD_uninsured_analysis
Analysis of uninsured pre and post COVID-19-related loss of ESI 
adj. to account for unauth. immigrants

Data files were obtained from multiple sources. IPUMS was used to generate the 
data and DDI files for American Community Survey data, the unemployment map was imported using the `ESRI2df`
r library, and BLS CES data was downloaded via the API in the Urban Institute
modified script (available [here](https://github.com/Maryland-Health-Benefits-Exchange/covid-neighborhood-job-analysis)), and other data as detailed in our methodology. 

IPUMS extract request: 2018 ACS 5yr (sample) 

Variables:
`YEAR, MULTYEAR, SAMPLE, 
SERIAL, HHWT, CLUSTER, STRATA, COUNTYFIP, STATEFIP, METRO, PUMA, GQ, OWNERSHP, 
OWNERSHPD, PERNUM, PERWT, FAMSIZE, RELATE, RELATED, SEX, AGE, RACE, RACED, 
HISPAN, HISPAND, BPL, BPLD, CITIZEN, YRIMMIG, HCOVANY, HCOVPRIV, HCOVPUB, 
HINSEMP, EMPSTAT, EMPSTATD, OCC, EDUC, INCSS, INCWELFR, INCSUPP, VETSTAT, IND, 
POVERTY, MIGRATE1, MIGRATE1D, MARST` - limited to Maryland (`STATEFIP==24`)

IPUMS extract request: 2018 ACS 1yr (sample) 

Variables:
`YEAR, SAMPLE, 
SERIAL, HHWT, CLUSTER, STRATA, COUNTYFIP, STATEFIP, METRO, PUMA, GQ, OWNERSHP, 
OWNERSHPD, PERNUM, PERWT, FAMSIZE, RELATE, RELATED, SEX, AGE, RACE, RACED, 
HISPAN, HISPAND, BPL, BPLD, CITIZEN, YRIMMIG, HCOVANY, HCOVPRIV, HCOVPUB, 
HINSEMP, EMPSTAT, EMPSTATD, OCC, EDUC, INCSS, INCWELFR, INCSUPP, VETSTAT, IND, 
POVERTY, MIGRATE1, MIGRATE1D, MARST` - limited to Maryland (`STATEFIP==24`)

## Directory Structure:
- `MD_uninsured_analysis`  main repository
  - `COVID-19_Uninsured_Analysis` project directory
    - `COVID_Uninsured_Analysis_Dashboard.Rmd`  rmarkdown document to produce 
    flex dashboard
    - `COVID19_Uninsured_Analysis.Rproj`  initial R project file
    - `code`  R scripts
      * `methodology_final.R` script for sub-state (PUMA, county) level data
      - `methodology_statewide.R` script for statewide data
      - `household_analysis.R` script to analyze who can/will take up spousal insurance
    - `data`  input data files
      * `raw`  files that are input for R script
      * `processed`  files that are input for Rmarkdown file
    

## Required R Libraries
- `ipumsr`
- `acs`
- `tidyverse`
- `labelled`
- `janitor`
- `stringr`
- `knitr`
- `rmarkdown`
- `flexdashboard`
- `sf`
- `leaflet`
- `testit`
- `esri2sf`
- `tigris`
- `htmlwidgets`
- `DT`
- `shiny`
- `ggthemes`
- `plotly`


## Limitations and Assumptions
See our Rmarkdown dashboard "methodology" section for complete list of limitations 
and assumptions made for this analysis: [put final URL here]

## Citations
See our Rmarkdown dashboard "Citations" section for complete list of works cited 
and persons/organizations that assisted with this analysis : [put final URL here]

## Contact
MHBE.policy@maryland.gov
