# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # #   Maryland Health Benefit Exchange  # # # # # # # # # # # # 
# # # # # # # # # #                                     # # # # # # # # # # # # 
# # # # # # # # # # Program Name: household_analysis.R  # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # Analysis of married households that lost ESI due to COVID-19 Job loss # # 
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
# generate dataframe that will be used to adjust esi_loss for people able to 
# enroll in spousal insurance

# generate dataframe that is only married people and their spouses
acs_df_hh <- acs_df %>% 
  filter(relate==1 | relate==2 & lawful==1) %>%
  select(serial, relate, puma, statefip, perwt, hhwt, marst, age, race, hinsemp, 
         empstat, poverty, total_employment, total_disemployment) %>%
  filter(marst==1) %>%
  group_by(serial) %>%
  # summarise data by household, detailing couples employment & insurance
  summarise(puma = puma,
            state = statefip,
            perwt.head = sum(ifelse(relate==1,perwt,0)),
            perwt.sp = sum(ifelse(relate==2,perwt,0)),
            age.head = sum(ifelse(relate==1,age,0)),
            age.sp = sum(ifelse(relate==2,age,0)),
            race.head = ifelse(relate==1,as.character(race),NA),
            race.sp = ifelse(relate==2,as.character(race),NA),
            esi.head = sum(ifelse(relate==1 & hinsemp==2,perwt,0)),
            esi.sp = sum(ifelse(relate==2 & hinsemp==2,perwt,0)),
            pov.head = sum(ifelse(relate==1,poverty,0)),
            pov.sp = sum(ifelse(relate==2,poverty,0)),
            empl.head = sum(ifelse(relate==1,total_employment,0)),
            disempl.head = sum(ifelse(relate==1,total_disemployment,0)),
            empl.sp = sum(ifelse(relate==2,total_employment,0)),
            disempl.sp = sum(ifelse(relate==2,total_disemployment,0)),
  ) %>%
  # calculate percent change in employment for head of household and spouse
  mutate(
    # change neg vals to zero for future calcs
    disempl.head = ifelse(disempl.head<0, 0, disempl.head),
    disempl.sp = ifelse(disempl.sp<0, 0, disempl.sp),
    pct.change.head = ifelse(empl.head == 0,
                                  0,
                                  disempl.head / empl.head),
    pct.change.sp = ifelse(empl.sp == 0,
                                0,
                                disempl.sp / empl.sp)) %>%
  # use percent change to calculate ESI loss for head and spouse
  mutate(esi.loss.head = esi.head * pct.change.head,
         esi.loss.sp = esi.sp * pct.change.sp
         )
# fill in missing values in race.head and race.sp
acs_df_hh <- acs_df_hh %>% group_by(serial) %>% fill(race.sp, .direction = "updown")
acs_df_hh <- acs_df_hh %>% group_by(serial) %>% fill(race.head, .direction = "updown")
# remove duplicate rows
acs_df_hh <- distinct(acs_df_hh)

acs_df_hh <- acs_df_hh %>% mutate(sp.not.fired = 1-(disempl.sp/perwt.sp),
                                  head.not.fired = 1-(disempl.head/perwt.head))


# generate vars to flag households where both partners lost ESI
acs_df_hh <- acs_df_hh %>%
  mutate(flag1 = ifelse(empl.head>0 & esi.head>0 & empl.sp>0 & esi.sp>0, 1, 0),
         head.esi.switchers = ifelse(flag1==1, disempl.head*sp.not.fired,0),
         sp.esi.switchers = ifelse(flag1==1, disempl.sp*head.not.fired,0))

esi_loss_df <- acs_df %>%
  # uncomment to filter out unlawfully present migrants
  filter(lawful==1) %>%
  group_by(puma) %>%
  summarise(ins_emp = sum(ifelse(hinsemp==2,perwt,0)),
            total_emp_pre = sum(total_employment),
            total_unemp_post = sum(total_disemployment)) %>%
  ungroup() %>%
  mutate(pct_change_imputed = ifelse(total_emp_pre == 0,
                                     0,
                                     total_unemp_post / total_emp_pre)) %>%
  group_by(puma) %>%
  mutate(esi_loss = ins_emp * pct_change_imputed) %>%
  mutate(esi_loss = ifelse(esi_loss<0, 0, esi_loss))

# total up the switchers and calculate the fraction that switch
acs_df_hh_puma <- acs_df_hh %>%
  group_by(puma) %>%
  summarise(tot.switch = sum(head.esi.switchers + sp.esi.switchers),
            disempl.head = sum(disempl.head),
            disempl.sp = sum(disempl.sp)) %>%
  left_join(esi_loss_df, by = "puma") %>%
  mutate(pct_uptake = tot.switch/esi_loss)


############################################################################
# 
# generate dataframe that is only married people and their spouses
acs_df_hh <- acs_df %>% 
  filter(relate==1 | relate==2 & lawful==1) %>%
  select(serial, relate, puma, statefip, perwt, hhwt, marst, age, race, hinsemp, empstat, poverty,
         total_employment, total_disemployment) %>%
  filter(marst==1) %>%
  group_by(serial) %>%
  # summarise data by household, detailing couples employment & insurance
  summarise(puma = puma,
            state = statefip,
            perwt.head = sum(ifelse(relate==1,perwt,0)),
            perwt.sp = sum(ifelse(relate==2,perwt,0)),
            age.head = sum(ifelse(relate==1,age,0)),
            age.sp = sum(ifelse(relate==2,age,0)),
            race.head = ifelse(relate==1,as.character(race),NA),
            race.sp = ifelse(relate==2,as.character(race),NA),
            esi.head = sum(ifelse(relate==1 & hinsemp==2,perwt,0)),
            esi.sp = sum(ifelse(relate==2 & hinsemp==2,perwt,0)),
            pov.head = sum(ifelse(relate==1,poverty,0)),
            pov.sp = sum(ifelse(relate==2,poverty,0)),
            empl.head = sum(ifelse(relate==1,total_employment,0)),
            disempl.head = sum(ifelse(relate==1,total_disemployment,0)),
            empl.sp = sum(ifelse(relate==2,total_employment,0)),
            disempl.sp = sum(ifelse(relate==2,total_disemployment,0)),
  ) %>%
  # calculate percent change in employment for head of household and spouse
  mutate(
    # change neg vals to zero for future calcs
    disempl.head = ifelse(disempl.head<0, 0, disempl.head),
    disempl.sp = ifelse(disempl.sp<0, 0, disempl.sp),
    pct.change.head = ifelse(empl.head == 0,
                             0,
                             disempl.head / empl.head),
    pct.change.sp = ifelse(empl.sp == 0,
                           0,
                           disempl.sp / empl.sp)) %>%
  # use percent change to calculate ESI loss for head and spouse
  mutate(esi.loss.head = esi.head * pct.change.head,
         esi.loss.sp = esi.sp * pct.change.sp
  )
# fill in missing values in race.head and race.sp
acs_df_hh <- acs_df_hh %>% group_by(serial) %>% fill(race.sp, .direction = "updown")
acs_df_hh <- acs_df_hh %>% group_by(serial) %>% fill(race.head, .direction = "updown")
# remove duplicate rows
acs_df_hh <- distinct(acs_df_hh)

acs_df_hh <- acs_df_hh %>% mutate(sp.not.fired = 1-(disempl.sp/perwt.sp),
                                  head.not.fired = 1-(disempl.head/perwt.head))


# generate vars to flag households where both partners lost ESI
acs_df_hh <- acs_df_hh %>%
  mutate(flag1 = ifelse(empl.head>0 & esi.head>0 & empl.sp>0 & esi.sp>0, 1, 0),
         head.esi.switchers = ifelse(flag1==1, disempl.head*sp.not.fired,0),
         sp.esi.switchers = ifelse(flag1==1, disempl.sp*head.not.fired,0))

esi_loss_df <- acs_df %>%
  # uncomment to filter out unlawfully present migrants
  filter(lawful==1) %>%
  group_by(puma) %>%
  summarise(ins_emp = sum(ifelse(hinsemp==2,perwt,0)),
            total_emp_pre = sum(total_employment),
            total_unemp_post = sum(total_disemployment)) %>%
  ungroup() %>%
  mutate(pct_change_imputed = ifelse(total_emp_pre == 0,
                                     0,
                                     total_unemp_post / total_emp_pre)) %>%
  group_by(puma) %>%
  mutate(esi_loss = ins_emp * pct_change_imputed) %>%
  mutate(esi_loss = ifelse(esi_loss<0, 0, esi_loss))

# total up the switchers and calculate the fraction that switch
acs_df_hh_puma <- acs_df_hh %>%
  group_by(puma) %>%
  summarise(tot.switch = sum(head.esi.switchers + sp.esi.switchers),
            disempl.head = sum(disempl.head),
            disempl.sp = sum(disempl.sp)) %>%
  left_join(esi_loss_df, by = "puma") %>%
  mutate(pct.uptake = tot.switch/esi_loss)
