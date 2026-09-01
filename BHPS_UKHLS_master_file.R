##################### BHPS/UKHLS master file ############################
# download required packages for this task: "dplyr", "haven", "tidyr"
source("00_setting_work_space.R")

# Information for all persons in household, incl. children and non-respondents
folder_indall<-list.files(path = paste0(folder_main_uk, "ukhls/"), pattern = "_indall_protect\\.dta$")

year_bhps = data.frame(year_wave = 1991:2008,
                       letter = letters[1:18],
                       FIRSTOBS_Y_bhps = 1:18,
                       LASTOBS_Y_bhps  = 1:18)

year_ukhls = data.frame(letter = str_remove(folder_indall, "_indall_protect.dta"),
                        year_wave = 2009:2019,
                        wave = 1:length(folder_indall),
                        FIRSTOBS_Y_ukhls = 1:11,
                        LASTOBS_Y_ukhls  = 1:11)

################################################################################
# xwavedat - Stable characteristics of individuals


master <- haven::read_dta(paste0(folder_main_uk, "ukhls/", "xwavedat_protect.dta")) %>%
  mutate(
    # UKHLS
    lwintvd_dv = ifelse(lwintvd_dv==-8, lwenum_dv, lwintvd_dv),
    fwintvd_dv = ifelse(fwintvd_dv==-8, fwenum_dv, fwintvd_dv),
    # BHPS
    lwintvd_dv_bh = ifelse(lwintvd_dv_bh==-8, lwenum_dv_bh, lwintvd_dv_bh),
    fwintvd_dv_bh = ifelse(fwintvd_dv_bh==-8, fwenum_dv_bh, fwintvd_dv_bh)
         ) %>% 
  select(pidp, pid, birthm, birthy, sex,
         lwintvd_dv,    # Last wave interviewed (incl. proxy), (UKHLS)
         fwintvd_dv,    # First wave interviewed (inc. proxy), (UKHLS)
         lwintvd_dv_bh, # Last wave interviewed (incl. proxy), (BHPS)
         fwintvd_dv_bh, # First wave interviewed (inc. proxy), (BHPS)
         xwdat_dv,      # Study enumerated in: UKHLS, BHPS or both
         anychild_dv) %>% 
  mutate(
    anychild_dv = ifelse(anychild_dv==1, "Yes", ifelse(anychild_dv==2,"No", NA)),
    date_birth = paste0(birthy, "-", ifelse(birthm<=9,  paste0("0",birthm), birthm), "-01")) %>%
  rename( BORN_M = birthm,
          BORN_Y = birthy,
          SEX = sex,
          which_study      = xwdat_dv) %>% 
  mutate( FIRSTOBS_Y_bhps  = fwintvd_dv_bh,
          LASTOBS_Y_bhps   = lwintvd_dv_bh, 
          FIRSTOBS_Y_ukhls = fwintvd_dv,
          LASTOBS_Y_ukhls  = lwintvd_dv) %>% 
  mutate(which_study = case_when(
          which_study == 1 ~ "in UKHLS",
          which_study == 2 ~ "in BHPS",
          which_study == 3 ~ "in both")) %>% 
  #bhps
  mutate(FIRSTOBS_Y_bhps  = as.numeric(FIRSTOBS_Y_bhps),
         LASTOBS_Y_bhps   = as.numeric(LASTOBS_Y_bhps),
         FIRSTOBS_Y_ukhls = as.numeric(FIRSTOBS_Y_ukhls),
         LASTOBS_Y_ukhls  = as.numeric(LASTOBS_Y_ukhls)) %>% 
  merge(select(year_bhps, year_wave, LASTOBS_Y_bhps), by = c("LASTOBS_Y_bhps"), all.x = T) %>% 
  select(-LASTOBS_Y_bhps) %>%
  rename(LASTOBS_Y_bhps = year_wave) %>%
  merge(select(year_bhps,year_wave,FIRSTOBS_Y_bhps), by = c("FIRSTOBS_Y_bhps"), all.x = T) %>%
  select(-FIRSTOBS_Y_bhps) %>%
  rename(FIRSTOBS_Y_bhps = year_wave) %>% 
  # ########## ukhls 
  merge(select(year_ukhls,year_wave,LASTOBS_Y_ukhls), by = c("LASTOBS_Y_ukhls"), all.x = T) %>% 
  select(-LASTOBS_Y_ukhls) %>%
  rename(LASTOBS_Y_ukhls = year_wave) %>%
  merge(select(year_ukhls,year_wave,FIRSTOBS_Y_ukhls), by = c("FIRSTOBS_Y_ukhls"), all.x = T) %>%
  select(-FIRSTOBS_Y_ukhls) %>%
  rename(FIRSTOBS_Y_ukhls = year_wave)

master %>% head()

# pidp,which_study,SEX, BORN_M, BORN_Y, FIRSTOBS_Y, LASTOBS_Y?
################################################################################
# BHPS  interview date
interview_date_bhps<-c()
for (i in 1:18) {
  if (i==1) {
    indresp <- haven::read_dta(paste0(folder_bhps_1,paste0("b",letters[i],"_indresp_protect.dta")),
                               col_select = c("pidp",
                                              paste0("b",letters[i],"_istrtdatd"),   # istrtdatd Date of interview: day
                                              paste0("b",letters[i],"_istrtdatm"),   # istrtdatm Date of interview: month
                               ))
    
    indresp[ , paste0("b", letters[i], "_istrtdaty")] <-1991

  }else{
    indresp <- haven::read_dta(paste0(folder_bhps_1,paste0("b",letters[i],"_indresp_protect.dta")),
                               col_select = c("pidp",
                                              paste0("b",letters[i],"_istrtdatd"),  # istrtdatd Date of interview: day
                                              paste0("b",letters[i],"_istrtdatm"),  # istrtdatm Date of interview: month
                                              paste0("b",letters[i],"_istrtdaty"),  # istrtdaty Date of interview: 4 digit year
                               ))

  }
  
  colnames(indresp) <- sub(paste0("b",letters[i], "_"), "", colnames(indresp))
  
  indresp <- indresp %>%
    mutate(
      istrtdaty = as.numeric(istrtdaty),
      istrtdatd = as.numeric(istrtdatd),
      istrtdatm = as.numeric(istrtdatm),
      interview_date = as.Date(paste(istrtdaty, istrtdatm, istrtdatd, sep = "-"), format = "%Y-%m-%d"),
      year_wave = year_bhps$year_wave[i]) %>% 
    select(pidp, year_wave, interview_date )
  
  interview_date_bhps <- bind_rows(interview_date_bhps, indresp)
  
  rm(indresp)
  
}

interview_date_bhps

# interview_status
interview_status_bhps <- haven::read_dta(paste0(folder_bhps_1,"xwaveid_bh_protect.dta"),
                                         col_select = c("pid","pidp", "ba_ivfio1_bh", ends_with("ivfio"))) %>% 
  tidyr::pivot_longer(cols = c("ba_ivfio1_bh", paste0("b", letters[2:18], "_ivfio")),
                      names_to  = 'letter',
                      values_to = 'interview_status') %>%
  mutate(letter = str_replace(letter, "_ivfio", "")) %>% 
  mutate(letter = str_replace(letter, "1_bh", "")) %>% 
  mutate(letter = str_replace(letter, "b", "")) %>% 
  select(pidp, letter, interview_status ) %>% 
  merge(year_bhps, by = c("letter"), all.x = T) %>% 
  select(pidp, year_wave, interview_status )

interview_bhps = interview_status_bhps %>% merge(interview_date_bhps, by = c("pidp", "year_wave"), all.x = T)

# rm(interview_status_bhps, interview_date_bhps, year_bhps)

# UKHLS: interview date
interview_date_ukhls <-c()
for (i in 1:length(folder_indall)) {
  
  x <- haven::read_dta(paste0(folder_main_uk,"ukhls/",letters[i],"_indresp_protect.dta"), 
                       col_select = c("pidp", 
                                      ends_with("intdaty_dv"), # Interview date: Year, derived
                                      ends_with("intdatm_dv"), # Interview date: Month, derived
                                      ends_with("intdatd_dv")  # Interview date: Day, derived
                       ))
  
  names(x)[-1] <- str_remove(names(x)[-1], paste0(letters[i], "_"))
  
  x = x %>% mutate(
    year_wave = year_ukhls$year_wave[i],
    interview_date = as.Date(paste(intdaty_dv, intdatm_dv, intdatd_dv, sep = "-"), format = "%Y-%m-%d"))
  
  interview_date_ukhls<-rbind(interview_date_ukhls, x)
  rm(x)
  
}


# interview_status
interview_status_ukhls <- read_dta(paste0(folder_main_uk,"ukhls/","xwaveid_protect.dta")) %>% 
  select(pidp, paste0(letters[1:length(year_ukhls$year_wave)], "_ivfio")) %>% 
  tidyr::pivot_longer(cols=paste0(letters[1:length(year_ukhls$year_wave)], "_ivfio"),
                      names_to='letter',
                      values_to='interview_status') %>%
  mutate(letter = str_replace(letter, "_ivfio","")) %>% 
  merge(year_ukhls, by = c("letter"), all.x = T) %>% 
  select(pidp, year_wave, interview_status ) 

interview_ukhls<-merge(interview_status_ukhls, interview_date_ukhls, by = c("pidp", "year_wave"), all.x =T)


rm(interview_status_ukhls, interview_date_ukhls, year_ukhls)
################################################################################
# merge interview information with master file
interview_ukhls = merge(interview_ukhls, master, by = c("pidp"), all.x = T)
interview_bhps  = merge(interview_bhps,  master, by = c("pidp"), all.x = T)

interview_status = bind_rows(interview_bhps, interview_ukhls)

# rm(master)

master_uk = bind_rows(interview_ukhls, interview_bhps) %>% 
  mutate(FIRSTOBS_Y = coalesce(FIRSTOBS_Y_bhps,FIRSTOBS_Y_ukhls),
         LASTOBS_Y = coalesce(LASTOBS_Y_ukhls, LASTOBS_Y_bhps)
         )

proxy = master_uk %>% mutate(
  interview_status = sjlabelled::as_character(interview_status),
  n = ifelse(interview_status!="full interview", 1,0)) %>% 
  group_by(pidp) %>% summarise(proxy = sum(n), ever_proxy = ifelse(proxy>0,"proxy", "not proxy"))

# cross sectional weights 
weights_bhps_ukhls = read_dta(paste0(folder_main_uk,"ukhls/","xwavedat_protect.dta")) %>% 
  select(pidp, psnenub_xd) %>% rename(cross_weights = psnenub_xd)

#### time invariant variables
master_bhps_ukhls_nonchange = master_uk %>%
  select(pidp, pid, BORN_Y, BORN_M, SEX, FIRSTOBS_Y, LASTOBS_Y, which_study ) %>% distinct() %>% 
  merge(proxy , by = c("pidp")) %>%
  merge(weights_bhps_ukhls, by = c("pidp"))

################################################################################
# add longitudinal weights
which_study = master_uk %>% 
  select(pidp, FIRSTOBS_Y, which_study) %>% distinct()

long_weights_bhps<-c()
for (i in 2:18) {
  
  indresp <- haven::read_dta(paste0(folder_bhps_1,paste0("b",letters[i],"_indresp_protect.dta")),
                             # those at BHPS or BHPS&UKHLS: indresp
                             col_select = c("pidp",paste0("b",letters[i],"_indin91_lw"))) 
  
  
  names(indresp)[-1] = substring(names(indresp)[-1], 4) 
  
  indresp$year_wave = 1990+i
  
  long_weights_bhps<-bind_rows(long_weights_bhps, indresp)
  
}

long_weights_ukhls_1<-c()
for (i in 2:length(folder_indall)) {
  
  indresp <- haven::read_dta(paste0(folder_main_uk,"ukhls/",letters[i],"_indresp_protect.dta"), 
                             # those at BHPS or BHPS&UKHLS: indresp
                             col_select = c("pidp", ends_with("_indin91_lw")))
  
  names(indresp)[-1] = substring(names(indresp)[-1], 3) 
  
  indresp$year_wave = 2008+i
  
  
  long_weights_ukhls_1<-bind_rows(long_weights_ukhls_1, indresp)
  
}

long_weights_ukhls_2<-c()
for (i in 2:length(folder_indall)) {
  
  indresp <- haven::read_dta(paste0(folder_main_uk,"ukhls/",letters[i],"_indresp_protect.dta"), 
                             # for only UKHLS respondents
                             col_select = c("pidp",ends_with("_indinus_lw")))
  
  names(indresp)[-1] = substring(names(indresp)[-1], 3) 
  
  indresp$year_wave = 2008+i
  
  long_weights_ukhls_2<-bind_rows(long_weights_ukhls_2, indresp)
  
}

# to long long_weights_bhps, long_weights_ukhls_1, long_weights_ukhls_2

# merge long long_weights_bhps, long_weights_ukhls_1, long_weights_ukhls_2
# and create one lonngitudal weight 
which_study = master_uk %>% 
  select(pidp, FIRSTOBS_Y, which_study) %>% distinct()


uk_long_weights = merge(long_weights_bhps, 
                        long_weights_ukhls_1, by = c("pidp", "year_wave"), all = T) %>% 
  merge(long_weights_ukhls_2,by = c("pidp", "year_wave"), all = T) %>% 
  merge(which_study, by = c("pidp"), all.x = T) %>% 
  # indinus_lw for only UKHLS respondents, 
  # indin91_lw for those at BHPS or BHPS&UKHLS: indresp
  mutate(long_weights = 
           case_when(which_study == "in BHPS" ~ indin91_lw.x,
                     which_study == "in both" ~ coalesce(indin91_lw.x, indin91_lw.y),
                     which_study == "in UKHLS" ~ indinus_lw
           )) %>% select(pidp, year_wave,long_weights)

################ emp_status ####################################################
# jbstat	Current labour force status	indresp
emp_status_bhps<-c()
for (i in 1:18) {
  
  indresp <- haven::read_dta(
    paste0(folder_bhps_1,paste0("b",letters[i],"_indresp_protect.dta")),
    col_select = c("pidp",
                   paste0("b",letters[i],"_jbstat")))
  
  names(indresp) <- gsub(paste0("^b",letters[i],"_"), "", names(indresp))
  
  indresp$jbstat = sjlabelled::as_character(indresp$jbstat)
  
  indresp$year_wave = i+1990
  
  emp_status_bhps<-bind_rows(emp_status_bhps,indresp)
  
}

emp_status_ukhls <-c()
for (i in 1:length(folder_indall)) {
  
  x <- haven::read_dta(paste0(folder_main_uk,"ukhls/",letters[i],"_indresp_protect.dta"), 
                       col_select = c("pidp", 
                                      paste0(letters[i],"_jbstat")
                       ))
  
  names(x)[-1] <- str_remove(names(x)[-1], paste0(letters[i], "_"))
  
  x$year_wave = i+2008
  
  x$jbstat = sjlabelled::as_character(x$jbstat)
  
  emp_status_ukhls <-bind_rows(emp_status_ukhls,x)  
  
}

# Maternity leave on maternity leave
emp_status = bind_rows(emp_status_ukhls,emp_status_bhps) %>% 
  mutate(emp_status = ifelse(
    jbstat %in%  c("self employed","Employed","employed","Paid employment(ft/pt)","Self employed", "self-employed",
                   "Maternity leave", "on maternity leave"
                   ), "working","not-working"
  )) %>% select(pidp,year_wave, emp_status)


################################################################################
# time variant to merge

master_uk  = master_uk %>% select(pidp, year_wave, interview_status,interview_date) %>% 
  mutate( interview_status =sjlabelled::as_character(interview_status))

master_uk[master_uk$pidp==68001367,] %>% arrange(year_wave)

master_uk_long = merge(master_uk,emp_status, by = c("pidp", "year_wave")) %>% 
  merge(uk_long_weights,by = c("pidp", "year_wave"), all.x = T)

master_uk_long[master_uk_long$pidp==68001367,] %>% arrange(year_wave)

# time variant to wide
variables = c("interview_status","long_weights","emp_status")

master_uk_long$interview_date =as.character(master_uk_long$interview_date)

master_bhps_ukhls_wide = reshape2::dcast(master_uk_long , pidp~ year_wave, value.var="interview_date")

year_max = max(as.numeric(colnames(master_bhps_ukhls_wide)[-1]))

colnames(master_bhps_ukhls_wide)[-1] <- paste0("interview_date" ,"_", 1991:year_max)

for (i in variables) {
  
  x = reshape2::dcast(master_uk_long  ,pidp~ year_wave, value.var=i)
  
  colnames(x)[-1] <- paste0(i ,"_", 1991:year_max)
  
  master_bhps_ukhls_wide<-merge(master_bhps_ukhls_wide, x, by = c("pidp"))
  
  # print(i)
  
  rm(x)
}

# merge time invariant with time variant 
master_bhps_ukhls_wide = merge(master_bhps_ukhls_nonchange, master_bhps_ukhls_wide,by = c("pidp"), all.x = T)

################################################################################
# add flags for partnership biography 

sampst <- haven::read_dta(paste0(folder_main_uk, "ukhls/", "xwavedat_protect.dta"),  
                          col_select = c("pidp",	"sampst")) 

ever_married_master <- haven::read_dta(paste0(folder_main_uk, "ukhls/", "xwavedat_protect.dta")) %>%
  select(pidp, evermar_dv, evercoh_dv) %>% 
  mutate(
    ever_married = case_when(
       evermar_dv == 1 ~ 1,  # If ever married or in a civil partnership, set to 1
       evermar_dv == 2 ~ 0,  # If never married, set to 0
       evermar_dv < 0  ~ NA_real_,  # If missing or negative, set to NA
       TRUE ~ 0  # Default case, set to 0
        ))
  
partnership_additional = merge(sampst, ever_married_master, by = c("pidp"), all.x = T)
master_bhps_ukhls_wide = merge(master_bhps_ukhls_wide,partnership_additional,by = c("pidp"), all.x = T)

############## add any_children

anychild_dv <- haven::read_dta(paste0(folder_main_uk, "ukhls/", "xwavedat_protect.dta"),  
                          col_select = c("pidp",	"anychild_dv")) %>% 
  mutate(anychild = case_when(
    anychild_dv == 1 ~ 1,
    anychild_dv ==2 ~2,
    TRUE~NA
  )) %>% select(-anychild_dv)

interview_status_1 = merge(interview_status, anychild_dv, by = c("pidp")) %>% 
  filter(interview_status ==1) %>% select(pidp, anychild ) %>% distinct()


master_bhps_ukhls_wide = merge(master_bhps_ukhls_wide, interview_status_1, by= c("pidp"), all.x = T)

# flags for fertility 

emboost_data <-c()
for (i in 1:length(folder_main_uk)) {
  
  x <- haven::read_dta(paste0(folder_main_uk,"ukhls/",letters[i],"_indresp_protect.dta"), 
                       col_select = c("pidp", 
                                      paste0(letters[i],"_emboost")))
  names(x)[-1] <- str_remove(names(x)[-1], paste0(letters[i], "_"))
  x$year_wave = i+2008
  emboost_data <-bind_rows(emboost_data,x)  
  
}

emboost_data = emboost_data %>% group_by(pidp) %>% 
  select(pidp, emboost) %>% distinct() %>% 
  # [1] for original sample members in the Ethnic Minority Boost sample 
  # [0] otherwise
  mutate(emboost_1 = sum(emboost))


place_birth = haven::read_dta(paste0(folder_main_uk, "ukhls/", "xwavedat_protect.dta")) %>% 
  select(pidp, ukborn, memorig, race_bh)


master_bhps_ukhls_wide_1 = master_bhps_ukhls_wide %>% 
  merge( place_birth, by = c("pidp"), all.x = T) %>% 
  merge(emboost_data,by = c("pidp"), all.x = T)


master_bhps_ukhls_wide_1[master_bhps_ukhls_wide_1$pidp==68001367,]
################################################################################
saveRDS(master_bhps_ukhls_wide, "output/master_bhps_ukhls_wide.rds")
saveRDS(master_bhps_ukhls_wide_1, "output/master_bhps_ukhls_wide.rds")

x = ls()

x=x[x %notin% c("country_set", "biography_set", "type_you_want", "to_download","script",
                "%notin%", "countries", "folder_Australia_1","folder_Australia_2","folder_bhps_1",
                "folder_family_files","folder_fertility", "folder_main_uk", "folder_part_uk",
                "folder_partnership","folder_personal", "folder_retro_shp_1", "folder_retro_shp_2",
                "folder_shp_1","folder_shp_2","folder_soep","folder_uk_fertility_1"
)]

rm(list = x)


