##################### BHPS/UKHLS master file ############################
# vote5	strength of support for stated party	
# indresp	12345679101112BH01BH02BH03BH04BH05BH06BH07BH08BH09BH10BH11BH12BH13BH14BH15BH16BH17BH18

# download required packages for this task: "dplyr", "haven", "tidyr"
source("00_setting_work_space.R")
library(dplyr)
# Information for all persons in household, incl. children and non-respondents
folder_indall <- list.files(
  path = paste0(folder_main_uk, "ukhls/"), 
  pattern = "_indresp_protect.*\\.dta$", 
  full.names = F
)

year_bhps = data.frame(year_wave = 1991:2008,
                       letter = letters[1:18],
                       FIRSTOBS_Y_bhps = 1:18,
                       LASTOBS_Y_bhps  = 1:18)

year_ukhls = data.frame(letter = str_remove(folder_indall, "_indresp_protect.dta"),
                        year_wave = 2009:2019,
                        wave = 1:length(folder_indall),
                        FIRSTOBS_Y_ukhls = 1:length(folder_indall),
                        LASTOBS_Y_ukhls  = 1:length(folder_indall))

master <- haven::read_dta(paste0(folder_main_uk, "ukhls/", "xwavedat_protect.dta")) %>%
  dplyr::select(pidp, pid,birthm, birthy, sex, xwdat_dv) %>% 
  mutate(
    date_birth = paste0(birthy, "-",ifelse(birthm<=9,  paste0("0",birthm), birthm), "-01")) %>%
  rename( BORN_M = birthm,
          BORN_Y = birthy,
          SEX = sex,
          which_study      = xwdat_dv) %>% 
  mutate(which_study = case_when(
    which_study == 1 ~ "in UKHLS",
    which_study == 2 ~ "in BHPS",
    which_study == 3 ~ "in both"))

DataExplorer::plot_missing(master)

# interview date and proxy special sample 
interview_status_bhps <- haven::read_dta(paste0(folder_bhps_1,"xwaveid_bh_protect.dta"),
                                         col_select = c("pid","pidp", "ba_ivfio1_bh", ends_with("ivfio"))) %>% 
  tidyr::pivot_longer(cols = c("ba_ivfio1_bh", paste0("b", letters[2:18], "_ivfio")),
                      names_to  = 'letter',
                      values_to = 'interview_status') %>%
  mutate(letter = str_replace(letter, "_ivfio", "")) %>% 
  mutate(letter = str_replace(letter, "1_bh", "")) %>% 
  mutate(letter = str_replace(letter, "b", "")) %>% 
  dplyr::select(pidp, letter, interview_status ) %>% 
  merge(year_bhps, by = c("letter"), all.x = T) %>% 
  dplyr::select(pidp, year_wave, interview_status )%>% 
  filter(!is.na(interview_status))

DataExplorer::plot_missing(interview_status_bhps)


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
      interview_date = as.Date(paste(istrtdaty, istrtdatm, istrtdatd, sep = "-"), format = "%Y-%M-%d"),
      interview_date_m = as.Date(paste(istrtdaty, istrtdatm, "01", sep = "-"), format = "%Y-%m-%d"),
      year_wave = year_bhps$year_wave[i]) %>% 
    dplyr::select(pidp,year_wave,interview_date,interview_date_m)
  
  interview_date_bhps<-bind_rows(interview_date_bhps,indresp)
  
  rm(indresp)
  
}

# UKHLS: interview date
interview_date_ukhls <-c()
for (i in 1:length(folder_indall)) {
  
  x <- haven::read_dta(paste0(folder_main_uk,"ukhls/",letters[i],"_indall_protect.dta"), 
                       col_select = c("pidp", 
                                      ends_with("intdaty_dv"), # Interview date: Year, derived
                                      ends_with("intdatm_dv"), # Interview date: Month, derived
                                      ends_with("intdatd_dv")  # Interview date: Day, derived
                       ))
  
  names(x)[-1] <- str_remove(names(x)[-1], paste0(letters[i], "_"))
  
  x = x %>% mutate(
    year_wave = year_ukhls$year_wave[i],
    interview_date = as.Date(paste(intdaty_dv, intdatm_dv, intdatd_dv, sep = "-"), format = "%Y-%m-%d"),
    interview_date_m = as.Date(paste(intdaty_dv, intdatm_dv, "01", sep = "-"), format = "%Y-%m-%d")
  ) %>% dplyr::select(pidp,year_wave,interview_date,interview_date_m)
  
  interview_date_ukhls<-rbind(interview_date_ukhls, x)
  rm(x)
  
}

interview_date = bind_rows(interview_date_bhps, interview_date_ukhls)
interview_date = interview_date %>% rename(date = interview_date_m)

DataExplorer::plot_missing(interview_date)

first_last_date = interview_date %>% group_by(pidp) %>% 
  summarise(FIRSTOBS_date = min(interview_date),
            LASTOBS_date = max(interview_date))

length(unique(first_last_date$pidp))==nrow(first_last_date)


################################################################################
# Define additional variables by wave
extra_vars <- list(
  mastat = c(1:18),# De facto marital status
  region = c(1:18),# Region (England, Wales, Scotland, Northern Ireland)
  hiqual_dv= c(1:18),# Highest qualification
  isced = c(1:18),# ISCED education level
  fimngrs_dv = c(1:18),# Total monthly personal gross income
  fimnlabgrs_dv = c(1:18),# Total monthly labour gross income
  vote1 = c(1:18),# Supports a particular political party
  vote2 = c(1:18),# Closer to one political party than others
  vote4 = c(1:18),# Political party they feel closest to
  hlstat = c(1:8, 10:18), # Self-assessed health status
  vote6	= c(1,2,3,4,5,6,11,12,13,14,15,16,17,18),# Level of interest in politics	indresp	
  vote8 = c(2,5,7,8,9,10,11,12,13,14,15,16,17,18), # party voted for in last general election
  vote7 = c(2,5,7,8,9,10,11,12,13,14,15,16,17,18),	# voted in last general election
  opeur4 = c(9,12)	# Voting intention in EU referendum	indresp	BH09BH12
  
)

variable_bhps<- c()
for (i in 1:18) {
  prefix <- paste0("b",letters[i], "_")
  # Base variable selection
  var <- c("pidp")
  # Add extra variables based on wave
  var <- c(var, unlist(lapply(names(extra_vars), function(v) {if (i %in% extra_vars[[v]]) paste0(prefix, v)})))
  # Load dataset
  indresp <- haven::read_dta(paste0(folder_bhps_1,paste0("b",letters[i],"_indresp_protect.dta")), col_select = var)
  # Clean column names
  colnames(indresp) <- sub(prefix, "", colnames(indresp))
  # Add survey wave year
  indresp$year_wave <- 1990 + i

  print(i)
  
  # Append results
  variable_bhps <- bind_rows(variable_bhps, indresp)
}

# encode variables

# encode variables 
variable_bhps_1 = variable_bhps %>% mutate(
  # supports a particular political party    
  vote1_1 = case_when(
    vote1 ==1 ~1,
    vote1 ==2 ~0,
    TRUE ~ -1
  ),
  vote1_1 = factor(vote1_1, levels = c(0,1,-1), labels = c("no", "yes", "missing")), 
  ####
  # supports a particular political party    
  vote2_2 = case_when(
    vote2 ==1 ~1,
    vote2 ==2 ~0,
    TRUE ~ -1
  ),
  vote2_2 = factor(vote2_2, levels = c(0,1,-1), labels = c("no", "yes", "missing")), 
  
  vote1_vote2 = ifelse(vote1==1 | vote2==1, "ok", "no political oreintation"),
  ####################### 
  political_category = case_when(
    ##### right wing ##########################################################
    # 1 [conservative]                63400 476187 13.3     
    # 7 [ulster unionist]              2718 476187  0.571   
    # 9 [alliance party]               1059 476187  0.222   
    # 10 [democratic unionist]          2768 476187  0.581   
    # 12 [uk independence party]        3780 476187  0.794   
    # 13 [british national party]        214 476187  0.0449  
    vote4 %in% c(1, 7, 9,12, 13, 16, 27, 10, 28, 29) ~ "Right-wing", 
    ##### left wing ##########################################################
    # 2 [labour]                      79738 476187 16.7     
    # 3 [liberal democrat]            16067 476187  3.37    
    # 4 [scottish national party]      5656 476187  1.19    
    # 5 [plaid cymru]                  1275 476187  0.268   
    # 6 [green party]                  5153 476187  1.08    
    # 8 [sdlp]                         1870 476187  0.393   
    # 11 [sinn fein]                    1661 476187  0.349   
    # 14 [mebyon kernow]                 363 476187  0.0762  
    # 15 [monster raving loony party]     14 476187  0.00294 
    vote4 %in% c(2, 3, 4, 5, 6, 8, 11, 14,15,18, 19, 20, 23) ~ "Left-wing", 
    ##### no political oreintation ###########################################
    # -10 [Not available for IEMB]       4615 476187  0.969   
    # -9 [missing]                     19711 476187  4.14    
    # -8 [inapplicable]               193294 476187 40.6     
    # -7 [proxy respondent]            27089 476187  5.69    
    # -2 [refusal]                      1688 476187  0.354   
    # -1 [don't know]                    849 476187  0.178   
    # 96 [Can't vote]                    653 476187  0.137   
    # 97 [Other party]                  3257 476187  0.684   
    vote4 %in% c(95, 96, 97, 22, -1,-2) | vote1_vote2 =="no political oreintation"  ~ "no political oreintation",  # None, Can't vote, Missing
    TRUE ~ NA
  ),
  
  political_category = factor(political_category, 
                              levels = c("Right-wing","Left-wing","no political oreintation", "-1"), 
                              labels = c("Right-wing","Left-wing","no political oreintation", "-1")),
  
  ############################################################################
  
  political_category_1 = case_when(
    vote4 %in% c(1) ~ "conservative", 
    vote4 %in% c(2) ~ "labour",
    vote4 %notin% c(1,2,95, 96, 97, 22, -1,-2) ~ "other",
    vote4 %in% c(95, 96, 97, 22, -1,-2) | vote1_vote2 =="no political oreintation"  ~ "no political oreintation",  # None, Can't vote, Missing
    TRUE ~ NA
  ),
  
  political_category_1 = factor(political_category_1, 
                                levels = c("conservative","labour","other","no political oreintation"), 
                                labels = c("conservative","labour","other","no political oreintation")),
  
  political_category_2 = case_when(
    political_category_1 =="conservative" & vote6 %in% c(1,2)~ "conservative & interested",
    political_category_1 =="conservative" & vote6 %in% c(3,4)~ "conservative & not interested", 
    political_category_1 =="labour" & vote6 %in% c(1,2)~ "labour & interested",
    political_category_1 =="labour" & vote6 %in% c(3,4)~ "labour & not interested",
    political_category_1 =="other" & vote6 %in% c(1,2)~ "other & interested",
    political_category_1 =="other" & vote6 %in% c(3,4)~ "other & not interested",
    TRUE ~ "no political oreintation"
  ),
   edu = case_when(hiqual_dv %in% c(1, 2) ~ 2,               # Degree, Other higher
   hiqual_dv %in% c(3, 4, 5, 9) ~ 1, # A level, GCSE, Other, None
   hiqual_dv %in% c(-9, -8, -2, -1) ~ NA_integer_ # Missing/NA codes
  ),

  edu = factor(edu, levels = c(1,2,-1), labels = c("low&middle", "high","missing")),
  
  region_1 = case_when(
    
    region %in% 1:16 ~ 1, # "[England]",
    region == 17 ~ 2,     # "[Wales]",
    region == 18 ~ 3,     # "[Scotland]",
    region == 19 ~ 4,     # "[Northern Ireland]",
    region %in% c(-9, -8, -7, -2, -1) ~ NA_integer_
  ),
  region_1 = factor(region_1, levels =c(1,2,3,4,-1), labels = c("England", "Wales","Scotland","Northern Ireland", "missing")),
  
  gen_health_1 = case_when(
    hlstat  %in% c(1,2) ~ 3,
    hlstat  %in% c(3) ~ 2,
    hlstat  %in% c(4,5) ~ 1,
    TRUE ~-1
  ),
  gen_health_1 = factor(gen_health_1, levels = c(1,2,3,-1), labels = c("poor", "ok", "excellent", "missing")),
  
  
  marital_status = case_when(
    mastat %in% c(0,1,4:9) ~ 1, # single
    mastat %in% c(10) ~ 2,  # cohabit
    mastat %in% c(2,3) ~ 3, # married
    mastat<0 ~ -1
    
  ),
  marital_status = factor(marital_status, levels = c(1,2,3,-1), labels = c("single","cohabit","married", "missing")),
  gross_income = fimnlabgrs_dv 
  
  ) 


variable_bhps_1 %>% dplyr::select(pidp, year_wave,marital_status,
                              edu,
                              gen_health_1,
                              gross_income,
                              region_1,
                              political_category_1,vote7,vote8)


################################################################################
# UKHLS

max_1 = length(year_ukhls$year_wave)

extra_vars <- list(
  mastat_dv = c(1:max_1),# De facto marital status
  country = c(1:max_1),# Region (England, Wales, Scotland, Northern Ireland)
  qfhigh_dv = c(1:max_1),# ISCED education level
  fimngrs_dv = c(1:max_1),# Total monthly personal gross income
  fimnlabgrs_dv = c(1:max_1),# Total monthly labour gross income
  vote1 = c(1,2,3,4,5,6,7,9,10,11,12),# Supports a particular political party
  vote2 = c(1,2,3,4,5,6,7,9,10,11,12),# Closer to one political party than others
  vote4 = c(1,2,3,4,5,6,7,9,10,11,12),# Political party they feel closest to
  sf1 = c(1:max_1), # Self-assessed health status
  vote6	= c(1,2,3,4,5,6,7,9,10,11,12),# Level of interest in politics	indresp	
  vote8 = c(2,7,8,9,10,11,12,14), # party voted for in last general election
  vote7= c(2,7,8,9,10,11,12,14),	# voted in last general election
  scotvot3 = c(5,6), #	voted in scottish referendum	indresp	56
  scotvot1 = c(5,6),  # plan to vote in scottish referendum	indresp	56
  euref	 = c(10,11,12),   # Voted in EU Referendum	indresp	101112
  scotvot2 = c(5,6),  #	how expects to vote in scottish referendum	indresp	56
  voteeuref	= c(10,11,12),# How voted in EU Referendum	indresp	101112
  scotvot4= c(5,6)	# how voted in scottish referendum	indresp	56
  
)

variable_ukhls<- c()
for (i in 1:max_1) {
  prefix <- paste0(letters[i], "_")
  # Base variable selection
  var <- c("pidp")
  # Add extra variables based on wave
  var <- c(var, unlist(lapply(names(extra_vars), function(v) {if (i %in% extra_vars[[v]]) paste0(prefix, v)})))
  # Load dataset
  indresp <- haven::read_dta(paste0(folder_main_uk, "ukhls/", prefix, "indresp_protect.dta"), col_select = var)
  # Clean column names
  colnames(indresp) <- sub(prefix, "", colnames(indresp))
  # Add survey wave year
  indresp$year_wave <- 2008 + i
  
  print(i)
  
  # Append results
  variable_ukhls <- bind_rows(variable_ukhls, indresp)
}


# encode variables 
variable_ukhls_1 = variable_ukhls %>% mutate(
  # supports a particular political party    
  vote1_1 = case_when(
    vote1 ==1 ~1,
    vote1 ==2 ~0,
    TRUE ~ -1
  ),
  vote1_1 = factor(vote1_1, levels = c(0,1,-1), labels = c("no", "yes", "missing")), 
  ####
  # supports a particular political party    
  vote2_2 = case_when(
    vote2 ==1 ~1,
    vote2 ==2 ~0,
    TRUE ~ -1
  ),
  vote2_2 = factor(vote2_2, levels = c(0,1,-1), labels = c("no", "yes", "missing")), 
  
  vote1_vote2 = ifelse(vote1==1 | vote2==1, "ok", "no political oreintation"),
  ####################### 
  political_category = case_when(
    ##### right wing ##########################################################
    # 1 [conservative]                63400 476187 13.3     
    # 7 [ulster unionist]              2718 476187  0.571   
    # 9 [alliance party]               1059 476187  0.222   
    # 10 [democratic unionist]          2768 476187  0.581   
    # 12 [uk independence party]        3780 476187  0.794   
    # 13 [british national party]        214 476187  0.0449  
    vote4 %in% c(1, 7, 9,12, 13, 16, 27, 10, 28, 29) ~ "Right-wing", 
    ##### left wing ##########################################################
    # 2 [labour]                      79738 476187 16.7     
    # 3 [liberal democrat]            16067 476187  3.37    
    # 4 [scottish national party]      5656 476187  1.19    
    # 5 [plaid cymru]                  1275 476187  0.268   
    # 6 [green party]                  5153 476187  1.08    
    # 8 [sdlp]                         1870 476187  0.393   
    # 11 [sinn fein]                    1661 476187  0.349   
    # 14 [mebyon kernow]                 363 476187  0.0762  
    # 15 [monster raving loony party]     14 476187  0.00294 
    vote4 %in% c(2, 3, 4, 5, 6, 8, 11, 14,15,18, 19, 20, 23) ~ "Left-wing", 
    ##### no political oreintation ###########################################
    # -10 [Not available for IEMB]       4615 476187  0.969   
    # -9 [missing]                     19711 476187  4.14    
    # -8 [inapplicable]               193294 476187 40.6     
    # -7 [proxy respondent]            27089 476187  5.69    
    # -2 [refusal]                      1688 476187  0.354   
    # -1 [don't know]                    849 476187  0.178   
    # 96 [Can't vote]                    653 476187  0.137   
    # 97 [Other party]                  3257 476187  0.684   
    vote4 %in% c(95, 96, 97, 22, -1,-2) | vote1_vote2 =="no political oreintation"  ~ "no political oreintation",  # None, Can't vote, Missing
    TRUE ~ NA
  ),
  
  political_category = factor(political_category, 
                              levels = c("Right-wing","Left-wing","no political oreintation", "-1"), 
                              labels = c("Right-wing","Left-wing","no political oreintation", "-1")),
  
  ############################################################################
  
  political_category_1 = case_when(
    vote4 %in% c(1) ~ "conservative", 
    vote4 %in% c(2) ~ "labour",
    vote4 %notin% c(1,2,95, 96, 97, 22, -1,-2) ~ "other",
    vote4 %in% c(95, 96, 97, 22, -1,-2) | vote1_vote2 =="no political oreintation"  ~ "no political oreintation",  # None, Can't vote, Missing
    TRUE ~ NA
  ),
  
  political_category_1 = factor(political_category_1, 
                                levels = c("conservative","labour","other","no political oreintation"), 
                                labels = c("conservative","labour","other","no political oreintation")),
  
  political_category_2 = case_when(
    political_category_1 =="conservative" & vote6 %in% c(1,2)~ "conservative & interested",
    political_category_1 =="conservative" & vote6 %in% c(3,4)~ "conservative & not interested", 
    political_category_1 =="labour" & vote6 %in% c(1,2)~ "labour & interested",
    political_category_1 =="labour" & vote6 %in% c(3,4)~ "labour & not interested",
    political_category_1 =="other" & vote6 %in% c(1,2)~ "other & interested",
    political_category_1 =="other" & vote6 %in% c(3,4)~ "other & not interested",
    TRUE ~ "no political oreintation"
  )
)  %>% 
  mutate(edu = case_when(
    qfhigh_dv %in% c(13, 14, 15, 16, 96) ~ 1, # GCSE, CSE, O-level, None of the above
    qfhigh_dv %in% c(7, 8, 9, 10, 11, 12) ~ 1, # A-levels, Highers, Baccalaureates
    qfhigh_dv %in% c(1, 2, 3, 4, 5, 6) ~ 2, # University degrees & diplomas
    qfhigh_dv %in% c(-9, -8) ~ NA, # Missing/Inapplicable
    TRUE ~ NA_integer_
  ),
  edu = factor(edu, levels = c(1,2,-1), labels = c("low&middle", "high","missing")),
  
  region_1 = case_when(
    
    country == 1 ~ 1, # [England]        
    country == 2 ~ 2, # [Wales]             
    country == 3 ~ 3, # [Scotland]          
    country == 4 ~ 4, # [Northern Ireland] 
    TRUE ~ -1
  ),
  region_1 = factor(region_1, levels =c(1,2,3,4,-1), labels = c("England", "Wales","Scotland","Northern Ireland", "missing")),
  
  gen_health_1 = case_when(
    sf1  %in% c(1,2) ~ 3,
    sf1  %in% c(3) ~ 2,
    sf1  %in% c(4,5) ~ 1,
    TRUE ~-1
  ),
  gen_health_1 = factor(gen_health_1, levels = c(1,2,3,-1), labels = c("poor", "ok", "excellent", "missing")),
  
  
  marital_status = case_when(
    mastat_dv %in% c(0,1,4:9) ~ 1, # single
    mastat_dv %in% c(10) ~ 2,  # cohabit
    mastat_dv %in% c(2,3) ~ 3, # married
    mastat_dv<0 ~ -1
    
  ),
  marital_status = factor(marital_status, levels = c(1,2,3,-1), labels = c("single","cohabit","married", "missing")),
  gross_income = fimnlabgrs_dv 
  
  ) 


# variable_ukhls_1 %>% select(pidp, year_wave,political_category,region_1,edu, gen_health_1,marital_status) %>% head()
# 
# 
# variable_ukhls_1 %>% select(pidp, year_wave,marital_status,
# edu,
# gen_health_1,
# gross_income,
# region_1,
# political_category_1,vote7,vote8)


# combine bhps and ukhls 


both_bhps_ukhls = bind_rows(variable_bhps_1,variable_ukhls_1)

saveRDS(both_bhps_ukhls,"output/1_may_2025_both_bhps_ukhls.rds")

################## add fertility ###############################################
master_memoring = readRDS("output/master_bhps_ukhls_wide.rds") %>% dplyr::select(pidp, memorig)

bhps_ukhls_fertility = readRDS("output/bhps_ukhls_fertility.rds") %>%
  dplyr::select(pidp, KID_1, KID_Y1, KID_M1)

bhps_ukhls_fertility = readRDS("output/bhps_ukhls_fertility.rds") %>%
  dplyr::select(pidp, KID_1, KID_Y1, KID_M1) %>% 
  filter(KID_Y1>0| is.na(KID_Y1)) %>% 
  mutate(KID_M1 = ifelse(KID_M1<0 | is.na(KID_M1),1,KID_M1)) %>% 
  mutate(KID_date1= as.Date(paste(KID_Y1, KID_M1, 1, sep = "-"), format = "%Y-%m-%d")) %>%  
  merge(master, by = c("pidp")) %>% 
  dplyr::select(pidp,date_birth,SEX,KID_1,KID_date1, KID_Y1) %>% 
  mutate(date_birth = as.Date(date_birth)) %>% 
  filter(date_birth<KID_date1 | is.na(KID_date1)) %>%  # how many individuals?
  merge(first_last_date, by = c("pidp"), all.x = T) %>% 
  mutate(time_in_months = as.numeric(difftime(KID_date1, date_birth, units = "days")) / 30.44) %>% 
  mutate(time_in_months = ifelse(is.na(time_in_months),difftime(LASTOBS_date, date_birth, units = "days")/ 30.44,time_in_months )) %>% 
  filter(time_in_months>0) %>% distinct() %>% 
  filter(SEX==2) %>% merge(master_memoring, by = c("pidp")) %>% filter( memorig %notin% c(7,8))

data_split <- survSplit(Surv(time_in_months, KID_1) ~ ., 
                        data = bhps_ukhls_fertility, 
                        cut = seq(0, 
                                  max(bhps_ukhls_fertility$time_in_months, na.rm = TRUE), by = 1),  # 1-month intervals
                        episode = "month",  # Name the interval variable "month"
                        id = "pid_split") %>%   # Use a different name for the id
  mutate(date = date_birth %m+% months(month-1)) %>% 
  # calculate age so to filter to retrospective age
  mutate(age = as.integer(floor(difftime(date, date_birth, units = "days")/ 365))) %>% 
  # filter to age which is of our interest 
  filter(age %in% c(18:44))%>% 
  mutate(age_2 = age*age)

# data_split %>% group_by(pidp) %>% arrange(date, .by_group = T) %>% 
#   slice(n()) %>% group_by(KID_1) %>% summarise(n=n())%>% ungroup() %>% mutate(total = sum(n), percent = 100*(n/total))
# KID_1     n total percent
# 1     0 18232 47637    38.3
# 2     1 29405 47637    61.7
library(fixest) 
model <- feols(KID_1 ~ age +   age_2| pidp,data = data_split)
summary(model)


# We limit to individuals who are in total_ukhls_climate_1
data_split_1 = data_split[data_split$pidp %in% both_bhps_ukhls$pidp,]

data_split_1 = data_split_1 %>% group_by(pidp) %>% arrange(date,.by_group = T) %>% 
  mutate(KID_1_dummy = lead(KID_1, 9)) %>% filter(!is.na(KID_1_dummy))


model <- feols(KID_1_dummy ~ age +   age_2 | pidp,data = data_split_1)
summary(model)

data_split_2 = as.data.table(data_split_1) %>% merge(as.data.table(interview_date), by = c("pidp","date"), all.x=T) %>% 
  merge(as.data.table(both_bhps_ukhls), by = c("pidp", "year_wave"), all.x = T) %>% 
  mutate(age_group = cut(age, breaks = c(-1,14,25,29,34,39,44,120),
                         labels = c("0-18","18-25", "26-29", "30-34", "35-39", "40-44", "45-120") )) %>% 
  mutate(wave_interval = cut(year_wave, breaks = c(1990, 1995, 1999, 2003, 2007, 2011, 2015, 2020))) 

data_split_2 %>% group_by(age_group) %>% summarise(min(age), max(age))

data_split_2 %>% group_by(SEX) %>% summarise(n())


# select necessary variables
data_split_3 = data_split_2 %>%  
  filter(age_group %in% c("18-25", "26-29", "30-34", "35-39", "40-44")) %>% 
  group_by(pidp) %>% arrange(date,.by_group = T) %>% 
  fill(year_wave,marital_status,
       edu,
       gen_health_1,
       gross_income,
       region_1,
       political_category_1,vote7,vote8,
       .direction = "down") %>% 
  fill(scotvot1, 
       scotvot2, 
       scotvot3,
       scotvot4, 
       euref, 
       voteeuref, 
       opeur4, .direction = "downup") %>% 
  group_by(pidp) %>% arrange(date,.by_group = T)


################################################################################
# master_int_status = readRDS("output/master_bhps_ukhls_wide.rds") %>% 
#   pivot_longer(cols = starts_with("interview_status"),
#                names_to = "year_wave",
#                values_to = "interview_status") %>% 
#   select(pidp, year_wave, interview_status) %>% 
#   mutate(year_wave = as.numeric(stringr::str_replace(year_wave ,"interview_status_","")))
# 
# master_date = readRDS("output/master_bhps_ukhls_wide.rds") %>% 
#   pivot_longer(cols = starts_with("interview_date"),
#                names_to = "year_wave",
#                values_to = "interview_date") %>% 
#   select(pidp, year_wave, interview_date) %>% 
#   mutate(year_wave = as.numeric(stringr::str_replace(year_wave ,"interview_date_","")))
# 
# fwintvd_dv = haven::read_dta(paste0(folder_main_uk, "ukhls/", "xwavedat_protect.dta")) %>% 
#   select(pidp, fwintvd_dv)%>% filter(fwintvd_dv>0) %>% 
#   mutate(year_wave = fwintvd_dv +2008)
# 
# master_date_1 = master_date %>% merge(fwintvd_dv, by = c("pidp")) %>% 
#   # filter( year_wave.x== year_wave.y) %>% 
#   mutate(
#     interview_date = as.Date(interview_date),
#     interview_date = floor_date(interview_date, unit = "month")) %>% 
#   select(pidp, interview_date)
# 
# 
# data_split_4 = merge(as.data.table(data_split_3), as.data.table(master_int_status), by = c("pidp", "year_wave"), all.x = T) %>% 
#   group_by(pidp) %>% arrange(date, .by_group = T) %>% 
#   fill(interview_status, .direction = "down") %>% 
#   merge(as.data.table(master_date_1), by = c("pidp"), all.x = T)


data_split_3 = data_split_3 %>% group_by(pidp) %>% fill(interview_date, .direction = "down")

data_split_5 = data_split_3[data_split_3$interview_date<=data_split_3$date, ] %>% filter(!is.na(interview_date))

data_split_5[data_split_5$pidp =="50335",c("pidp", "age", "date", "date_birth", "interview_date", "FIRSTOBS_date")]

data_split_3[data_split_3$pidp =="50335",c("pidp", "age", "date", "date_birth", "interview_date", "FIRSTOBS_date")]


interview_date[interview_date$pidp=="50335",]

min(data_split_5$date)
max(data_split_5$date)
DataExplorer::plot_missing(data_split_5)
data_split_5 %>% group_by(SEX) %>% summarise(n())
data_split_5 %>% group_by(age_group) %>% summarise(min(age), max(age))

data_split_5  = data_split_5 %>% group_by(pidp) %>% 
  arrange(date,.by_group = T) %>% 
  fill(scotvot1, 
       scotvot2, 
       scotvot3,
       scotvot4, 
       euref, 
       voteeuref, 
       opeur4, .direction = "down") %>% 
  fill(scotvot1, 
       scotvot2, 
       scotvot3,
       scotvot4, 
       euref, 
       voteeuref, 
       opeur4, .direction = "up")
  
  
data_split_5  = data_split_5 %>% group_by(pidp) %>% 
  arrange(date,.by_group = T) %>% fill(voteeuref, .direction = "updown")

data_split_5$voteeuref
# Labels:
# value                                 label
# -9                               Missing
# -8                          Inapplicable
# -2                               Refused
# -1                             Dont know
# 1 Remain a member of the European Union
# 2              Leave the European Union


DataExplorer::plot_missing(data_split_5[,c("KID_1_dummy", "age", "age_2", "edu", "marital_status", "political_category_1",
                "scotvot1", 
                "scotvot2", 
                "scotvot3",
                "scotvot4", 
                "euref", 
                "voteeuref", 
                "opeur4")])


# make simple regression #######################################################


saveRDS(data_split_5, "output/1_may_data_split_5.rds")

write_dta(data_split_5, "output/1_may_data_split_5.dta")

data_split_5_x <- read_dta("output/1_may_data_split_5.dta")

ewa_1 = data_split_5_x[,c("pidp","KID_1_dummy", "age", "age_2", "edu", "marital_status", "political_category_1",
                                                         "euref", 
                                                         "voteeuref")]


DataExplorer::plot_missing(ewa_1[,c("KID_1_dummy", "age", "age_2", "edu", "marital_status", "political_category_1",
                                    "euref", 
                                    "voteeuref")])


ewa = ewa_1 %>% filter(KID_1_dummy==1)

ewa = data_split_5_x[data_split_5_x$pidp %in% ewa$pidp,c("pidp" , "year_wave", "date","voteeuref", "KID_1_dummy")]

ewa_x = data_split_5_x %>% group_by(KID_Y1) %>% summarise(n())



ewa = data_split_5_x[data_split_5_x$pidp==1634326374,] %>% filter(!is.na(pidp))

ewa_2 = ewa[,c("pidp" , "year_wave", "date","voteeuref", "KID_1_dummy")]

ewa_2

min(data_split_5_x$date)
max(data_split_5_x$date)

data_split_6 <- data_split_5[complete.cases(data_split_5[, c("pidp", "KID_1_dummy", "age", "age_2", "edu", "marital_status", "political_category_1")]), ]

data_split_6 %>% group_by(KID_1_dummy) %>% summarise(n())


nrow(data_split_6)==531997

# library(speedglm)
# model_first <- speedglm(as.formula(paste0("KID_1_dummy ~ age + age_2 + edu + marital_status + political_category_1 + (1|pidp) " )),
#                        data = data_split_5_x,
#                        family = binomial(link = "cloglog"))
# 
# model_first



print( summary(model_first))


windowsFonts(A = windowsFont("Gordita"))

font_c = 25

x = sjPlot::plot_model(model_first, 
                       type = "eff", 
                       dodge = 1,
                       ci.lvl = 0.83, 
                       title = "",
                       terms = c( "political_category_1"),  # interaction terms as separate strings
                       # axis.title = c("Volume of news on climate change","Predicted Probability of First Birth"),
                       axis.title = c("political_category_1","Predicted Probability of First Birth"),
                       # colors = c("darkred", "darkblue", "black","gray"),"mediumpurple"
                       colors = c("#004180", "#FF1D1D"),
                       line.size = 1.5, 
                       legend.title = "Volume of news on climate change",
                       dot.size = 3, grid.breaks=0)+theme_minimal()+
  # theme(legend.position = "bottom")+
  theme(plot.margin=unit(c(0.5,0.7,0.5,0.5), 'cm')) +
  # ylim(-0.001, 0.007)+
  theme(axis.text = element_text(face="bold", color="black", angle =0,
                                 size=font_c, family="A"),
        axis.text.x = element_text(face="bold", color="black", angle =0, hjust = 0.5,
                                   size=font_c, family="A", margin = margin(t = 15)),
        axis.text.y = element_text(face="bold", color="black",
                                   size=font_c, family="A"),
        axis.title.x = element_text(face="bold", color="black", angle =0,
                                    size=font_c, family="A", margin = margin(t = 25)),
        
        axis.title.y = element_text(face="bold", color="black",
                                    size=font_c, family="A"),
        plot.title = element_text(family = "A", face = "bold", size = (15), color="black", margin = margin(b = 20)),
        plot.subtitle = element_text(family = "A", face = "bold", size = (10), color="black"),
        legend.text =  element_text(face="bold", color="black", angle =0,
                                    size=font_c-5, family="A"),
        legend.title = element_text(face="bold", color="black", angle =0,
                                    size=font_c-5, family="A"),
        panel.grid.major.x = element_blank() ,
        # legend.position = c(0.8, 0.8),
        legend.position = c(0.17, 0.95),
        # explicitly set the horizontal lines (or they will disappear too)
        panel.grid.major.y = element_line( size=.1, color="gray" ),
        panel.background = element_blank())


x

ggsave("figure_1.jpg", plot = x)
























