##################### BHPS/UKHLS fertility history ############################
# This R script is designed to process and clean data related to fertility of individuals 
# in BHPS and UKHS study datasets. The script performs various data transformations and merges 
# to construct a detailed history of fertility for individuals, incorporating three main sources 
# of information: family matrix, non-resident children and file created by Pronzato,(2010).
# The strategy it to transform all those three sources together in long format recalculate 
# the parity and at the end transform it to wide format. Afterward add childless individuals 
# (those who did not appear in any of those aforementioned files).

# 1. Load Required Packages:
source("00_setting_work_space.R")

# 2. Prepare family matrix xhhrel_protect.dta
# Extract information on biological children from a family matrix file 
# (`xhhrel_protect.dta`) containing children IDs. From this data - family matrix xhhrel_protect.dta 
# biological children, we take only biological children.

biological_children <- haven::read_dta(paste0(folder_uk_fertility_1,"xhhrel_protect.dta"), 
                   col_select = c(
                     "pidp",
                     "bcx_N",                   # total no. of biological children	xhhrel
                     starts_with("bcx_pidp_"))) # biological child 1-16 pidp

# biological_children[,paste0("KID_", 1:16)]<-apply(biological_children[,paste0("bcx_pidp_", 
#                                                                             1:16)], 2, function(y)
#     ifelse(y!=-8, 1, 0))
# 
# biological_children$tot_kids = rowSums(biological_children[,paste0("KID_", 1:16)])


# - Individuals with no children id are filtered into a separate data frame (`childless`). 
# But the fact that they do not have any non-resident children. We will verify it by including information from files: 
#   _natchild_protect.dta and f_natchild_protect.dta for ukhls and "b","k","l"_childnt_protect.dta for bhps.

childless<-filter(biological_children, bcx_N==0)

biological_children<-biological_children %>% filter(bcx_N!=0)

nr_kids<-length(names(biological_children))-2 # it should be 16

# - transform those data with child id to long format to add children 
# characteristics from master file like: SEX and BORN_Y.

biological_children_long<-biological_children %>% 
  tidyr::pivot_longer(cols=paste0("bcx_pidp_",1:nr_kids),
                      names_to='parity',
                      values_to='kid_pid') %>% 
  mutate(parity = str_replace(parity, "bcx_pidp_",""),
         kid_pid = ifelse(kid_pid==-8,NA, kid_pid)) %>% 
  rename(nr_kids_bcx = bcx_N) 


nr_kids_bcx<-biological_children_long %>% select(pidp,nr_kids_bcx) %>% distinct()

parents<-filter(biological_children_long, !is.na(kid_pid))

# Merge children id with master to extract date of birth 

master_uk_1 = readRDS("output/master_bhps_ukhls_wide.rds") %>% select( pidp,SEX,BORN_Y,BORN_M) %>% distinct()

children<-master_uk_1  %>% 
  rename(kid_pid=pidp, 
         KID_S = SEX,
         KID_Y=BORN_Y, 
         KID_M=BORN_M )


table(parents$KID_Y)

#  -9 missing or wild

parents<-merge(parents, children, by= c("kid_pid")) %>% 
  select( pidp, nr_kids_bcx, parity, KID_S,KID_M,KID_Y,kid_pid) %>% 
  # noooo !!!!
  mutate(KID_Y = ifelse(KID_Y<0,-1,KID_Y),
         KID_M = ifelse(KID_M<0,-1,KID_M),
         KID_S = ifelse(KID_S<0,-1,KID_S)) %>% filter(!is.na(KID_Y))

parents_family_matrix <- parents %>% 
  group_by(pidp) %>% arrange(KID_Y, .by_group = T) %>% 
  mutate(parity=1:n(),
         KID_S = as.numeric(KID_S),     
         KID_M = as.numeric(KID_M),     
         KID_Y = as.numeric(KID_Y)) %>% ungroup()

rm(biological_children_long, biological_children, children, parents, master_uk_1)

# 3. Add Non-Resident Children:
# for ukhls:a_natchild_protect.dta and f_natchild_protect.dta
# where we have information on children gender and year of birth (KID_S = a_lchsx,  KID_Y =a_lchdoby)

non_resident_a = read_dta(paste0(folder_uk_fertility_1,"a_natchild_protect.dta"), 
                        col_select = c("pidp","a_lchsx","a_lchdoby", "a_childno", "a_lchlv")) %>% filter(a_lchlv!=1 ) %>% 
  rename(parity=a_childno, KID_S = a_lchsx,  KID_Y =a_lchdoby ) %>% 
  # f_lchlv # Child still lives with parent
  # 1              yes
  # 2               no
  # 3 SPONTANEOUS Died
  # 4        stillborn
  # -9          missing
  # -8     inapplicable
  # -2          refusal
  # -1       don't know
  filter(a_lchlv %in%  c(2,3,4)) %>% 
  mutate(KID_S = as.numeric(KID_S),
         KID_Y = as.numeric(KID_Y)) %>% select(-parity, -a_lchlv) %>% 
  mutate(KID_Y = ifelse(KID_Y<0,-1,KID_Y),
         KID_S = ifelse(KID_S<0,-1,KID_S)
  )
  
non_resident_f = read_dta(paste0(folder_uk_fertility_1,"f_natchild_protect.dta"), 
                        col_select = c("pidp","f_lchsx","f_lchdoby", "f_childno", "f_lchlv")) %>% 
  # f_lchlv # Child still lives with parent
  # 1              yes
  # 2               no
  # 3 SPONTANEOUS Died
  # 4        stillborn
  # -9          missing
  # -8     inapplicable
  # -2          refusal
  # -1       don't know
  filter(f_lchlv %in%  c(2,3,4)) %>% 
  rename(parity=f_childno, KID_S = f_lchsx,  KID_Y =f_lchdoby ) %>% 
  mutate(KID_S = as.numeric(KID_S),
         KID_Y = as.numeric(KID_Y)) %>% select(-parity, -f_lchlv) %>% 
  mutate(KID_Y = ifelse(KID_Y<0,-1,KID_Y),
         KID_S = ifelse(KID_S<0,-1,KID_S)
         )


# for bhps:
# b_childnt_protect.dta, k_childnt_protect.dta, l_childnt_protect.dta where we have
# information on children gender, month and year of birth (KID_M=lchbm, KID_Y=lchby4, KID_S=lchsx)

bhps_letter = c("b","k","l")

for (letter in bhps_letter) {
  
  non_resident_bhps = read_dta(paste0(folder_bhps_1 ,"b", 
                                      letter,"_childnt_protect.dta"),
                               col_select = c("pidp","pid",
                                              paste0("b",letter,"_","lchbm"), 
                                              paste0("b",letter,"_","lchby4"),
                                              paste0("b",letter,"_","lchsx"),
                                              paste0("b",letter,"_","lchlv")
                               ))
  
  colnames(non_resident_bhps) <- gsub(paste0("^b",letter,"_"), "", colnames(non_resident_bhps))
  
  non_resident_bhps = non_resident_bhps %>% 
    rename(KID_M=lchbm,
           KID_Y=lchby4,
           KID_S=lchsx) %>% 
    group_by(pidp) %>% arrange(KID_Y, .by_group = T) %>% 
    # bl_lchlv natural child still lives in resp. hh
    # -9      missing
    # -8 inapplicable
    # -7        proxy
    # -2      refusal
    # -1   don't know
    # 1          yes
    # 2           no
    # 3         died
    # 4    stillborn
    filter(lchlv %in% c(2,3,4))
  
  assign(x = paste0("non_resident_bhps_",letter),value = non_resident_bhps)

}

non_resident_bhps = merge(get(paste0("non_resident_bhps_","b")),  
                          get(paste0("non_resident_bhps_","k")),by = c( "pidp","pid"), all = T) %>% 
  mutate(KID_M  =  coalesce(KID_M.x, KID_M.y),
         KID_Y = coalesce(KID_Y.x, KID_Y.y),
         KID_S = coalesce(KID_S.x,KID_S.y)) %>% select(pid, pidp,KID_M, KID_Y, KID_S) %>% 
  merge(get(paste0("non_resident_bhps_","l")), by = c( "pidp","pid"), all = T) %>% 
  mutate(KID_M  =  coalesce(KID_M.x, KID_M.y),
         KID_Y = coalesce(KID_Y.x, KID_Y.y),
         KID_S = coalesce(KID_S.x,KID_S.y)) %>% select(pid, pidp,KID_M, KID_Y, KID_S)


non_resident_bhps = non_resident_bhps %>% select(pidp, pidp,KID_M, KID_Y, KID_S)%>% distinct() %>% 
  mutate(
    KID_Y = ifelse(KID_Y<0,-1,KID_Y),
    KID_S = ifelse(KID_S<0,-1,KID_S),
  )

# - Non-resident children data from bhps and ukhls are combined into one file on 
# non-resident children (appended, bind - stacking them vertically or side by side) 

non_resident = bind_rows(non_resident_a,non_resident_f) %>% distinct()


# 5. Combining family matrix file on children with non-resident children 
parents_family_matrix_non_resid_ukhks_bhps = 
  bind_rows(parents_family_matrix, non_resident, non_resident_bhps) %>%
  group_by(pidp) %>% arrange(KID_Y,  .by_group = T) %>% 
  mutate(parity=1:n(),
         KID_S = as.numeric(KID_S),     
         KID_M = as.numeric(KID_M),     
         KID_Y = as.numeric(KID_Y)) %>% ungroup() %>% 
  filter(!is.na(KID_Y))


# 6. Combine family matrix and non-resident with Pronzato file
# •	The parents_family_matrix_non_resid_ukhks_bhps dataset is merged with the Pronzato dataset.
# •	The merge is performed by matching rows based on the pidp and KID_Y columns.
# •	all = T ensures that all rows from both data sets are kept in the result, even if they don’t have a match (full outer join).

parents_family_matrix_non_resid_ukhks_bhps_Pronzato = parents_family_matrix_non_resid_ukhks_bhps 

parents_family_matrix_non_resid_ukhks_bhps_Pronzato =  parents_family_matrix_non_resid_ukhks_bhps_Pronzato  %>%
  filter(!is.na(KID_Y)) %>%
  group_by(pidp) %>% arrange(KID_Y,  .by_group = T) %>%
  mutate(parity=1:n(),
         KID_S = as.numeric(KID_S),
         KID_M = as.numeric(KID_M),
         KID_Y = as.numeric(KID_Y)) %>% ungroup()



variables <- c("KID_S", "KID_M")

# 6. Reshape to Wide Format:
# - Transforms the combined data on children and parents back to wide format, creating columns for each child’s birth year, month, and gender by birth order.
# - Additional columns are created to accommodate multiple children, with placeholders for up to 16 children.

data_long = parents_family_matrix_non_resid_ukhks_bhps_Pronzato
# data_long = parents_family_matrix_non_resid_ukhks_bhps

wide_fomrat<-reshape2::dcast(data_long , pidp ~ parity, value.var="KID_Y")

colnames(wide_fomrat)[-1] <- paste0("KID_Y", colnames(wide_fomrat)[-1])

for (var in variables) {

  x<-reshape2::dcast(data_long , pidp ~ parity, value.var=var)

  colnames(x)[-1] <- paste0(var, colnames(x)[-1])

  wide_fomrat = merge(wide_fomrat, x, by = c("pidp"))
}

# 8. Add childless respondents
x = unique(data_long$pidp)

# master_uk = haven::read_dta(paste0(folder_uk_fertility_1,"xhhrel_protect.dta"))
# childless<-master_uk[master_uk$pidp %notin% x, ]

master_uk = haven::read_dta(paste0(folder_uk_fertility_1,"xhhrel_protect.dta"), 
                col_select = c(
                  "pidp",
                  "bcx_N",                   # total no. of biological children	xhhrel
                  starts_with("bcx_pidp_"))) # biological child 1-16 pidp


childless<-master_uk[master_uk$pidp %notin% x, ]

uk_fertility_biography = bind_rows(wide_fomrat,  
                                   data.frame(pidp = unique(childless$pidp)))

# 8. Add Dummy Variables and Merge Additional Child Information:
# create dummies
max_kids = length(select(uk_fertility_biography, starts_with("KID_Y")) %>% names())

uk_fertility_biography_1<-uk_fertility_biography %>% 
  mutate(across(.cols = paste0("KID_Y",1:max_kids ),
                .fns = ~ifelse(is.na(.), 0, 1),
                .names = "KID_{col}") ) %>% 
  rename_with(
    ~ gsub("KID_KID_Y", "KID_", .x, fixed = TRUE),
    starts_with("KID_KID_Y"))


uk_fertility_biography_1$nr_kids<-rowSums(uk_fertility_biography_1[,paste0("KID_",1:16)])

uk_fertility_biography_1<-merge(uk_fertility_biography_1, nr_kids_bcx, by = c("pidp"), all.x = T)

uk_fertility_biography_1 = uk_fertility_biography_1 %>% select(-ends_with("x")) %>% select(-ends_with("y"))

# uk_fertility_biography_1 = merge(uk_fertility_biography_1, flag_Pronzato, by = c("pidp"), all.x = T)

kids_order_var<-c()
for (i in 1:max_kids) {
  kids_order_var<-rbind(kids_order_var,
                        paste0("KID_Y",i),paste0("KID_M",i),paste0("KID_S",i),paste0("KID_",i))
}

kids_order_var = as.vector(kids_order_var)

uk_fertility_biography_1 = uk_fertility_biography_1 %>% 
  select("pidp","nr_kids",
         # "flag", 
         kids_order_var)

# Save results
saveRDS(uk_fertility_biography_1, "output/bhps_ukhls_fertility.rds")


x = ls()

x=x[x %notin% c("country_set", "biography_set", "type_you_want", "to_download","script",
                "%notin%", "countries", "folder_Australia_1","folder_Australia_2","folder_bhps_1",
                "folder_family_files","folder_fertility", "folder_main_uk", "folder_part_uk",
                "folder_partnership","folder_personal", "folder_retro_shp_1", "folder_retro_shp_2",
                "folder_shp_1","folder_shp_2","folder_soep","folder_uk_fertility_1"
)]

rm(list = x)














