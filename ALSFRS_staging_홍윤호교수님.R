# Objectives 
# Estimate clinical stage for ALS (eg, King's stages and MiTOS stages) using ALSFRS scores 

# Reference: 
# Rubika Balendra (2014) Estimating clinical stage of amyotrophic lateral sclerosis from the ALS Functional Rating Scale, Amyotrophic Lateral Sclerosis and Frontotemporal Degeneration, 15:3-4, 279-284, DOI: 10.3109/21678421.2014.897357
# Chiò A, Hammond ER, Mora G, et al. J Neurol Neurosurg Psychiatry 2015;86:38–44.

library(dplyr)

setwd("/Users/hong/Dropbox/ALSmaster/PROACT")

# Read-in ALSFRS_orig and ALSFRS_rev data
#alsfrs_orig <- read.csv("ALSFRS_orig.csv")
alsfrs_rev <- read.csv("ALSFRS_rev.csv")

#dim(alsfrs_orig) # 36,971 observations 
dim(alsfrs_rev) # 28,059 observations

#length(unique(alsfrs_orig$SubjectID)) # 4,082 patients
length(unique(alsfrs_rev$SubjectID)) # 3,059 patients

# King's staging: 
# gastrostomy를 받은 환자나 NIV를 사용하는 환자는 
# stage4로 따로 분류하고,  
# stage 4가 아닌 환자 중 bulbar, 상지, 하지 중 involve 된 개수로 
# stage 1~3 부여 

# temp1_orig = alsfrs_orig %>%
#   mutate(bulbar_involved = ifelse(bulbar < 12, 1, 0), 
#          upperlimb_involved = ifelse((Q4_Handwriting + Q5_Cutting) < 8, 1, 0), 
#          lowerlimb_involved = ifelse(Q8_Walking < 4, 1, 0)) %>%
#   mutate(king = bulbar_involved + upperlimb_involved + lowerlimb_involved) %>%
#   mutate(king = case_when((Gastrostomy == T)|(Q10_Respiratory == 1) ~ 4, 
#                           TRUE ~ king)) %>% # Q10 == 1 (vent. assistance required)
#   select(-c(bulbar_involved, upperlimb_involved, lowerlimb_involved))
# 
# temp1_orig %>%
#   count(king) %>%
#   mutate(prop = round(n/sum(n)*100, 2))
# 
# # stage = 0 in 187 observations, how?  
# # trunk involved (Q6, Q7) or 
# # leg involved in climbing stairs (Q9) but not in walking (Q8) or 
# # respiration involved (Q10) to such an extent that does not require NIV 
# 
# # replace stage 0 with stage 1 
# temp1_orig$king = ifelse(temp1_orig$king == 0, 1, temp1_orig$king)
# alsfrs_orig_king = temp1_orig
# 
# # MiTos staging
# 
# temp2_orig = alsfrs_orig %>%
#   mutate(swallowing = ifelse(Q3_Swallowing <= 1, 1, 0)) %>%
#   mutate(breathing = ifelse(Q10_Respiratory <= 2, 1, 0)) %>%
#   mutate(communicating = ifelse(Q1_Speech <= 1 & 
#                                   Q4_Handwriting <=1, 1, 0)) %>%
#   mutate(movement = ifelse(Q6_Dressing_and_Hygiene <= 1|
#                              Q8_Walking <=1, 1, 0)) %>%
#   mutate(mitos = swallowing + breathing + communicating + movement) %>%
#   select(mitos)
# 
# temp2_orig %>%
#   count(mitos) %>%
#   mutate(prop = round(n/sum(n)*100, 2))
# 
# # combine king and mitos stage 
# alsfrs_orig_stage = cbind(alsfrs_orig_king, temp2_orig)

###################

temp1_rev = alsfrs_rev %>%
  mutate(bulbar_involved = ifelse(bulbar < 12, 1, 0), 
         upperlimb_involved = ifelse((Q4_Handwriting + Q5_Cutting) < 8, 1, 0), 
         lowerlimb_involved = ifelse(Q8_Walking < 4, 1, 0)) %>%
  mutate(king = bulbar_involved + upperlimb_involved + lowerlimb_involved) %>%
  mutate(king = case_when((Gastrostomy == T)|(R1_Dyspnea == 0|R3_Respiratory_Insufficiency < 4) ~ 4, 
                          TRUE ~ king)) %>% # R1 == 0, R3 < 4; vent. assistance required 
  select(-c(bulbar_involved, upperlimb_involved, lowerlimb_involved))

temp1_rev %>%
  count(king) %>%
  mutate(prop = round(n/sum(n)*100, 2))

# stage = 0 in 119 observations, how?  
# trunk involved (Q6, Q7) or 
# leg involved in climbing stairs (Q9) but not in walking (Q8) or 
# respiration involved (Q10) to such an extent that does not require NIV 

# replace stage 0 with stage 1 
temp1_rev$king = ifelse(temp1_rev$king == 0, 1, temp1_rev$king)
alsfrs_rev_king = temp1_rev

# MiTos staging

temp2_rev = alsfrs_rev %>%
  mutate(swallowing = ifelse(Q3_Swallowing <= 1, 1, 0)) %>%
  mutate(breathing = ifelse(R1_Dyspnea <= 1|R3_Respiratory_Insufficiency <= 2, 1, 0)) %>%
  mutate(communicating = ifelse(Q1_Speech <= 1 & 
                                  Q4_Handwriting <=1, 1, 0)) %>%
  mutate(movement = ifelse(Q6_Dressing_and_Hygiene <= 1|
                             Q8_Walking <=1, 1, 0)) %>%
  mutate(mitos = swallowing + breathing + communicating + movement) %>%
  select(mitos)

temp2_rev %>%
  count(mitos) %>%
  mutate(prop = round(n/sum(n)*100, 2))

# combine king and mitos stage 
alsfrs_rev_stage = cbind(alsfrs_rev_king, temp2_rev)

# Merge with survival data 
# death = stage 5 in Mitos and King's staging system  

surv = read.csv('survival.csv')
dim(surv) # 9,080 patients 
death = surv %>%
  filter(status == 1)
dim(death) # 3,075 patients who died 

death = within(death, {
  feature_delta <- time_event
  king <- 5
  mitos <- 5
  rm(time_event, status)
})

length(unique(death$SubjectID)) # 3075 patients 

# death_orig = death %>%
#   filter(SubjectID %in% unique(alsfrs_orig_stage$SubjectID))
# length(unique(death_orig$SubjectID)) # 1012 patients 

death_rev = death %>%
  filter(SubjectID %in% unique(alsfrs_rev_stage$SubjectID))
length(unique(death_rev$SubjectID)) # 690 patients 

# combine alsfrs_orig_stage and death 
# temp3_orig = alsfrs_orig_stage %>%
#   select(SubjectID, feature_delta, mitos, king)
# temp4_orig = death_orig %>%
#   select(SubjectID, feature_delta, mitos, king)
# stage_orig_final = rbind(temp3_orig, temp4_orig)
# dim(stage_orig_final) # 37,983 observations; 36,971 ALSFRS obs + 1,012 death
# length(unique(stage_orig_final$SubjectID)) # 4082 patients

# combine alsfrs_rev_stage and death 
temp3_rev = alsfrs_rev_stage %>%
  select(SubjectID, feature_delta, mitos, king)
temp4_rev = death_rev %>%
  select(SubjectID, feature_delta, mitos, king)
stage_rev_final = rbind(temp3_rev, temp4_rev)
dim(stage_rev_final) # 28,749 observations
length(unique(stage_rev_final$SubjectID)) # 3,059 patients


#write.csv(stage_orig_final, "ALS_orig_clinicalStage.csv", quote = F, row.names = F)
#write.csv(alsfrs_orig_stage, "ALSFRS_orig_clinicalStage.csv", quote = F, row.names = F)


write.csv(stage_rev_final, "ALS_rev_clinicalStage.csv", quote = F, row.names = F)
write.csv(alsfrs_rev_stage, "ALSFRS_rev_clinicalStage.csv", quote = F, row.names = F)


######################################################







