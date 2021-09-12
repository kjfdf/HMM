# install.packages("gmodels")
# ALSFRS-R을 기본으로 해서 B(Bulbar), M(motor), R(respiration)으로 분류. B,L,M은 각각 0(normal or mild),1(moderately and moderately severe),2(severe)로 분류
# inclusion, exclusion은 기존 2012 King's college논문과 2015년 MiToS논문에 기초해서 설정. **추가필요**
# ALSFRS 1~3은 0,1은 0으로, 2,3은 1로, 4는 2로 collapse한뒤 B에 반영, 4~9는 0,1은 0으로 2,3은 1로, 4는 2로 collapse하여 motor에 반영, 10과 11은 0,1은 0, 2,3은 1로, 4는 2로 12는 0은 0, 1~3은 1로 4는 2로 collapse
# B 0~2를 어떤 기준으로 정할지?: 1,2,3이 모두 1이하는 0
# M 0~2를 어떤 기준으로 정할지?:
# R 0~2를 어떤 기준으로 정할지?:
# BMR을 1개의 stage로 어떻게 collapse할지?(like TNM staging in cancer): NIV, gastrostomy가 필요한 환자는 stage 4로. B2M2R2는 stage 4
# stage 0는 B0M0R0, stage 1는 BMR 1이 1개~3개, stage 2는 2가 1개, stage 3는 2개? 
# ** 해결이 필요한 문제사항: ALSFRS-R 중 noise로 작용하는 drooling의 처리와 환경에 따라 점수가 바뀔 수 있는 계단오르기를 어떻게 처리할지?, panel data인 PROACT, 서울대병원 데이터의 단점 
# PROACT data를 가지고 먼저 분석해서 stage나누고 stage내의 homogeneitry, stage간의 discriminatory ability비교, standardized median time을 비교해서 고르게 분포하는지 확인, Survival curve확인. 
# PROACT data에 기반한 King's stage와 MiToS와 비교해서 더 우수한지 확인.
# 마지막으로 서울대병원, 보라매병원 데이터를 가지고 validation. 
# 논문의 장점은 기존 stage시스템이 반영하지 못한 한 region에서의 progression을 반영하고 ALSFRS-R의 multidimensionality를 반영하고 Rasch analysis에 의해 확인된 문항의 평가항목 간소화를 통해 더 적합한 평가가능.
# 임상시험의 endpoint로서 사용되고 환자 예후평가에도 더 적합할것임. 
library(readxl)
library(ggplot2)
library(survival)
library(survminer)
library(extrafont)
library(dplyr)
library(tidyr)
library(survival)
library(writexl)
library(lubridate)
library(gmodels)
pro <- read.csv("PROACT.csv")
sur <- read.csv("PROACT_Survival_all.csv")
write.csv(all_pro_gast,"PROACT_BMRstaging_new.csv")
gastrostomy <- read.csv("ALSFRS_revised.csv")
gastrostomy_ <- gastrostomy %>% select(SubjectID,feature_delta,Q5a_Cutting_without_Gastrostomy,Q5b_Cutting_with_Gastrostomy)
all_pro_gast <- all_pro %>% right_join(gastrostomy_,by=c("SubjectID","feature_delta"))
write.csv(all_pro_gast,"PROACT_BMRstaging_new.csv")
names(pro)
names(sur)
pro %>% summarise(n_distinct(SubjectID)) #3408 subjects
dim(pro) # 30156*15
sur %>% summarise(n_distinct(SubjectID)) #9080 subjects
str(all_pro)
dim(all_pro)
all_pro$status <- as.factor(all_pro$status)
#King staging
all_pro_gast_all %>% filter(status==1)
str(all_pro_gast_all)
all_pro_gast_all <- all_pro_gast_all %>% mutate(bulbar=case_when(
    Q1_Speech&Q2_Salivation&Q3_Swallowing==4~0,
    Q1_Speech|Q2_Salivation|Q3_Swallowing<4~1
  )
) 
all_pro_gast_all <- all_pro_gast_all %>% mutate(upper=case_when(
  Q4_Handwriting|Q5_Cutting|Q6_Dressing_and_Hygiene==4~0,
  Q4_Handwriting|Q5_Cutting|Q6_Dressing_and_Hygiene<4~1
  )
) 
all_pro_gast_all <- all_pro_gast_all %>% mutate(lower=case_when(
    Q7_Turning_in_Bed|Q8_Walking|Q9_Climbing_Stairs==4~0,
    Q7_Turning_in_Bed|Q8_Walking|Q9_Climbing_Stairs<4~1
  )
)
all_pro_gast_all$King <- ifelse(all_pro_gast_all$R3_Respiratory_Insufficiency<3,"4b",
                                ifelse(all_pro_gast_all$Q5b_Cutting_with_Gastrostomy<=4,"4a",as.factor(all_pro_gast_all$b_c_ls)))
all_pro_gast_all_na <- all_pro_gast_all %>% filter(is.na(King)) %>% mutate(King=bulbar+upper+lower)
all_pro_gast_all_notna <- all_pro_gast_all %>% filter(!is.na(King))
class(all_pro_gast_all_notna$King)
all_pro_gast_all_na$King <- as.factor(all_pro_gast_all_na$King)
all_pro_gast_all_notna$King <- as.factor(all_pro_gast_all_notna$King)
all_pro_gast_all_notna$King
all_pro_gast_all_na$King
all_pro_gast_all$MiToS
all_pro_gast_all_na
all_pro_gast_all <- rbind(all_pro_gast_all_na,all_pro_gast_all_notna)
all_pro_gast_all %>% arrange(SubjectID,feature_delta)
table(all_pro_gast_all$King)
#MiToS staging
all_pro_gast_all$movement <- ifelse(all_pro_gast_all$Q8_Walking<=1|all_pro_gast_all$Q6_Dressing_and_Hygiene<=1,1,0)
all_pro_gast_all$swallowing <- ifelse(all_pro_gast_all$Q3_Swallowing<=1,1,0)
all_pro_gast_all$communicating <- ifelse(all_pro_gast_all$Q1_Speech<=1&all_pro_gast_all$Q4_Handwriting<=1,1,0)
all_pro_gast_all$breathing <- ifelse(all_pro_gast_all$R1_Dyspnea<=1|all_pro_gast_all$R3_Respiratory_Insufficiency<=2,1,0)
all_pro_gast_all <- all_pro_gast_all %>% mutate(MiToS=movement+swallowing+communicating+breathing)
# ALSFRS-R 1~11, 12 collapse 5 categories to 3 categories ALSFRS-R 1~11은 0,1 to 0, 2,3 to 1, 4 to 2 and ALSFRS-R 12는 0은 0, 1~3은 1, 4는 2로 
all_pro <- all_pro %>% mutate(b1=case_when(
  Q1_Speech<2~0,
  Q1_Speech<4~1,
  Q1_Speech==4~2
))
CrossTable(all_pro$Q1_Speech,all_pro$b1)
table(all_pro$Q1_Speech,all_pro$b1)
all_pro <- all_pro %>% mutate(b2=case_when(
  Q2_Salivation<2~0,
  Q2_Salivation<4~1,
  Q2_Salivation==4~2
))
all_pro <- all_pro %>% mutate(b3=case_when(
  Q3_Swallowing<2~0,
  Q3_Swallowing<4~1,
  Q3_Swallowing==4~2
))
all_pro <- all_pro %>% mutate(m1=case_when(
  Q4_Handwriting<2~0,
  Q4_Handwriting<4~1,
  Q4_Handwriting==4~2
))
all_pro <- all_pro %>% mutate(m2=case_when(
  Q5_Cutting<2~0,
  Q5_Cutting<4~1,
  Q5_Cutting==4~2
))
all_pro <- all_pro %>% mutate(m3=case_when(
  Q6_Dressing_and_Hygiene<2~0,
  Q6_Dressing_and_Hygiene<4~1,
  Q6_Dressing_and_Hygiene==4~2
))
all_pro <- all_pro %>% mutate(m4=case_when(
  Q7_Turning_in_Bed<2~0,
  Q7_Turning_in_Bed<4~1,
  Q7_Turning_in_Bed==4~2
))
all_pro <- all_pro %>% mutate(m5=case_when(
  Q8_Walking<2~0,
  Q8_Walking<4~1,
  Q8_Walking==4~2
))
all_pro <- all_pro %>% mutate(m6=case_when(
  Q9_Climbing_Stairs<2~0,
  Q9_Climbing_Stairs<4~1,
  Q9_Climbing_Stairs==4~2
))
all_pro <- all_pro %>% mutate(r1=case_when(
  R1_Dyspnea<2~0,
  R1_Dyspnea<4~1,
  R1_Dyspnea==4~2
))
all_pro <- all_pro %>% mutate(r2=case_when(
  R2_Orthopnea<2~0,
  R2_Orthopnea<4~1,
  R2_Orthopnea==4~2
))
all_pro <- all_pro %>% mutate(r3=case_when(
  R3_Respiratory_Insufficiency<1~0,
  R3_Respiratory_Insufficiency<4~1,
  R3_Respiratory_Insufficiency==4~2
))
table(all_pro$Q2_Salivation,all_pro$b2)
table(all_pro$Q3_Swallowing,all_pro$b3)
table(all_pro$Q4_Handwriting,all_pro$m1)
table(all_pro$Q5_Cutting,all_pro$m2)
table(all_pro$Q6_Dressing_and_Hygiene,all_pro$m3)
table(all_pro$Q7_Turning_in_Bed,all_pro$m4)
table(all_pro$Q8_Walking,all_pro$m5)
table(all_pro$Q9_Climbing_Stairs,all_pro$m6)
table(all_pro$R1_Dyspnea,all_pro$r1)
table(all_pro$R2_Orthopnea,all_pro$r2)
table(all_pro$R3_Respiratory_Insufficiency,all_pro$r3)
# Bular, Respiration collapse: b1-3와 r1-3의 합계값 0-2를 0, 3-4를 1로, 5-6을 2로 
# Motor collapse: m1-6의 합계를 가지고 0-4를 0으로, 5-8을 1로, 9-12를 2로 
all_pro_gast <- all_pro_gast %>% mutate(B=case_when(
  b1+b2+b3<3~0,
  b1+b2+b3<5~1,
  b1+b2+b3<7~2
))
all_pro_gast <- all_pro_gast %>% mutate(R=case_when(
  r1+r2+r3<3~0,
  r1+r2+r3<5~1,
  r1+r2+r3<7~2
))
all_pro_gast <- all_pro_gast %>% mutate(M=case_when(
  m1+m2+m3<5~0,
  m1+m2+m3<9~1,
  m1+m2+m3<13~2
))
# BRM staging은 B+M+R을 BMR stage 0부터 4까지로 collapse: B+M+R 0은 0, 1과2는 1로, 3과4는 2로, 5는 3으로 ,6은 4로하되 ALSFRS-R 12번이 2점이하인 경우(NIV적용상태)와 5b가 NA가 아닌 경우(gastrostomy받은경우)는 4로
all_pro_gast_1 <- all_pro_gast %>% filter(R3_Respiratory_Insufficiency>2&is.na(Q5b_Cutting_with_Gastrostomy)) %>% mutate(BMR_stage=case_when(
  B+M+R<1~0,
  B+M+R<3~1,
  B+M+R<5~2,
  B+M+R==5~3,
  B+M+R==6~4
))
all_pro_gast_2 <- all_pro_gast %>% filter(R3_Respiratory_Insufficiency<3|!is.na(Q5b_Cutting_with_Gastrostomy))
all_pro_gast_2$BMR_stage <- 4
all_pro_gast_all <- rbind(all_pro_gast_1,all_pro_gast_2)
all_pro_gast_all <- all_pro_gast_all %>% arrange(SubjectID,feature_delta)
all_pro_gast_all %>% select(R3_Respiratory_Insufficiency,Q5b_Cutting_with_Gastrostomy,Q5a_Cutting_without_Gastrostomy,B,M,R,BMR_stage)
# stage내의 homogeneity와 stage간 discriminatory ability비교, Spearman’s coefficient, chisquare test
# stage별로 standardized median time비교, Median number of months from onset
all_pro_gast_median_time <- all_pro_gast_all %>% filter(status==1)
all_pro_gast_median_time1 <- all_pro_gast_median_time %>% group_by(SubjectID) %>% 
  mutate(prog=BMR_stage-lag(BMR_stage,default = BMR_stage[1]))
all_pro_gast_median_time1 <- all_pro_gast_median_time1 %>% mutate(stan_med_time=(feature_delta-min(feature_delta))/time_event)
all_pro_gast_median_time3 <- all_pro_gast_median_time2 %>% group_by(SubjectID,BMR_stage) %>% arrange(feature_delta) %>% slice(1L)
  slice(-1,-nrow(.))
all_pro_gast_median_time4 %>% select(SubjectID,feature_delta,BMR_stage)
all_pro_gast_median_time4 %>% group_by(BMR_stage) %>% summarise(med_time=median(stan_med_time))
all_pro_gast_median_time5 <- all_pro_gast_median_time2 %>% group_by(SubjectID,King) %>% arrange(feature_delta) %>% slice(1L)
all_pro_gast_median_time5 %>% group_by(King) %>% summarise(med_time=median(stan_med_time))
all_pro_gast_median_time6 <- all_pro_gast_median_time2 %>% group_by(SubjectID,MiToS) %>% arrange(feature_delta) %>% slice(1L)
all_pro_gast_median_time6 %>% group_by(MiToS) %>% summarise(med_time=median(stan_med_time))
# stage별 survival curve비교 stage 0,1,2,3,4의 비교 Kaplan-Meier survival analysis and log-rank test, site of onset
# stage간 transition probability by Markov model 
all_pro_gast_all <- read.csv("BMR_staging_PROACT.csv")
write.csv(all_pro_gast_all,"BMR_staging_PROACT.csv")
save.image("20210909.RData")
load("20210909.RData")