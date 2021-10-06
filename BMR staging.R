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
library(moonBook)
# install.packages("nparcomp") #kruskal walis test에서의 사후검정
library(nparcomp)
library(tidyverse)
library(dlookr)
library(broom)
library(gridExtra)
library(naniar)
# install.packages("VIM")
library(VIM)
# install.packages("dlookr")
library(dlookr)
options("scipen"=100)
options(max.print=1000000)
pro <- read.csv("PROACT.csv")
sur <- read.csv("PROACT_Survival_all.csv")
demo <- read.csv("PROACT_preprocessed.csv")
demo1 <- demo %>% select(SubjectID,Gastrostomy,Age,Gender,Race,alsfrs_slope,diag_delta,onset_delta,onset_site,family_ALS_hist)
gastrostomy <- read.csv("ALSFRS_revised.csv")
gastrostomy_ <- gastrostomy %>% select(SubjectID,feature_delta,Q5a_Cutting_without_Gastrostomy,Q5b_Cutting_with_Gastrostomy)
all_pro_gast <- all_pro %>% full_join(gastrostomy_,by=c("SubjectID","feature_delta"))
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
# all_pro_gast_all %>% filter(status==1)
# str(all_pro_gast)
all_pro_gast_all1 <- all_pro_gast_all %>% filter(!is.na(Q5a_Cutting_without_Gastrostomy)&!is.na(Q5b_Cutting_with_Gastrostomy))
all_pro_gast_all <- all_pro_gast_all %>% 
  mutate(bulbar=ifelse(Q1_Speech+Q2_Salivation+Q3_Swallowing<12,1,0),
         upper=ifelse(Q4_Handwriting)
# all_pro_gast_notna$King
# all_pro_gast_na$King
# all_pro_gast_all_notna
all_pro_gast_all <- rbind(all_pro_gast_all_na,all_pro_gast_all_notna)
all_pro_gast_all %>% arrange(SubjectID,feature_delta)
# all_pro_gast_all %>% filter(is.na(King))
#MiToS staging
all_pro_gast_all$movement <- ifelse(all_pro_gast_all$Q8_Walking<=1|all_pro_gast_all$Q6_Dressing_and_Hygiene<=1,1,0)
all_pro_gast_all$swallowing <- ifelse(all_pro_gast_all$Q3_Swallowing<=1,1,0)
all_pro_gast_all$communicating <- ifelse(all_pro_gast_all$Q1_Speech<=1&all_pro_gast_all$Q4_Handwriting<=1,1,0)
all_pro_gast_all$breathing <- ifelse(all_pro_gast_all$R1_Dyspnea<=1|all_pro_gast_all$R3_Respiratory_Insufficiency<=2,1,0)
all_pro_gast_all <- all_pro_gast_all %>% mutate(MiToS=movement+swallowing+communicating+breathing)
all_pro_gast_all %>% group_by(MiToS) %>% tally()
# ALSFRS-R 1~11, 12 collapse 5 categories to 3 categories ALSFRS-R 1~11은 0,1 to 0, 2,3 to 1, 4 to 2 and ALSFRS-R 12는 0은 0, 1~3은 1, 4는 2로 
all_pro <- all_pro %>% mutate(b1=case_when(
  Q1_Speech<2~0,
  Q1_Speech<4~1,
  Q1_Speech==4~2
))
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
# Bular, Respiration 각각 collapse: b1-3와 r1-3의 합계값을 각각 0-2를 0, 3-4를 1로, 5-6을 2로 
# Motor collapse: m1-6의 합계를 가지고 0-4를 0으로, 5-8을 1로, 9-12를 2로 
all_pro_gast_all <- all_pro_gast_all %>% mutate(B=case_when(
  b1+b2+b3<3~0,
  b1+b2+b3<5~1,
  b1+b2+b3<7~2
))
all_pro_gast_all <- all_pro_gast_all %>% mutate(R=case_when(
  r1+r2+r3<3~0,
  r1+r2+r3<5~1,
  r1+r2+r3<7~2
))
all_pro_gast_all <- all_pro_gast_all %>% mutate(M=case_when(
  m1+m2+m3+m4+m5+m6<5~0,
  m1+m2+m3+m4+m5+m6<9~1,
  m1+m2+m3+m4+m5+m6<13~2
))
# BRM staging은 B+M+R을 BMR stage 0부터 4까지로 collapse: B+M+R 0은 4로, 1과2는 3으로, 3과4는 2로, 5는 1로 ,6은 0으로하되 ALSFRS-R 12번이 2점이하인 경우(NIV적용상태)와 5b가 NA가 아닌 경우(gastrostomy받은경우)는 4로
all_pro_gast_all_1 <- all_pro_gast_all %>% filter(R3_Respiratory_Insufficiency>2&is.na(Q5b_Cutting_with_Gastrostomy)) %>% mutate(BMR_stage=case_when(
  B+M+R==6~0,
  B+M+R==5~1,
  B+M+R<5~2,
  B+M+R<3~3,
  B+M+R<1~4
))
all_pro_gast_all_2 <- all_pro_gast_all %>% filter(R3_Respiratory_Insufficiency<=2|!is.na(Q5b_Cutting_with_Gastrostomy)) %>% mutate(BMR_stage=4)
all_pro_gast_all <- rbind(all_pro_gast_all_1,all_pro_gast_all_2)
all_pro_gast_all <- all_pro_gast_all %>% arrange(SubjectID,feature_delta)
# DurationFromOnset은 symptom onset부터 f/u시점까지의 기간, SurvivalDurationFromOnset은 onset부터 생존까지의 기간 
all_pro_gast_all <- all_pro_gast_all %>% mutate(Dx_delay=abs(diag_delta),From_onset=abs(onset_delta))
all_pro_gast_all <- all_pro_gast_all %>% mutate(DurationFromOnset=From_onset+feature_delta)
all_pro_gast_all <- all_pro_gast_all %>% mutate(SurvDurationFromOnset=From_onset+time_event)
all_pro_gast_all %>% group_by(SubjectID) %>% select(DurationFromOnset)
# stage내의 homogeneity와 stage간 discriminatory ability비교, Spearman’s coefficient, chisquare test
# stage별로 standardized median time비교, Median number of months from onset
all_pro_gast_median_time <- all_pro_gast_all %>% filter(!is.na(DurationFromOnset)&!is.na(SurvDurationFromOnset)&status==1)
all_pro_gast_median_time %>% group_by(SubjectID) %>% select(DurationFromOnset,SurvDurationFromOnset) %>% print(n=100)
all_pro_gast_median_time <- all_pro_gast_median_time %>% group_by(SubjectID,BMR_stage) %>% mutate(BMR_stan_time=max(DurationFromOnset)/SurvDurationFromOnset)
all_pro_gast_median_time %>% group_by(BMR_stage) %>% summarise(median(BMR_stan_time))
all_pro_gast_median_time %>% group_by(BMR_stage) %>% summarise(mean(BMR_stan_time))
all_pro_gast_median_time %>% group_by(BMR_stage) %>% summarise(mean(DurationFromOnset))
all_pro_gast_median_time <- all_pro_gast_median_time %>% group_by(SubjectID,King) %>% mutate(King_stan_time=max(DurationFromOnset)/SurvDurationFromOnset)
all_pro_gast_median_time %>% group_by(King) %>% summarise(median(King_stan_time))
all_pro_gast_median_time %>% group_by(King) %>% summarise(mean(King_stan_time))
all_pro_gast_median_time %>% group_by(King) %>% summarise(mean(DurationFromOnset))
all_pro_gast_median_time <- all_pro_gast_median_time %>% group_by(SubjectID,MiToS) %>% mutate(MiToS_stan_time=max(DurationFromOnset)/SurvDurationFromOnset)
all_pro_gast_median_time %>% group_by(MiToS) %>% summarise(median(MiToS_stan_time))
all_pro_gast_median_time %>% group_by(MiToS) %>% summarise(mean(MiToS_stan_time))
all_pro_gast_median_time %>% group_by(MiToS) %>% summarise(mean(DurationFromOnset))
all_pro_gast_median_time3 <- all_pro_gast_median_time1 %>% group_by(SubjectID,BMR_stage) %>% arrange(feature_delta) %>% filter(stan_med_time!=0)
all_pro_gast_median_time3 %>% group_by(BMR_stage) %>% summarise(med_stan_time=median(stan_med_time), med_time=median(feature_delta)%/%12)
# all_pro_gast_median_time3 %>% group_by(BMR_stage) %>% summarise(mean_time=mean(stan_med_time))
all_pro_gast_median_time5 <- all_pro_gast_median_time1 %>% group_by(SubjectID,King) %>% arrange(feature_delta) %>% filter(stan_med_time!=0)
all_pro_gast_median_time5 %>% group_by(King) %>% summarise(med_stan_time=median(stan_med_time),med_time=median(feature_delta)%/%12)
# all_pro_gast_median_time5 %>% group_by(King) %>% summarise(mean_time=mean(stan_med_time))
all_pro_gast_median_time6 <- all_pro_gast_median_time2 %>% group_by(SubjectID,MiToS) %>% arrange(feature_delta) %>% filter(stan_med_time!=0)
all_pro_gast_median_time6 %>% group_by(MiToS) %>% summarise(med_stan_time=median(stan_med_time), med_time=median(feature_delta)%/%12)
# stage별 survival curve비교 stage 0,1,2,3,4의 비교 Kaplan-Meier survival analysis and log-rank test, site of onset
# Cox regression model to calculate the log-likelihood in order to determine homogeneity, Cochran–Armitage test for trend to measure the discriminatory ability of each staging system.
all_pro_gast_all_forsuvvival <- all_pro_gast_all %>% group_by(SubjectID) %>% filter(feature_delta==min(feature_delta))
all_pro_gast_all_forsuvvival %>% summarise(n=n_distinct(SubjectID))
survdiff(Surv(time_event,status==1)~BMR_stage,data=all_pro_gast_all_forsuvvival) #X2=119, p<.001
ggsurvplot(survfit(Surv(time_event,status==1)~BMR_stage,data=all_pro_gast_all_forsuvvival),title="Kaplan Meier curve for BMR stage at entry")
ggsurvplot(survfit(Surv(time_event,status==1)~King,data=all_pro_gast_all_forsuvvival),title="Kaplan Meier curve for King stage at entry")
ggsurvplot(survfit(Surv(time_event,status==1)~MiToS,data=all_pro_gast_all_forsuvvival),title="Kaplan Meier curve for MiToS stage at entry")
survdiff(Surv(time_event,status==1)~King,data=all_pro_gast_all_forsuvvival) # X2=96.5, p<.001
survdiff(Surv(time_event,status==1)~MiToS,data=all_pro_gast_all_forsuvvival) # X2=32.9, p<.001
all_pro_gast_all_forsuvvival$BMR_stage=factor(all_pro_gast_all_forsuvvival$BMR_stage,levels=c("0","1","2","3","4")) 
all_pro_gast_all_forsuvvival$King=factor(all_pro_gast_all_forsuvvival$King,levels=c("0","1","2","3","4a","4b")) 
all_pro_gast_all_forsuvvival$MiToS=factor(all_pro_gast_all_forsuvvival$MiToS,levels=c("0","1","2","3","4")) 
#BMR stage의 stage간의 discriminatory ability, Cochrane-Armitage test
result <- table(all_pro_gast_all_forsuvvival$status,all_pro_gast_all_forsuvvival$BMR_stage)
round(prop.table(result)*100,2)
prop.trend.test(result[2,],colSums(result)) #x2=130.25, p<.001
plot(t(result),col=c("grey","black"),
     main="BMR stage and death",
     ylab="death",
     xlab="BMR stage")
#BMR stage간 duration 기간 차이있는지 비교, Kruskal-Wallis test
shapiro.test(all_pro_gast_all_forsuvvival$SurvDurationFromOnset[all_pro_gast_all_forsuvvival$BMR_stage==0])
shapiro.test(all_pro_gast_all_forsuvvival$SurvDurationFromOnset[all_pro_gast_all_forsuvvival$BMR_stage==1])
shapiro.test(all_pro_gast_all_forsuvvival$SurvDurationFromOnset[all_pro_gast_all_forsuvvival$BMR_stage==2])
shapiro.test(all_pro_gast_all_forsuvvival$SurvDurationFromOnset[all_pro_gast_all_forsuvvival$BMR_stage==3])
shapiro.test(all_pro_gast_all_forsuvvival$SurvDurationFromOnset[all_pro_gast_all_forsuvvival$BMR_stage==4])
kruskal.test(SurvDurationFromOnset~factor(BMR_stage),data=all_pro_gast_all_forsuvvival)
result=mctp(SurvDurationFromOnset~factor(BMR_stage),data=all_pro_gast_all_forsuvvival)
summary(result)
pairwise.wilcox.test(all_pro_gast_all_forsuvvival$SurvDurationFromOnset, all_pro_gast_all_forsuvvival$BMR_stage, p.adj="bonferroni") #0:314, 1:1116, 2:1016, 4:962
all_pro_gast_all_forsuvvival %>% group_by(BMR_stage) %>% tally()
df.summary <- df %>%
  group_by(dose) %>%
  summarise(
    sd = sd(len, na.rm = TRUE),
    len = mean(len)
  )
ggplot(
  df.summary, 
  aes(x = len, y = dose, xmin = len-sd, xmax = len+sd)
) +
  geom_point(aes(color = dose)) +
  geom_errorbarh(aes(color = dose), height=.2)+
  theme_light()
#King stage의 stage간의 discriminatory ability, Cochrane-Armitage test
result <- table(all_pro_gast_all_forsuvvival$status,all_pro_gast_all_forsuvvival$King)
round(prop.table(result)*100,2)
prop.trend.test(result[2,],colSums(result)) #x2=121.7, p<.001
plot(t(result),col=c("grey","black"),
     main="King stage and death",
     ylab="death",
     xlab="King stage")
#King stage간 duration 기간 차이있는지 비교, Kruskal-Wallis test
shapiro.test(all_pro_gast_all_forsuvvival$SurvDurationFromOnset[all_pro_gast_all_forsuvvival$King==0])
shapiro.test(all_pro_gast_all_forsuvvival$SurvDurationFromOnset[all_pro_gast_all_forsuvvival$King==1])
shapiro.test(all_pro_gast_all_forsuvvival$SurvDurationFromOnset[all_pro_gast_all_forsuvvival$King==2])
shapiro.test(all_pro_gast_all_forsuvvival$SurvDurationFromOnset[all_pro_gast_all_forsuvvival$King==3])
shapiro.test(all_pro_gast_all_forsuvvival$SurvDurationFromOnset[all_pro_gast_all_forsuvvival$King=="4a"])
shapiro.test(all_pro_gast_all_forsuvvival$SurvDurationFromOnset[all_pro_gast_all_forsuvvival$King=="4b"])
all_pro_gast_all_forsuvvival %>% group_by(King) %>% tally() #0:9, 1:336, 2:1077, 3:1024, 4a:757, 4b:205
kruskal.test(SurvDurationFromOnset~factor(King),data=all_pro_gast_all_forsuvvival)
result <- mctp(SurvDurationFromOnset~factor(King),data=all_pro_gast_all_forsuvvival)
summary(result)
pairwise.wilcox.test(all_pro_gast_all_forsuvvival$SurvDurationFromOnset, all_pro_gast_all_forsuvvival$King, p.adj="bonferroni") 
#MiToS stage의 stage간의 discriminatory ability, Cochrane-Armitage test
result <- table(all_pro_gast_all_forsuvvival$status,all_pro_gast_all_forsuvvival$MiToS)
round(prop.table(result)*100,2)
prop.trend.test(result[2,],colSums(result)) #x2=24.759, p<.001
plot(t(result),col=c("grey","black"),
     main="MiToS stage and death",
     ylab="death",
     xlab="MiToS stage")
#MiToS stage간 duration 기간 차이있는지 비교, Kruskal-Wallis test
shapiro.test(all_pro_gast_all_forsuvvival$SurvDurationFromOnset[all_pro_gast_all_forsuvvival$MiToS==0])
shapiro.test(all_pro_gast_all_forsuvvival$SurvDurationFromOnset[all_pro_gast_all_forsuvvival$MiToS==1])
shapiro.test(all_pro_gast_all_forsuvvival$SurvDurationFromOnset[all_pro_gast_all_forsuvvival$MiToS==2])
shapiro.test(all_pro_gast_all_forsuvvival$SurvDurationFromOnset[all_pro_gast_all_forsuvvival$MiToS==3])
shapiro.test(all_pro_gast_all_forsuvvival$SurvDurationFromOnset[all_pro_gast_all_forsuvvival$MiToS==4])
all_pro_gast_all_forsuvvival %>% group_by(MiToS) %>% tally() #0:2712, 1:639, 2:54, 3:3, 4:0
kruskal.test(SurvDurationFromOnset~factor(MiToS),data=all_pro_gast_all_forsuvvival)
result <- mctp(SurvDurationFromOnset~factor(MiToS),data=all_pro_gast_all_forsuvvival)
summary(result)
# stage간 transition probability by Markov model 


#########################################
#PROACT data load and other variables extract from preproccesed dataset
proact <- read.csv("ALSFRS_rev.csv")
surv <- read.csv("survival.csv")
demo <- read.csv("PROACT_preprocessed.csv")
demo1 <- demo %>% select(SubjectID,Gastrostomy,Age,Gender,Race,alsfrs_slope,diag_delta,onset_delta,onset_site,family_ALS_hist)
#King staging
proact1 <- proact %>% mutate(bulbar=ifelse(Q1_Speech+Q2_Salivation+Q3_Swallowing<12,1,0),
                             upper=ifelse(Q4_Handwriting+Q5_Cutting<8,1,0),
                             lower=ifelse(Q8_Walking<4,1,0)) %>% 
  mutate(King=bulbar+upper+lower) %>% 
  select(-c(bulbar,upper,lower))
proact1$King <- as.factor(proact1$King)
proact1_K4 <- proact1 %>% filter(Gastrostomy==T|(R1_Dyspnea==0|R3_Respiratory_Insufficiency<4))
proact1_K123 <- proact1 %>% anti_join(proact1_K4) #proact data중 King stage가 4인 것들을 제외하는 left anti join
proact1_K4 <- proact1_K4 %>% select(-King) %>% 
  mutate(King=case_when(
    R1_Dyspnea==0|R3_Respiratory_Insufficiency<4~"4b",
    Gastrostomy==T~"4a"
  ))
proact2 <- proact1_K123 %>% full_join(proact1_K4) #proact data중 King stage가 4a,4b인 subset과 1,2,3인 subset의 합집합 
proact2 %>% filter(King==0) %>% group_by(ALSFRS_R_Total) %>% tally() #King stage 0인 경우 중 ALSFRS 결과 48:59명, 47:35, 46:16, 45:5, 44:2, 43:1, 42:1
proact2_k1 <- proact2 %>% filter(ALSFRS_R_Total<48&King==0) %>% mutate(King="1") #King stage가 0인 경우 중 ALSFRS가 48미만인 경우는 stage 1로 변경 
proact2_k1 %>% group_by(King) %>% tally()
proact2_kother <- proact2 %>% filter(ALSFRS_R_Total==48|King!=0)
proact2_kother %>% group_by(King) %>% tally()
proact3 <- proact2_kother %>% full_join(proact2_k1)
proact3 %>% group_by(King) %>% tally() # King stage 0;59, 1;4017, 2;6590, 3;8064, 4a;4566, 4b;4763
# MiToS staging
proact4 <- proact3 %>% mutate(movement=ifelse(Q8_Walking<=1|Q6_Dressing_and_Hygiene<=1,1,0),
                   swallowing=ifelse(Q3_Swallowing<=1,1,0),
                   communicating=ifelse(Q1_Speech<=1&Q4_Handwriting<=1,1,0),
                   breathing=ifelse(R1_Dyspnea<=1|R3_Respiratory_Insufficiency<=2,1,0)) %>% 
  mutate(Mitos=movement+swallowing+communicating+breathing) %>% 
  select(-c(movement,swallowing,communicating,breathing))
proact4 %>% group_by(Mitos) %>% tally() # MiToS stage 0;14993, 1;8720, 2;2828, 3;922, 4;596
proact4[,c("King","Mitos")] <- lapply(proact4[,c("King","Mitos")],as.factor)

#dead patient information 
dim(surv) #9080 rows
dead <- surv %>% filter(status==1)
dim(dead) #3075 *3
dead %>% distinct(SubjectID) %>% tally() #3075 subjects
dead1 <- dead %>% filter(SubjectID %in% unique(proact4$SubjectID))
dead1 %>% distinct(SubjectID) %>% tally() #690 subjects (ALSFRS가 기록된 환자들 중 사망한 환자의 총 수 690명)
dead2 <- dead1 %>% mutate(King=5, Mitos=5, BMR=5)
dead2 %>% group_by(King,Mitos) %>% tally()
dead3 <- dead2 %>% mutate(feature_delta=time_event) %>% 
  select(-time_event)
names(dead3)
str(dead3)
dead3[,c("King","Mitos")] <- lapply(dead3[,c("King","Mitos")],as.factor)
 
#ALSFRS 데이터셋과 사망환자 데이터셋 merge
proact5 <- proact4 %>% full_join(dead3)
proact5 %>% group_by(King,Mitos,BMR) %>% tally() %>% print(n=30)
dim(proact5) #28749 rows
proact5 %>% distinct(SubjectID) %>% tally() #3059 subjects

#BMR staging Q1~9와 R1,2는 0,1을 0으로, 2,3을 1로, 4를 2로, R3는 0을 0으로, 1~3을 1로 4를 2로 collapse based on Rasch analysis, 
#Q1~Q3는 B로, R1~3은 R로 collapse하되 0,1는 0, 2,3,4는 1로 5,6은 2로, Q4~9를 M으로 collapse 0~4는 0, 5~8은 1, 9~12는 2로 collapse 
#B,M,R을 BMR로 collapse하되 B+M+R값이 6은 0으로 5는 1로, 4는 2로, 3,2는 3으로, 1,0은 4로 collapse. King stage 4a나 4b에 해당되는 상태는 stage 4
proact6 <- proact5 %>% mutate(b1=case_when(Q1_Speech==0|Q1_Speech==1~0,Q1_Speech==2|Q1_Speech==3~1,Q1_Speech==4~2),
                              b2=case_when(Q2_Salivation==0|Q2_Salivation==1~0,Q2_Salivation==2|Q2_Salivation==3~1,Q2_Salivation==4~2),
                              b3=case_when(Q3_Swallowing==0|Q3_Swallowing==1~0,Q3_Swallowing==2|Q3_Swallowing==3~1,Q3_Swallowing==4~2),
                              m1=case_when(Q4_Handwriting==0|Q4_Handwriting==1~0,Q4_Handwriting==2|Q4_Handwriting==3~1,Q4_Handwriting==4~2),
                              m2=case_when(Q5_Cutting==0|Q5_Cutting==1~0,Q5_Cutting==2|Q5_Cutting==3~1,Q5_Cutting==4~2),
                              m3=case_when(Q6_Dressing_and_Hygiene==0|Q6_Dressing_and_Hygiene==1~0,Q6_Dressing_and_Hygiene==2|Q6_Dressing_and_Hygiene==3~1,Q6_Dressing_and_Hygiene==4~2),
                              m4=case_when(Q7_Turning_in_Bed==0|Q7_Turning_in_Bed==1~0,Q7_Turning_in_Bed==2|Q7_Turning_in_Bed==3~1,Q7_Turning_in_Bed==4~2),
                              m5=case_when(Q8_Walking==0|Q8_Walking==1~0,Q8_Walking==2|Q8_Walking==3~1,Q8_Walking==4~2),
                              m6=case_when(Q9_Climbing_Stairs==0|Q9_Climbing_Stairs==1~0,Q9_Climbing_Stairs==2|Q9_Climbing_Stairs==3~1,Q9_Climbing_Stairs==4~2),
                              r1=case_when(R1_Dyspnea==0|R1_Dyspnea==1~0,R1_Dyspnea==2|R1_Dyspnea==3~1,R1_Dyspnea==4~2),
                              r2=case_when(R2_Orthopnea==0|R2_Orthopnea==1~0,R2_Orthopnea==2|R2_Orthopnea==3~1,R2_Orthopnea==4~2),
                              r3=case_when(R3_Respiratory_Insufficiency==0~0,R3_Respiratory_Insufficiency==1|R3_Respiratory_Insufficiency==2|R3_Respiratory_Insufficiency==3~1,R3_Respiratory_Insufficiency==4~2)) %>% 
  mutate(B=case_when(b1+b2+b3<=1~0,b1+b2+b3<=3~1,b1+b2+b3<=6~2),
         M=case_when(m1+m2+m3+m4+m5+m6<=3~0,m1+m2+m3+m4+m5+m6<=7~1,m1+m2+m3+m4+m5+m6<=12~2),
         R=case_when(r1+r2+r3<=1~0,r1+r2+r3<=3~1,r1+r2+r3<=6~2)) %>% 
  mutate(BMR1=case_when(B+M+R<=1~4,
                       B+M+R<=3~3,
                       B+M+R<=4~2,
                       B+M+R<=5~1,
                       B+M+R<=6~0)) %>% 
  mutate(BMR1=case_when(R1_Dyspnea==0|R3_Respiratory_Insufficiency<4|Gastrostomy==T~4,
                        King==5~5,
                        TRUE~BMR1)) %>% 
  select(-c(b1,b2,b3,m1,m2,m3,m4,m5,m6,r1,r2,r3,B,M,R))
proact6 %>% group_by(King,Mitos,BMR1) %>% tally() %>% print(n=48)
proact6 <- proact6 %>% arrange(SubjectID,feature_delta) #proact6: King, MiToS, BMR staging 데이터셋 
proact6 %>% distinct(SubjectID) %>% tally() #3059 subjects

# time from enrollment: King, MiToS, BMR
p1=ggplot(proact6,aes(factor(King),feature_delta))+
  geom_boxplot()+
  labs(x="King's stage", y="Time from enrollment (month)")
p2=ggplot(proact6,aes(factor(Mitos),feature_delta))+
  geom_boxplot()+
  labs(x="MiToS stage", y="Time from enrollment (month)")
p3=ggplot(proact6,aes(factor(BMR1),feature_delta))+
  geom_boxplot()+
  labs(x="BMR stage", y="Time from enrollment (month)")
grid.arrange(p1,p2,p3,nrow=2, ncol=2)

# ALSFRS total scores: King, MiToS, BMR
p4 <- ggplot(proact6,aes(factor(King),ALSFRS_R_Total))+
  geom_boxplot()+
  labs(x="King's stage",y="ALSFRS-r")
p5 <- ggplot(proact6,aes(factor(Mitos),ALSFRS_R_Total))+
  geom_boxplot()+
  labs(x="MiToS stage",y="ALSFRS-r")
p6 <- ggplot(proact6,aes(factor(BMR1),ALSFRS_R_Total))+
  geom_boxplot()+
  labs(x="BMR stage",y="ALSFRS-r")
grid.arrange(p4,p5,p6,nrow=2, ncol=2)

# Distribution of ALSFRS total score at enrollment 
# At enrollment means the first visit within 3 months after enrollment 
first <- proact6 %>% group_by(SubjectID) %>% arrange(feature_delta) %>% 
  mutate(visit=rank(feature_delta)) %>% 
  filter(visit==1&feature_delta<=3)

dim(first)

first %>% ggplot(aes(ALSFRS_R_Total))+
  geom_histogram(bins=max(first$ALSFRS_R_Total)-min(first$ALSFRS_R_Total),col='white')+
  labs(title="Distribution of ALSFRS-r at enrollment")

#Distribution of King's, MiToS and BMR stage at enrollment 
p7 <- ggplot(first,aes(King))+
  geom_bar()+
  scale_y_continuous(limits=c(0,2500))+
  labs(x="King's stage",y="Number of subject")
p8 <- ggplot(first,aes(Mitos))+
  geom_bar()+
  scale_y_continuous(limits=c(0,2500))+
  labs(x="MiToS stage",y="Number of subject")
p9 <- ggplot(first,aes(BMR1))+
  geom_bar()+
  scale_y_continuous(limits=c(0,2500))+
  labs(x="BMR stage",y="Number of subject")
grid.arrange(p7,p8,p9,nrow=1)

miss_var_summary(proact6)
miss_case_summary(proact6)
gg_miss_upset(proact6)
aggr(proact6)

# exclude patients with only one ALSFRS record
temp <- proact6 %>% count(SubjectID) %>% filter(n>1)
temp2 <- proact6 %>% filter(SubjectID %in% temp$SubjectID)

slope_alsfrs <- temp2 %>% group_by(SubjectID) %>% 
  nest() %>% 
  mutate(model=map(data,~lm(ALSFRS_R_Total~feature_delta,data=.x))) %>% 
  mutate(coef=map(model,~tidy(.x))) %>% 
  unnest(coef) %>% 
  filter(term=="feature_delta") %>% 
  select(SubjectID,estimate)

#distribution of the slope estimates
ggplot(slope_alsfrs,aes(estimate))+
  geom_density()
# categorize the patients into subgroups according to the rate of decline
q4 <- quantile(slope_alsfrs$estimate,probs = c(0,0.25,0.75,1),na.rm=T)
slope_alsfrs$slope_gr <- cut(slope_alsfrs$estimate,breaks=q4,include.lowest = T,right=F)

# trajectory ALSFRS 
fu_alsfrs <- temp2 %>% left_join(slope_alsfrs,by="SubjectID")
fu_alsfrs %>% ggplot(aes(feature_delta,ALSFRS_R_Total,col=slope_gr,group=factor(SubjectID)))+
  geom_line(alpha=0.2)+
  facet_wrap(~slope_gr,ncol=1)+
  scale_x_continuous(limits=c(0,100))
  scale_color_discrete(name="ALSFRS-r slope per month")+
  labs(x="Elapsed time from enrollment(months)")+
  theme(legend.position="none")
  
# waterfall plot
fu_als <- slope_alsfrs %>% left_join(proact6,by="SubjectID")

#King's stage 
p10 <- fu_als %>% ggplot(aes(feature_delta,King,col=slope_gr,group=factor(SubjectID)))+
  geom_line(alpha=0.2)+
  facet_wrap(~slope_gr,ncol=1)+
  scale_color_discrete(name="ALSFRS-r slope")+
  labs(x="elapsed time from enrollment")+
  theme(legend.position = "none")+
  scale_y_reverse()+
  labs(y="King's stage")
  
#MiToS stage
p11 <- fu_als %>% ggplot(aes(feature_delta,Mitos,col=slope_gr,group=factor(SubjectID)))+
  geom_line(alpha=0.2)+
  facet_wrap(~slope_gr,ncol=1)+
  scale_color_discrete(name="ALSFRS-r slope")+
  labs(x="elapsed time from enrollment")+
  theme(legend.position = "none")+
  scale_y_reverse()+
  labs(y="MiToS stage")

#BMR stage
p12 <- fu_als %>% ggplot(aes(feature_delta,BMR1,col=slope_gr,group=factor(SubjectID)))+
  geom_line(alpha=0.2)+
  facet_wrap(~slope_gr,ncol=1)+
  scale_color_discrete(name="ALSFRS-r slope")+
  labs(x="elapsed time from enrollment")+
  theme(legend.position = "none")+
  scale_y_reverse()+
  labs(y="BMR stage")

grid.arrange(p10,p11,p12, nrow=1)

proact6 <- proact6 %>% group_by(SubjectID) %>% mutate(visit=rank(feature_delta,na.last = NA))
#first visit시의 data만 남기기 
proact7 <- proact6 %>% filter(visit==1)
proact99 <- proact6 %>% group_by(SubjectID) %>% mutate(BMR_first=first(BMR1))
proact99 %>% group_by(SubjectID,BMR_first) %>%tally() 
proact_death <- proact6 %>% filter(King==5)
death_data <- surv %>% select(SubjectID,time_event)
proact6 <- proact6 %>% full_join(death_data,by="SubjectID")

proact6 <- read.csv("BMR_staging_new.csv")
write.csv(proact6,"BMR_staging_new.csv",append=F,quote=F,na="NA",dec=".",row.names=F,col.names=F)
save.image("20210909.RData")
load("20210909.RData")

