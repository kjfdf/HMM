install.packages("gmodels")
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
names(pro)
names(sur)
pro %>% summarise(n_distinct(SubjectID)) #3408 subjects
dim(pro) # 30156*15
sur %>% summarise(n_distinct(SubjectID)) #9080 subjects
all_pro <- merge(pro,sur,all = T,by="SubjectID")
str(all_pro)
dim(all_pro)
all_pro$status <- as.factor(all_pro$status)
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
# b1,b2,b3를 B로 collapse시키기 
all_pro_complete <- na.omit(all_pro)
all_pro_B <- prcomp((all_pro_b[,c(18:29)]))
summary(all_pro_B)
plot(all_pro_B,type='l')
m1 <- kmeans.result(all_pro_complete,3)
table(is.na(all_pro_complete))
# m1~6를 M으로 collapse시키는 경우의 수:
# r1~3를 R로 collapse시키는 경우의 수: 
write.csv(all_pro,"PROACT_BMRstaging.csv")
save.image("20210909.RData")
load("20210909.RData")