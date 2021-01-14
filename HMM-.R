data1 <- read.csv("ALSFRS_R.csv")
install.packages("depmixS4")
library("depmixS4")
?depmixS4
# mod <- depmix(EventTime ~ 1, data = data[,3:15], nstates = 6, family
#               =multinomial("identity"), transition = ~ Count, instart = runif(2))
# fm <- fit(mod, emcontrol=em.control(classification="soft", maxit = 60))
# modNew <- depmix(EventTime~1,data=data2,transition=~Count,nstates=2,
#                  family=multinomial("identity"))
# modNew <- setpars(modNew,getpars(fm))
# probs2 <- viterbi(modNew)
# sum(forwardbackward(setpars(depmix(list(var~1), data=newData, nstates=3,family=list(gaussian())), getpars(originalModel)))[["alpha"]][nrow(data),])
# modNew <- depmix(EventTime~1,data=data2,transition=~Count,nstates=2,
#                  family=multinomial("identity"))
# modNew <- setpars(modNew,getpars(fm))
# modNew <- fit(modNew)
# predStates <- posterior(modNew)
# predStates$state
variable.names(data)
set.seed(1)
mod <- mix(list(Q1_Speech ~ 1, Q2_Salivation ~ 1, Q3_Swallowing ~ 1, Q4_Handwriting ~ 1,Q5_Cutting~1,Q6_Dressing_and_Hygiene~1,Q7_Turning_in_Bed~1,Q8_Walking~1,Q9_Climbing_Stairs~1,R1_Dyspnea~1,R2_Orthopnea~1,R3_Respiratory_Insufficiency~1), data = data1,nstates = 6, family = list(multinomial("identity"),multinomial("identity"), multinomial("identity"),multinomial("identity"),multinomial("identity"),multinomial("identity"),multinomial("identity"),multinomial("identity"),multinomial("identity"),multinomial("identity"),multinomial("identity"),multinomial("identity")), respstart = runif(522), prior = ~ SubjectID,initdata = data1)
fm <- fit(mod, verbose = FALSE, emc=em.control(rand=FALSE))
fm
summary(fm,which='prior')
mod <- depmix(list(Q1_Speech ~ 1, Q2_Salivation ~ 1, Q3_Swallowing ~ 1, Q4_Handwriting ~ 1, Q5_Cutting~1, Q6_Dressing_and_Hygiene~1, Q7_Turning_in_Bed~1, Q8_Walking~1, Q9_Climbing_Stairs~1, R1_Dyspnea~1, R2_Orthopnea~1, R3_Respiratory_Insufficiency~1), data = data1,nstates = 6, family = list(multinomial("identity"), multinomial("identity"), multinomial("identity"), multinomial("identity"), multinomial("identity"), multinomial("identity"), multinomial("identity"), multinomial("identity"), multinomial("identity"), multinomial("identity"), multinomial("identity"), multinomial("identity")), transition = 6, instart = runif(522))
fm <- fit(mod, emcontrol=em.control(classification="soft", maxit = 500, tol=1e-08, crit="relative", random.start = T))
# modNew <- depmix(list(Q1_Speech ~ 1, Q2_Salivation ~ 1, Q3_Swallowing ~ 1, Q4_Handwriting ~ 1,Q5_Cutting~1,Q6_Dressing_and_Hygiene~1,Q7_Turning_in_Bed~1,Q8_Walking~1,Q9_Climbing_Stairs~1,R1_Dyspnea~1,R2_Orthopnea~1,R3_Respiratory_Insufficiency~1), data = data1,nstates = 6, family = list(multinomial("identity"),multinomial("identity"), multinomial("identity"),multinomial("identity"),multinomial("identity"),multinomial("identity"),multinomial("identity"),multinomial("identity"),multinomial("identity"),multinomial("identity"),multinomial("identity"),multinomial("identity")))
# modNew <- setpars(modNew,getpars(fm))
# modNew <- fit(modNew)
# modNew <- depmix(list(Q1_Speech ~ 1, Q2_Salivation ~ 1, Q3_Swallowing ~ 1, Q4_Handwriting ~ 1,Q5_Cutting~1,Q6_Dressing_and_Hygiene~1,Q7_Turning_in_Bed~1,Q8_Walking~1,Q9_Climbing_Stairs~1,R1_Dyspnea~1,R2_Orthopnea~1,R3_Respiratory_Insufficiency~1),data=data1,transition=6,nstates=6,family=multinomial("identity"))
probs2 <- viterbi(mod)
sum(forwardbackward(setpars(depmix(list(Q1_Speech ~ 1, Q2_Salivation ~ 1, Q3_Swallowing ~ 1, Q4_Handwriting ~ 1,Q5_Cutting~1,Q6_Dressing_and_Hygiene~1,Q7_Turning_in_Bed~1,Q8_Walking~1,Q9_Climbing_Stairs~1,R1_Dyspnea~1,R2_Orthopnea~1,R3_Respiratory_Insufficiency~1), data=data1, nstates=7, family=list(gaussian())), getpars(mod)))[["alpha"]][nrow(data1),])
mod <- setpars(mod,getpars(fm))
predStates <- posterior(mod)
predStates$state
summary(fm,which='prior')
write.csv(predStates,"prediction_HMM.csv")
mod$prior$dens
save.image(file="HMM_2021.1.15.RData")