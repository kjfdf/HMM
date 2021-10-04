# exploratory data analysis of ALS clinical stages 

library(tidyverse)
library(broom)
library(gridExtra)

# Set working directory 
setwd("/Users/hong/Dropbox/ALSmaster/PROACT")

# Read ALSFRS stage dataset 
stage_alsfrs = read.csv("ALSFRS_rev_clinicalStage.csv")
stage_als = read.csv("ALS_rev_clinicalStage.csv")

length(unique(stage_alsfrs$SubjectID)) # 3059 patients 
length(unique(stage_als$SubjectID)) # 3059 patients 

# Time from enrollment: King's and MiTos stages
p1 = ggplot(stage_als, aes(factor(king), feature_delta)) + 
  geom_boxplot() + 
  labs(x = "King's stage", y = "Time (mo. from enrollment)") 
  
p2 = ggplot(stage_als, aes(factor(mitos), feature_delta)) + 
  geom_boxplot() + 
  labs(x = "MiToS stage", y = "Time (mo. from enrollment") 

grid.arrange(p1,p2, nrow = 1)

# ALSFRS total scores: King's and MiTos stages 
p3 = ggplot(stage_alsfrs, aes(factor(king), ALSFRS_R_Total)) + 
  geom_boxplot() + 
  labs(x = "King's stage", y = "ALSFRS_R_Total")

p4 = ggplot(stage_alsfrs, aes(factor(mitos), ALSFRS_R_Total)) + 
  geom_boxplot() + 
  labs(x = "MiTos stage", y = "ALSFRS_R_Total")

grid.arrange(p3, p4, nrow = 1)

# Distribution of ALSFRS total score at enrollment 
# At enrollment means the first visit within 3 months after enrollment 
first = stage_alsfrs %>%
  group_by(SubjectID) %>%
  arrange(feature_delta) %>%
  mutate(visit = rank(feature_delta)) %>%
  filter(visit == 1 & feature_delta <= 3)
dim(first) # 3058 patients 

first %>% 
  ggplot(aes(ALSFRS_R_Total)) + 
  geom_histogram(bins = max(first$ALSFRS_R_Total) - 
                   min(first$ALSFRS_R_Total), 
                 col = 'white') + 
  labs(title = "Distriubtion of ALSFRS_R_Total score at enrollment")

# Distribution of King's and MiToS stages at enrollment 
# first visit within 3 months after enrollment 
p5 = ggplot(first, aes(king)) + 
  geom_bar() + 
  scale_y_continuous(limits = c(0,2500)) + 
  labs(x = "King's stage", 
       y = "Number of patients",
       title = "At enrollment") 
p6 = ggplot(first, aes(mitos)) + 
  geom_bar() + 
  labs(x = "MiTos stage", 
       y = "",
       title = "")
grid.arrange(p5, p6, nrow = 1) 

# trajectory of ALSFRS total score 
# rate of progression; 
# rapid, intermediate (IQR), slow 
# linear fit to estimate ALSFRS total score slope (/mo)

# exclude patients with only one ALSFRS record
temp = stage_alsfrs %>%
  count(SubjectID) %>%
  filter(n > 1)
temp2 = stage_alsfrs %>%
  filter(SubjectID %in% temp$SubjectID)

slope_alsfrs = temp2 %>%
  group_by(SubjectID) %>%
  nest() %>%
  mutate(model = map(data, ~lm(ALSFRS_R_Total ~ feature_delta, 
                               data = .x))) %>%
  mutate(coef = map(model, ~tidy(.x))) %>%
  unnest(coef) %>%
  filter(term == "feature_delta") %>%
  select(SubjectID, estimate)
  
# distribution of the slope estimates  
ggplot(slope_alsfrs, aes(estimate)) + 
  geom_density()
# categorize the patients into subgroups 
# according to the rate of decline (slope estimates)
q4 = quantile(slope_alsfrs$estimate, probs = c(0,0.25,0.75,1))
slope_alsfrs$slope_gr = cut(slope_alsfrs$estimate, 
                         breaks = q4, 
                         include.lowest = T, 
                         right = F)

# trajectory of ALSFRS total scores 
fu_alsfrs = temp2 %>%
  left_join(slope_alsfrs, by = "SubjectID")

fu_alsfrs %>%
  ggplot(aes(feature_delta, ALSFRS_R_Total, 
             col = slope_gr, group = factor(SubjectID))) + 
  geom_line(alpha = 0.2) + 
  facet_wrap(~slope_gr, ncol = 1) + 
  scale_color_discrete(name = "ALSFRS_R_Total slope (/month)") + 
  labs(x = "Time (mo. from enrollment)") + 
  theme(legend.position = "none")

# Trajectory of King's and MiTos stages 
# waterfall plot 
fu_als = slope_alsfrs %>%
  left_join(stage_als, by = "SubjectID")

# King's stage 
p7 = fu_als %>%
  ggplot(aes(feature_delta, king, 
             col = slope_gr, group = factor(SubjectID))) + 
  geom_line(alpha = 0.2) + 
  facet_wrap(~slope_gr, ncol = 1) + 
  scale_color_discrete(name = "ALSFRS_R_Total slope (/month)") + 
  labs(x = "Time (mo. from enrollment)") + 
  theme(legend.position = "none") + 
  scale_y_reverse() + 
  labs(y = "King's stage")

# Mitos stage 
p8 = fu_als %>%
  ggplot(aes(feature_delta, mitos, 
             col = slope_gr, group = factor(SubjectID))) + 
  geom_line(alpha = 0.2) + 
  facet_wrap(~slope_gr, ncol = 1) + 
  scale_color_discrete(name = "ALSFRS_R_Total slope (/month)") + 
  labs(x = "Time (mo. from enrollment)") + 
  theme(legend.position = "none") + 
  scale_y_reverse() + 
  labs(y = "MiTos stage")

grid.arrange(p7, p8, nrow = 1)



