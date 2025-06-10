#packages and setup
rm(list=ls())
options(width = 100, scipen = 999)
#devtools::install_github("jwdink/eyetrackingR")
library("eyetrackingR")
library(tidyverse)
library(readxl)
library(knitr)
library(lme4)
library(lmerTest)
library(MuMIn)
library(rstatix)
library(afex)
library(emmeans)



# reading combined timings and cleaned metrics data
#timeAll = read.csv(file = 'data/TimingsData_All.csv', header = TRUE)
df = read.csv(file = 'data-2025-03-12/df_cleaned.csv', header = TRUE) %>% 
  select(-X) %>% 
  mutate(Group = factor(ifelse(SpeakerType == 1, "ASI", "HSS"), 
                        levels = c("ASI", "HSS")), 
         Determiner = factor(Determiner, levels = c("Possessive", "Definite")), 
         Gender = factor(N_Gender_T, levels = c("Masculine", "Feminine")), 
         Animacy = factor(ifelse(Animacy_T == "Non-animate", "Inanimate", 
                                 ifelse(Animacy_T == "Animate-human", "Human", 
                                        ifelse(Animacy_T == "Animate-animal, sex", "Animal", "Epicene"))), 
                          levels = c("Inanimate", "Human", "Animal", "Epicene")), 
         Transparency = factor(ifelse(N_ending_T == "Transparent o/a", "Transparent", "Opaque"), 
                               levels = c("Transparent", "Opaque")), 
         #Version = factor(Version, levels = c("A", "B")), 
         Phonol_Overlap = factor(ifelse(Phonol_Overlap_T == "Minimal Pair", "alt", "non-alt"), 
                                 levels = c("alt", "non-alt")), 
         Morphology = factor(paste(Transparency, Phonol_Overlap, sep = "_"), 
                             levels = c("Transparent_non-alt", "Transparent_alt", "Opaque_non-alt")))
df_clean = df %>% filter(#Accuracy == 1, 
                         Participant != "TA105", 
                         TOI != "Trial01-def-campesina_0", 
                         TOI != "Trial01-pos-campesina_0") %>% 
  mutate(Item = factor(substr(x = TOI, start = 1, stop = 7)))









######################## logistic mised-effect regression
# using TimeMS
df_w1 = df_clean %>% filter(TimeMS >= ArticleOnset, 
                            TimeMS < (NounOnset+200)) %>% 
  mutate(Window = "1")
df_w2 = df_clean %>% filter(TimeMS >= (NounOnset + 200), 
                            TimeMS < (EndingOffset + 200)) %>% 
  mutate(Window = "2")
df_w3 = df_clean %>% filter(TimeMS >= (EndingOffset + 200), 
                            TimeMS < (EndingOffset + 610)) %>% 
  mutate(Window = "3")

df_allw = rbind(df_w1, df_w2, df_w3) %>% 
  select(Target, Participant, TOI, Item, Group, Determiner, Transparency, Gender, Window, 
         TimeMS, Phonol_Overlap, Morphology) %>% 
  rename(Grp = Group, Det = Determiner, Gen = Gender, Trp = Transparency, 
         Pho = Phonol_Overlap, Mor = Morphology)

# checking duration of windows across trials
df_allw %>% 
  group_by(Window, TOI, Participant) %>% 
  summarize(dur = max(TimeMS) - min(TimeMS)) %>% 
  ungroup(Participant, TOI) %>% 
  summarize(mean = mean(dur), median = median(dur), SD = var(dur)^0.5,            min = min(dur), max = max(dur))
# adding duration
df_allw = df_allw %>% 
  group_by(Window, TOI, Participant) %>% 
  summarize(dur = max(TimeMS) - min(TimeMS)) %>% 
  merge(df_allw) %>% filter(dur != 0)

contrasts(df_allw$Grp) = contr.treatment # ref = late/ASI
contrasts(df_allw$Det) = contr.treatment # ref = pos
contrasts(df_allw$Gen) = contr.treatment # ref = Masculine
contrasts(df_allw$Trp) = contr.treatment # ref = Transparent
contrasts(df_allw$Pho) = contr.treatment # ref = non-alt
contrasts(df_allw$Mor) = contr.treatment # ref = Opaque_non-alt










################3 transparent only 
# logistic regression for possessives transparents in W2
m1_postrp = glmer(Target ~ Grp*Pho + (1|Participant) + (1|Item),
                  nAGQ=1, family=binomial, 
                  data=filter(df_allw, Window == 1, Det == "Possessive", Trp == "Transparent"))
m2_postrp = glmer(Target ~ Grp*Pho + (1|Participant) + (1|Item),
                  nAGQ=1, family=binomial, 
                  data=filter(df_allw, Window == 2, Det == "Possessive", Trp == "Transparent"))
m3_postrp = glmer(Target ~ Grp*Pho + (1|Participant) + (1|Item),
                  nAGQ=1, family=binomial, 
                  data=filter(df_allw, Window == 3, Det == "Possessive", Trp == "Transparent"))
summary(m1_postrp)
summary(m2_postrp)
summary(m3_postrp)

#proportion of looks to target per window per pariticipant
df_allw %>% 
  filter(Det == "Possessive", Trp == "Transparent") %>% 
  group_by(Participant, Grp, Window, Target) %>% 
  summarize(n = n()) %>% ungroup(Target) %>% 
  mutate(prop = n/sum(n)) %>% filter(Target == 1) %>% 
  select(-Target, -n) %>% 
  spread(key = Window, value = prop) %>% 
  write.csv(file = "data-2025-03-12/prop target fixations per participant, window.csv")

#proportion of looks to target per window per pariticipant per phonological overlap
df_allw %>% 
  filter(Det == "Possessive", Trp == "Transparent") %>% 
  group_by(Participant, Grp, Window, Pho, Target) %>% 
  summarize(n = n()) %>% ungroup(Target) %>% 
  mutate(prop = n/sum(n), 
         Window = paste("W", Window, sep = ""), 
         Pho_in_Window = paste(Window, Pho, sep = "_")) %>% 
  filter(Target == 1) %>% 
  group_by(Participant, Grp, Pho_in_Window, prop) %>%  
  select(Participant, Grp, Pho_in_Window, prop) %>% 
  spread(key = Pho_in_Window, value = prop) %>% 
  select(Participant, Grp, 
         W1_alt, W2_alt, W3_alt, 
         `W1_non-alt`, `W2_non-alt`, `W3_non-alt`) %>% 
  write.csv(file = "data-2025-03-12/prop target fixations per participant, window, PhonologicalOverlap.csv")









# combine pho & trp
m1_postrp = glmer(Target ~ Grp*Mor + (1|Participant) + (1|Item),
                  nAGQ=1, family=binomial, 
                  data=filter(df_allw, Window == 1, Det == "Possessive"))
m2_postrp = glmer(Target ~ Grp*Mor + (1|Participant) + (1|Item),
                  nAGQ=1, family=binomial, 
                  data=filter(df_allw, Window == 2, Det == "Possessive"))
m3_postrp = glmer(Target ~ Grp*Mor + (1|Participant) + (1|Item),
                  nAGQ=1, family=binomial, 
                  data=filter(df_allw, Window == 3, Det == "Possessive"))
summary(m1_postrp)
summary(m2_postrp)
summary(m3_postrp)








# logistic regression for all variables, all windows
# Window 1
df_w1 = df_allw %>% filter(Window == "1") 
m1 = glmer(Target ~ Det*Grp*Mor*Gen + (1|Participant) + (1|Item),
           nAGQ=1, family=binomial, data=df_w1)
#Window 2
df_w2 = df_allw %>% filter(Window == "2") 
m2 = glmer(Target ~ Det*Grp*Mor*Gen + (1|Participant) + (1|Item),
           nAGQ=1, family=binomial, data=df_w2)
# Window 3
df_w3 = df_allw %>% filter(Window == "3") 
m3 = glmer(Target ~ Det*Grp*Mor*Gen + (1|Participant) + (1|Item),
           nAGQ=1, family=binomial, data=df_w3)

summary(m1)
summary(m2)
summary(m3)









# logistic regression for det x group in all windows
m1 = glmer(Target ~ Det*Grp + (1|Participant) + (1|Item),
           nAGQ=1, family=binomial, 
           data=filter(df_allw, Window == "1"))
m2 = glmer(Target ~ Det*Grp + (1|Participant) + (1|Item),
               nAGQ=1, family=binomial, 
           data=filter(df_allw, Window == "2"))
m3 = glmer(Target ~ Det*Grp + (1|Participant) + (1|Item),
           nAGQ=1, family=binomial, 
           data=filter(df_allw, Window == "3"))
summary(m1)
summary(m2)
summary(m3)





########### post hoc tests
# running models on data subset to HSS
m1_HSS = glmer(Target ~ Det + (1|Participant) + (1|Item),
               nAGQ=1, family=binomial, 
               data=filter(df_allw, Window == "1", Grp == "HSS"))
m2_HSS = glmer(Target ~ Det + (1|Participant) + (1|Item),
               nAGQ=1, family=binomial, 
               data=filter(df_allw, Window == "2", Grp == "HSS"))
m3_HSS = glmer(Target ~ Det + (1|Participant) + (1|Item),
               nAGQ=1, family=binomial, 
               data=filter(df_allw, Window == "3", Grp == "HSS"))

summary(m1_HSS)
summary(m2_HSS)
summary(m3_HSS)

# method 2 using code from: https://stats.stackexchange.com/questions/622339/question-about-post-hoc-analyses-for-mixed-effects-logistic-regression-model
# interaction plot
emmeans::emmip(m1, Det ~ Grp)
ggsave("plots/interaction.png", height = 2, width = 3)

emm_1 <- emmeans(m1, ~ Det | Grp)
emm_2 <- emmeans(m2, ~ Det | Grp)
emm_3 <- emmeans(m3, ~ Det | Grp)
emm_1
confint(emm) # confidence intervals
pairs(emm_1)


contrast(emm_1, "trt.vs.ctrl")
contrast(emm_2, "trt.vs.ctrl")
contrast(emm_3, "trt.vs.ctrl")

citation("emmeans")





# logistic regression for all variables subset by group level in W2
# Window 2 
df_w2 = df_allw %>% filter(Window == "2", dur <= 600, Grp == "ASI") 
m2_600_ASI = glmer(Target ~ Det*Trp*Gen + (1|Participant) + (1|TOI),
            nAGQ=1, family=binomial, data=df_w2)
df_w2 = df_allw %>% filter(Window == "2", dur <= 600, Grp == "HSS") 
m2_600_HSS = glmer(Target ~ Det*Trp*Gen + (1|Participant) + (1|TOI),
               nAGQ=1, family=binomial, data=df_w2)
df_w2 = df_allw %>% filter(Window == "2", Grp == "ASI") 
m2_680_ASI = glmer(Target ~ Det*Trp*Gen + (1|Participant) + (1|TOI),
                   nAGQ=1, family=binomial, data=df_w2)
df_w2 = df_allw %>% filter(Window == "2", Grp == "HSS") 
m2_680_HSS = glmer(Target ~ Det*Trp*Gen + (1|Participant) + (1|TOI),
                   nAGQ=1, family=binomial, data=df_w2)

summary(m2_600_ASI)
summary(m2_600_HSS)
summary(m2_680_ASI)
summary(m2_680_HSS)









############ REGRESSION WITH EMP LOGIT
(df_w1 = df_clean %>% filter(TimeMS_adjusted >= ArticleOnset_adjusted, 
                            TimeMS_adjusted < (NounOnset_adjusted+200)) %>%
  group_by(Participant, Group, TOI, Determiner, Gender, Transparency, Target) %>% 
  summarize(y = n()) %>% ungroup(Target) %>% 
  mutate(n = sum(y), EmpLog = log((y+0.5)/(n-y+0.5)), 
         Window = "1", Participant = factor(Participant)) %>% 
  filter(Target == 1))
(df_w2 = df_clean %>% filter(TimeMS_adjusted >= (NounOnset_adjusted + 200), 
                            TimeMS_adjusted < (EndingOffset_adjusted + 200)) %>% 
    group_by(Participant, Group, TOI, Determiner, Gender, Transparency, Target) %>% 
    summarize(y = n()) %>% ungroup(Target) %>% 
    mutate(n = sum(y), EmpLog = log((y+0.5)/(n-y+0.5)), 
           Window = "2", Participant = factor(Participant)) %>% 
    filter(Target == 1))
(df_w3 = df_clean %>% filter(TimeMS_adjusted >= (EndingOffset_adjusted + 200)) %>% 
    group_by(Participant, Group, TOI, Determiner, Gender, Transparency, Target) %>% 
    summarize(y = n()) %>% ungroup(Target) %>% 
    mutate(n = sum(y), EmpLog = log((y+0.5)/(n-y+0.5)), 
           Window = "3", Participant = factor(Participant)) %>% 
    filter(Target == 1))

df_allw = rbind(df_w1, df_w2, df_w3) %>% 
  select(EmpLog, Participant, TOI, Group, Determiner, Transparency, Gender, Window) %>% 
  rename(Grp = Group, Det = Determiner, Gen = Gender, Trp = Transparency)

contrasts(df_w123$Grp) = contr.treatment # ref = late/ASI
contrasts(df_w123$Det) = contr.treatment # ref = pos
contrasts(df_w123$Gen) = contr.treatment # ref = Masculine
contrasts(df_w123$Trp) = contr.treatment # ref = Transparent





# linear regression with all variables
# Window 1
df_w1 = df_allw %>% filter(Window == "1")
m1 = lmer(EmpLog ~ Det*Grp*Trp*Gen + (1|Participant) + (1|TOI), 
          data = df_w1); r.squaredGLMM(m1)
# Window 2
df_w2 = df_allw %>% filter(Window == "2")
m2 = lmer(EmpLog ~ Det*Grp*Trp*Gen + (1|Participant) + (1|TOI), 
          data = df_w2); r.squaredGLMM(m2)
# Window 3
df_w3 = df_allw %>% filter(Window == "3")
m3 = lmer(EmpLog ~ Det*Grp*Trp*Gen + (1|Participant) + (1|TOI), 
          data = df_w3); r.squaredGLMM(m3)

Anova(m1, type = "III"); summary(m1)
Anova(m2, type = "III"); summary(m2)
Anova(m3, type = "III"); summary(m3)










######################## AMOVA AND T-TESTS
# calculating empiricial logit
df_w1 = df_clean %>% filter(TimeMS >= ArticleOnset, 
                            TimeMS < (NounOnset+200)) %>% 
  group_by(Participant, Group, TOI, Determiner, Gender, Transparency, Target) %>% 
  summarize(y = n()) %>% ungroup(Target) %>% 
  mutate(prop = y/sum(y),  
         Window = "1", Participant = factor(Participant)) %>% 
  filter(Target == 1)
df_w2 = df_clean %>% filter(TimeMS >= (NounOnset + 200), 
                            TimeMS < (EndingOffset+ 200)) %>% 
  group_by(Participant, Group, TOI, Determiner, Gender, Transparency, Target) %>% 
  summarize(y = n()) %>% ungroup(Target) %>% 
  mutate(prop = y/sum(y),  
         Window = "2", Participant = factor(Participant)) %>% 
  filter(Target == 1)
df_w3 = df_clean %>% filter(TimeMS >= (EndingOffset + 200), 
                            TimeMS <= (EndingOffset + 610)) %>% 
  group_by(Participant, Group, TOI, Determiner, Gender, Transparency, Target) %>% 
  summarize(y = n()) %>% ungroup(Target) %>% 
  mutate(prop = y/sum(y),  
         Window = "3", Participant = factor(Participant)) %>% 
  filter(Target == 1)
df_allw = rbind(df_w1, df_w2, df_w3) %>% 
  merge(df_dur) %>% filter(dur != 0)

(subj_w1 = df_allw %>% filter(Window == "1") %>% 
    group_by(Participant, Determiner, Group) %>% 
    summarise(prop = mean(prop, na.rm=T)) %>% ungroup())
(subj_w1_ASI = subj_w1 %>% filter(Group=='ASI') %>% droplevels())
(subj_w1_HSS = subj_w1 %>% filter(Group=='HSS', Participant != "TA111") %>% droplevels())

wilcox_test(prop~Determiner, data=subj_w1_ASI, paired=T)
wilcox_test(prop~Determiner, data=subj_w1_HSS, paired=T)

(subj_w2 = df_allw %>% filter(Window == "2") %>% 
    group_by(Participant, Determiner, Group) %>% 
    summarise(prop = mean(prop, na.rm=T)) %>% ungroup())
(subj_w2_ASI = subj_w2 %>% filter(Group=='ASI') %>% droplevels())
(subj_w2_HSS = subj_w2 %>% filter(Group=='HSS', Participant != "TA111") %>% droplevels())

wilcox_test(prop~Determiner, data=subj_w2_ASI, paired=T)
wilcox_test(prop~Determiner, data=subj_w2_HSS, paired=T)

(subj_w3 = df_allw %>% filter(Window == "3") %>% 
    group_by(Participant, Determiner, Group) %>% 
    summarise(prop = mean(prop, na.rm=T)) %>% ungroup())
(subj_w3_ASI = subj_w2 %>% filter(Group=='ASI') %>% droplevels())
(subj_w3_HSS = subj_w2 %>% filter(Group=='HSS', Participant != "TA111") %>% droplevels())

wilcox_test(prop~Determiner, data=subj_w3_ASI, paired=T)
wilcox_test(prop~Determiner, data=subj_w3_HSS, paired=T)
