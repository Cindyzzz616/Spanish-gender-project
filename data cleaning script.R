#packages and setup
rm(list=ls())
#devtools::install_github("jwdink/eyetrackingR")
library("eyetrackingR")
library(tidyverse)
library(readxl)



# reading timings data
timeA = read.csv(file = 'data/TimingsData_A.csv', header = TRUE)
timeB = read.csv(file = 'data/TimingsData_B.csv', header = TRUE)
# choosing only columns 1-5 (TOI, DetOnset, NounOnset, EndingOnset, EndingOffset)
timeA = timeA %>% rename(TOI = TrialCode)
timeB = timeB %>% rename(TOI = TrialCode)





# reading accuracy data
# resp_acc = read_xlsx(path = 'data-2025-03-12/Eyetracking_Accuracy_Adults.xlsx') %>% 
#   rename(TOI = Trial) %>% 
#   filter(Participant != "TA127")



# reading position data
position = read.csv(file = 'data/Trial_variables.csv', header = TRUE) %>% 
  select(Trial, Position.of.Target.1.top.left.2.top.right.3.bottom) %>% 
  rename(TOI = Trial, 
         Position = Position.of.Target.1.top.left.2.top.right.3.bottom) %>% 
  mutate(Position = ifelse(Position == 1, "top left", 
                           ifelse(Position == 2, "top right", "bottom centre")))
  


# reading participant demographic info
demo = read_xlsx(path = 'data-2025-03-12/Summary_Adult-April2025_NewSounds.xlsx')
demo = demo[, c(1, 3, 4, 6)] # selecting only participant name, sex and speaker type
colnames(demo) = c("Participant", "SpeakerType", "Sex", "Age") #renaming columns
demo$Participant = toupper(demo$Participant) #unifying names to all uppercase
demo




# reading in AOI metrics export
df = read.table(file = 'data-2025-03-12/Project-May15 Metrics (2025-03-12).tsv', sep = '\t', header = TRUE)

# fixing typos
df[df$Determiner == "Definite ", ]$Determiner = "Definite" # remove extra space at the end of "Definite 
df$Participant = toupper(df$Participant)
df[df$Participant == "T101", ]$Participant = "TA101"
df[df$Participant == "TA123A", ]$Participant = "TA123"
df[df$Participant == "TA126A", ]$Participant = "TA126"
df[df$Participant == "TA13", ]$Participant = "TA136"
df[df$Participant == "TA137A", ]$Participant = "TA137"
df = df %>% filter(Participant != "TA127") #removing TA127



# checking for discrepencies in TOI names between different files
allTOI = c(unique(timeA$TOI), unique(timeB$TOI)) #getting TOI names from timeA and timeB files
obsTOI = unique(df$TOI) #getting the unique TOI names in metrics data
x = 1 #indexing for loop

# printing TOIs from obsTOI that aren't in allTOI
for (i in 1:length(obsTOI)){
  ifelse(obsTOI[i] %in% allTOI, 
         x, print(obsTOI[i]))
}
# missing from timeA and timeB: 
#"Trial01-def-campesina_0", 
#"Trial26-pos-caballero_0", "Trial26-def-caballero_0"

# printing TOIs from allTOI that isn't in obsTOI
for (i in 1:length(allTOI)){
  ifelse(allTOI[i] %in% obsTOI, 
         x, print(allTOI[i]))
}
#missing: 
#"Trial01-def-campsina_0", "Trial01-pos-campsina_0", 
#"Trial26-pos-cabellero_0", "Trial26-def-cabellero_0"

#campsina in allTOI should be campesina
#caballero in obsTOI should be cabellero

# correting typos
timeA[timeA$TOI == "Trial01-def-campsina_0",]$TOI = "Trial01-def-campesina_0"
timeB[timeB$TOI == "Trial01-pos-campsina_0",]$TOI = "Trial01-pos-campesina_0"
timeB[timeB$TOI == "Trial26-def-cabellero_0",]$TOI = "Trial26-def-caballero_0"
timeA[timeA$TOI == "Trial26-pos-cabellero_0",]$TOI = "Trial26-pos-caballero_0"
# resp_acc[resp_acc$TOI == "Trial01-def-campsina_0",]$TOI = "Trial01-def-campesina_0"
# resp_acc[resp_acc$TOI == "Trial01-pos-campsina_0",]$TOI = "Trial01-pos-campesina_0"
# resp_acc[resp_acc$TOI == "Trial26-def-cabellero_0",]$TOI = "Trial26-def-caballero_0"
# resp_acc[resp_acc$TOI == "Trial26-pos-cabellero_0",]$TOI = "Trial26-pos-caballero_0"
position[position$TOI == "Trial01-def-campsina_0",]$TOI = "Trial01-def-campesina_0"
position[position$TOI == "Trial01-pos-campsina_0",]$TOI = "Trial01-pos-campesina_0"
position[position$TOI == "Trial26-def-cabellero_0",]$TOI = "Trial26-def-caballero_0"
position[position$TOI == "Trial26-pos-cabellero_0",]$TOI = "Trial26-pos-caballero_0"


# generate a subset of metrics data and pivoting to wide format
subdf = df %>% select(Participant, Timeline, TOI, Bin, Bin_duration, AOI, Fixation_hit) %>% 
  pivot_wider(names_from = AOI, values_from = Fixation_hit)





#adding timestamp variable
TimeMS = numeric(dim(subdf)[1])
current_time = 0
for (i in 1:length(TimeMS)){
  current_time = ifelse(subdf$Bin[i] != 1, 
                        current_time, 
                        0)
  current_time = current_time+subdf$Bin_duration[i]
  TimeMS[i] = current_time
}
subdf = cbind(subdf, TimeMS)

# # sanity check to determiner gaps in measurement
# # create a vector of difference between bins
# bin_diff = numeric(dim(subdf)[1])
# for (i in 1:(length(bin_diff)-1) ){
#   bin_diff[i] = subdf$Bin[i+1]-subdf$Bin[i]
# }
# subdf = cbind(subdf, bin_diff)
# 
# # add row indices
# numrow = c(1:dim(subdf)[1])
# subdf = cbind(subdf, numrow)
# 
# # do we find bin_diffs > 1?
# subdf %>% filter(bin_diff != 1) %>%
#   select(numrow, Participant, TOI, Bin, bin_diff) %>%
#   pull(bin_diff) %>% summary
# 
# # identify specific rows to look at
# subdf %>% filter(numrow %in% c(2531:2534)) %>%
#   select(Participant, TOI, Bin, TimeMS, bin_diff)







# Adding Animacy
df_animacy = df %>% group_by(TOI, AOI, Animacy) %>% 
  summarize(n = n()) %>% select(-n) %>% 
  pivot_wider(names_from = AOI, values_from = Animacy) %>% 
  rename(Animacy_T = Target, Animacy_C = Competitor, Animacy_D = Distractor)
subdf = merge(subdf, df_animacy)

# adding Determiner
df_det = df %>% group_by(TOI, Determiner) %>% 
  summarize(n = n()) %>% select(-n) 
subdf = merge(subdf, df_det)

# adding N_ending
df_N_end = df %>% group_by(TOI, AOI, N_ending) %>% 
  summarize(n = n()) %>% select(-n) %>% 
  pivot_wider(names_from = AOI, values_from = N_ending) %>% 
  rename(N_ending_T = Target, N_ending_C = Competitor, N_ending_D = Distractor)
subdf = merge(subdf, df_N_end)

# adding N_Gender
df_N_Gender = df %>% group_by(TOI, AOI, N_Gender) %>% 
  summarize(n = n()) %>% select(-n) %>% 
  pivot_wider(names_from = AOI, values_from = N_Gender) %>% 
  rename(N_Gender_T = Target, N_Gender_C = Competitor, N_Gender_D = Distractor)
subdf = merge(subdf, df_N_Gender)

# adding Phonol_Overlap
df_Phonol_Overlap = df %>% group_by(TOI, AOI, Phonol_Overlap) %>% 
  summarize(n = n()) %>% select(-n)  %>% 
  pivot_wider(names_from = AOI, values_from = Phonol_Overlap) %>%
  rename(Phonol_Overlap_T = Target, Phonol_Overlap_C = Competitor, Phonol_Overlap_D = Distractor)
subdf = merge(subdf, df_Phonol_Overlap)

# merge metrics data with timings and demo data
df_onsets = rbind(timeA[, c(1:6)], timeB[, c(1:6)])
subdf = merge(subdf, df_onsets)
subdf = merge(subdf, demo)
#subdf = merge(subdf, resp_acc)
subdf = merge(subdf, position)


# applying make_eyetracking_data function
subdf = subdf %>% mutate(Trackloss = 0) #assumes no trackloss
subdf2 <- make_eyetrackingr_data(subdf, 
                                 participant_column = "Participant",
                                 trial_column = "TOI",
                                 time_column = "TimeMS",
                                 trackloss_column = "Trackloss",
                                 aoi_columns = c('Target','Competitor', 'Distractor'),
                                 treat_non_aoi_looks_as_missing = TRUE)

subdf2 %>% pull(Trackloss) %>% summary
# If you want Katrina's graphs to work, you need to convert the three
#fixation hit columns (i.e., Target, Competitor, Distractor) into numbers
# instead of boolean
subdf2$Competitor = as.integer(subdf2$Competitor)
subdf2$Target = as.integer(subdf2$Target)
subdf2$Distractor = as.integer(subdf2$Distractor)


# filtering participants and rows without fixations
# remove rows without fixation towards any AOI and inaccurate responses
subdf2 = subdf2 %>% filter(Trackloss == FALSE)
# add adjusted timings
subdf2 = subdf2 %>% mutate(TimeMS_adjusted = floor((TimeMS - ArticleOnset)/10)*10, 
                           ArticleOnset_adjusted = 0,
                           NounOnset_adjusted = (NounOnset - ArticleOnset), 
                           EndingOnset_adjusted = (EndingOnset - ArticleOnset), 
                           EndingOffset_adjusted = (EndingOffset - ArticleOnset),
                           Remaining_adjusted = (Remaining - ArticleOnset),
                           GenderMatchTarget_D = ifelse(N_Gender_T == N_Gender_D, 1, 0))
#truly zero the boundary use: floor(x/10)*10
# checking to make sure the zeroing worked properly
subdf2 %>% filter(Participant == "TA101", TimeMS_adjusted == 0) %>% 
  select(TimeMS, TimeMS_adjusted, ArticleOnset)


write.csv(subdf2, file = "data-2025-03-12/df_cleaned.csv")

# combining timings data and exploring as combined file
timeA = timeA %>% mutate(Timeline = "VersionA")
timeB = timeB %>% mutate(Timeline = "VersionB")
timeAll = rbind(timeA, timeB)
timeAll = timeAll %>% 
  mutate(ArticleOnset_adjusted = 0,
         NounOnset_adjusted = (NounOnset - ArticleOnset), 
         EndingOnset_adjusted = (EndingOnset - ArticleOnset), 
         EndingOffset_adjusted = (EndingOffset - ArticleOnset), 
         Remaining_adjusted = (Remaining - ArticleOnset))
write.csv(timeAll, file = "data/TimingsData_All.csv")

