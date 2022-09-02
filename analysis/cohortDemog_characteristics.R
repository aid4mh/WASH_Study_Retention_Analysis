## Packages ########################################
library(magrittr)
library(scales)
library(dplyr)
library(tableone)

## Run script ######################################
# To produce a dataframe with the full cohort
# step 1: get unique ID list for each survey 
IDlist_SurveyA <- as.data.frame(unique(wash_survey_a_cleaned$`User ID`))
IDlist_SurveyB <- as.data.frame(unique(wash_survey_b_cleaned$`User ID`))
IDlist_SurveyC <- as.data.frame(unique(wash_survey_c_cleaned$`User ID`))
IDlist_SurveyD <- as.data.frame(unique(wash_survey_d_cleaned$`User ID`))
IDlist_SurveyE <- as.data.frame(unique(wash_survey_e_cleaned$`User ID`))
IDlist_SurveyF <- as.data.frame(unique(wash_survey_f_cleaned$`User ID`))
IDlist_SurveyG <- as.data.frame(unique(wash_survey_g_cleaned$`User ID`))
IDlist_SurveyEOS <- as.data.frame(unique(wash_survey_end_of_study_cleaned_duprm$`User ID`))

# step 2: rename colnames for above dataframes to User ID
colnames(IDlist_SurveyA)[1] <- "User ID"
colnames(IDlist_SurveyB)[1] <- "User ID"
colnames(IDlist_SurveyC)[1] <- "User ID"
colnames(IDlist_SurveyD)[1] <- "User ID"
colnames(IDlist_SurveyE)[1] <- "User ID"
colnames(IDlist_SurveyF)[1] <- "User ID"
colnames(IDlist_SurveyG)[1] <- "User ID"
colnames(IDlist_SurveyEOS)[1] <- "User ID"

# step 3: get unique ID from sensor metadata and rename colname
IDlist_Sensor <- as.data.frame(unique(all_metadata$`User ID`))
colnames(IDlist_Sensor)[1] <- "User ID"

# step 4: merge the survey and sensor ID lists with Baseline survey 
wash_survey_baseline_merged <- merge(merge(merge(merge(merge(merge(merge(merge(merge(
  wash_survey_baseline_cleaned_duprm, 
  IDlist_SurveyA, all = T),
  IDlist_SurveyB, all = T),
  IDlist_SurveyC, all = T),
  IDlist_SurveyD, all = T),
  IDlist_SurveyE, all = T),
  IDlist_SurveyF, all = T),
  IDlist_SurveyG, all = T),
  IDlist_SurveyEOS, all = T),
  IDlist_Sensor, all = T)

# Data manipulation 
# Age: re-arrange age into age groups
# first convert to numeric data type
wash_survey_baseline_merged$age <- as.numeric(wash_survey_baseline_merged$age)

wash_survey_baseline_merged %<>%
  mutate(Age = case_when(
    age < 19 ~ "invalid",
    age %in% c(19:29) ~ "19-29",
    age %in% c(30:39) ~ "30-39",
    age %in% c(40:49) ~ "40-49",
    age %in% c(50:59) ~ "50-59",
    age >= 60 ~ "60+",
    is.na(age) ~ "NA"))

wash_survey_baseline_merged$Age[which(wash_survey_baseline_merged$Age == "NA")] <- NA

# For manuscript result section: find median and IQR of age (without invalid and missing data)
wash_survey_baseline_merged$age_mu <- wash_survey_baseline_merged$age
# assign invalid entries to NA 
wash_survey_baseline_merged$age_mu[wash_survey_baseline_merged$age_mu < 19] <- NA
# check distribution
hist(wash_survey_baseline_merged$age_mu)
# get median and IQR
summary(wash_survey_baseline_merged$age_mu)
                                          
# Gender: set responses that are out of range to invalid
wash_survey_baseline_merged$gender[which(! (wash_survey_baseline_merged$gender 
                                            %in% c("Female", 
                                                   "Male",
                                                   "Non-binary", 
                                                   NA)))] <- "invalid"

# Race: set responses that are out of range to invalid
wash_survey_baseline_merged$race[which(! (wash_survey_baseline_merged$race 
                                          %in% c("American Indian or Alaska Native",
                                                 "Asian",
                                                 "Black or African American",
                                                 "Native Hawaiian or Other Pacific Islander",
                                                 "White",
                                                 "Other",
                                                 NA)))] <- "invalid"

# Ethnicity: set responses that are out of range to invalid
wash_survey_baseline_merged$Hispanic[which(! (wash_survey_baseline_merged$Hispanic 
                                              %in% c("Yes",
                                                     "No",
                                                     "I don't know",
                                                     NA)))] <- "invalid"

# Marital Status: set responses that are out of range to invalid
wash_survey_baseline_merged$maritial_status[which(! (wash_survey_baseline_merged$maritial_status 
                                                     %in% c("Divorced",
                                                            "Married/Domestic Partner",
                                                            "Separated",
                                                            "Single",
                                                            "Widowed",
                                                            NA)))] <- "invalid"


# Income: set responses that are out of range to invalid
wash_survey_baseline_merged$income_level[which(! (wash_survey_baseline_merged$income_level 
                                                  %in% c("Less than $25,000",
                                                         "$25,000 to $49,999",
                                                         "$50,000 to $74,999",
                                                         "$75,000 to $99,999",
                                                         "$100,000 to $124,999",
                                                         "$125,000 to $150,000",
                                                         "More than $150,000",
                                                         NA)))] <- "invalid"

# Education: set responses that are out of range to invalid
wash_survey_baseline_merged$`education_leve;`[which(! (wash_survey_baseline_merged$`education_leve;` 
                                                       %in% c("Elementary/Grade School", 
                                                              "Middle School/Junior High",
                                                              "High School", 
                                                              "Some College","College",
                                                              "Some Graduate School", "Graduate School",
                                                              NA)))] <- "invalid"


# Active Duty: set responses that are out of range to invalid
wash_survey_baseline_merged$`currently_in_military`[which(! (wash_survey_baseline_merged$`currently_in_military` 
                                                             %in% c("Yes", 
                                                                    "No",
                                                                    NA)))] <- "invalid"

# Veteran: set responses that are out of range to invalid
wash_survey_baseline_merged$veteran[which(! (wash_survey_baseline_merged$veteran 
                                             %in% c("Yes", 
                                                    "No",
                                                    NA)))] <- "invalid"

# Physical activity: set responses that are out of range to invalid
wash_survey_baseline_merged$level_of_physical_activity[which(! (wash_survey_baseline_merged$level_of_physical_activity 
                                                                %in% c("Sedentary (desk job, no regular exercise)", 
                                                                       "Lightly Active (desk job/exercise a few times a week)",
                                                                       "Active (physical laborer or exercise at least 3 times a week)", 
                                                                       "Very Active (exercise vigorously at least 5 times a week)",
                                                                       NA)))] <- "invalid"


# Phone access: set responses that are out of range to invalid
wash_survey_baseline_merged$always_have_acccess_to_phone[which(! (wash_survey_baseline_merged$always_have_acccess_to_phone
                                                                  %in% c("Yes", 
                                                                         "other",
                                                                         "Other",
                                                                         "Not at work", 
                                                                      "Not at the gym",
                                                                         NA)))] <- "invalid"

# further recode variables
# Race - White, asian, black vs others
wash_survey_baseline_merged$race_recode <- wash_survey_baseline_merged$race

wash_survey_baseline_merged$race_recode[wash_survey_baseline_merged$race_recode == "American Indian or Alaska Native"] <- "Other"
wash_survey_baseline_merged$race_recode[wash_survey_baseline_merged$race_recode == "Native Hawaiian or Other Pacific Islander"] <- "Other"
wash_survey_baseline_merged$race_recode[wash_survey_baseline_merged$race_recode == "Other"] <- "Other"

# Race - integrate Hispanic into Race
wash_survey_baseline_merged$race_hispanic <- wash_survey_baseline_merged$race_recode
wash_survey_baseline_merged$race_hispanic[wash_survey_baseline_merged$Hispanic == "Yes"] <- "Hispanic"


# Martial status - married, single, divorced and others
wash_survey_baseline_merged$maritial_status_recode <- wash_survey_baseline_merged$maritial_status

wash_survey_baseline_merged$maritial_status_recode[wash_survey_baseline_merged$maritial_status_recode == "Separated"] <- "Other"
wash_survey_baseline_merged$maritial_status_recode[wash_survey_baseline_merged$maritial_status_recode == "Widowed"] <- "Other"

# Income level - combine levels of high income
wash_survey_baseline_merged$income_level_recode <- wash_survey_baseline_merged$income_level

wash_survey_baseline_merged$income_level_recode[wash_survey_baseline_merged$income_level_recode %in% c("More than $150,000",
                                                                                         "$125,000 to $150,000",
                                                                                         "$100,000 to $124,999")] <- "More than $100,000"
# Education - college, grad, high school or lower
wash_survey_baseline_merged$education_level_recode <- wash_survey_baseline_merged$`education_leve;`
wash_survey_baseline_merged$education_level_recode[wash_survey_baseline_merged$education_level_recode == "High School"] <- "High School and lower"
wash_survey_baseline_merged$education_level_recode[wash_survey_baseline_merged$education_level_recode == "Elementary/Grade School"] <- "High School and lower"
wash_survey_baseline_merged$education_level_recode[wash_survey_baseline_merged$education_level_recode == "Middle School/Junior High"] <- "High School and lower"

# combine two levels: Some College and College into one level
wash_survey_baseline_merged$education_level_recode[which(wash_survey_baseline_merged$education_level_recode 
                                                    %in% c("Some College", 
                                                           "College"))] <- "College"

# combine two levels: Some Graduate School and Graduate School into one level
wash_survey_baseline_merged$education_level_recode[which(wash_survey_baseline_merged$education_level_recode 
                                                    %in% c("Some Graduate School", 
                                                           "Graduate School"))] <- "Graduate School"                                                    

# Access to phones
# combine two levels: Not at work and Not at the gym into one level
wash_survey_baseline_merged$always_have_acccess_to_phone_recode <- wash_survey_baseline_merged$always_have_acccess_to_phone
wash_survey_baseline_merged$always_have_acccess_to_phone_recode[which(wash_survey_baseline_merged$always_have_acccess_to_phone_recode
                                                               %in% c("Not at work", 
                                                                      "Not at the gym"))] <- "Do not have access to phone(s) at the gym or work"                                                    

wash_survey_baseline_merged$always_have_acccess_to_phone_recode[wash_survey_baseline_merged$always_have_acccess_to_phone_recode %in% c("Other",
                                                                                                                         "other")] <- "Other"

# Create raw Table 1 (without invalid and missing data
# create variables of interest to create Table 1 
# for the purpose of creating Table 1, invalid data of variables of interest will be categorized into NA
# create temp variables to assign invalid to NA
wash_survey_baseline_merged$Age_1 <- wash_survey_baseline_merged$Age
wash_survey_baseline_merged$Age_1[which(wash_survey_baseline_merged$Age_1 == "invalid")] <- NA

wash_survey_baseline_merged$gender_1 <- wash_survey_baseline_merged$gender
wash_survey_baseline_merged$gender_1[which(wash_survey_baseline_merged$gender_1 == "invalid")] <- NA
wash_survey_baseline_merged$gender_1[which(wash_survey_baseline_merged$gender_1 == "Non-binary")] <- NA

wash_survey_baseline_merged$race_1 <- wash_survey_baseline_merged$race_hispanic
wash_survey_baseline_merged$race_1[which(wash_survey_baseline_merged$race_1 == "invalid")] <- NA

wash_survey_baseline_merged$maritial_status_1 <- wash_survey_baseline_merged$maritial_status_recode
wash_survey_baseline_merged$maritial_status_1[which(wash_survey_baseline_merged$maritial_status_1 == "invalid")] <- NA

wash_survey_baseline_merged$income_level_1 <- wash_survey_baseline_merged$income_level_recode
wash_survey_baseline_merged$income_level_1[which(wash_survey_baseline_merged$income_level_1 == "invalid")] <- NA

wash_survey_baseline_merged$education_level_1 <- wash_survey_baseline_merged$education_level_recode
wash_survey_baseline_merged$education_level_1[which(wash_survey_baseline_merged$education_level_1 == "invalid")] <- NA

wash_survey_baseline_merged$currently_in_military_1 <- wash_survey_baseline_merged$currently_in_military
wash_survey_baseline_merged$currently_in_military_1[which(wash_survey_baseline_merged$`currently_in_military_1` == "invalid")] <- NA

wash_survey_baseline_merged$veteran_1 <- wash_survey_baseline_merged$veteran
wash_survey_baseline_merged$veteran_1[which(wash_survey_baseline_merged$veteran_1 == "invalid")] <- NA

wash_survey_baseline_merged$level_of_physical_activity_1 <- wash_survey_baseline_merged$level_of_physical_activity
wash_survey_baseline_merged$level_of_physical_activity_1[which(wash_survey_baseline_merged$level_of_physical_activity_1 == "invalid")] <- NA

wash_survey_baseline_merged$always_have_acccess_to_phone_1 <- wash_survey_baseline_merged$always_have_acccess_to_phone_recode
wash_survey_baseline_merged$always_have_acccess_to_phone_1[which(wash_survey_baseline_merged$always_have_acccess_to_phone_1 == "invalid")] <- NA
wash_survey_baseline_merged$always_have_acccess_to_phone_1[which(wash_survey_baseline_merged$always_have_acccess_to_phone_1 == "Other")] <- NA


# Variables of interest to create Table 1 
myvars <- c("Age_1", "gender_1", "race_1","maritial_status_1",
            "income_level_1", "education_level_1","currently_in_military_1",
            "veteran_1","level_of_physical_activity_1",
            "always_have_acccess_to_phone_1")
                                       
# create Table 1
tab1 <- CreateTableOne(vars = myvars, data = wash_survey_baseline_merged)
print(tab1, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# export Table 1
tab1Mat <- print(tab1, showAllLevels = TRUE, nonnormal = biomarkers, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab1Mat, file = "Table1.csv")


# Create raw Table 1 Supplementary (show invalid and missing data)
# Variables of interest to create Table 1 supplementary
myvars_sup <- c("Age", "gender", "race_hispanic","maritial_status","income_level",
                "education_leve;","currently_in_military","veteran",
                "level_of_physical_activity",
                "always_have_acccess_to_phone")

# get number and % of invalid and missing data (need to manually adjust % for invalid data)
tab1_sup <- CreateTableOne(vars = myvars_sup, data = wash_survey_baseline_merged)
summary(tab1_sup)


#create demo for participants enrolled before and after study relaunch date
# survival curve for participants enrolled before Aug 30, 2020 and after
# split cohort based on start date before and after Aug 30, 2020
all_metadata_first_date_temp <- all_metadata_first_date
colnames(all_metadata_first_date_temp)[2] <- "Start_date"

all_survey_early_date$Start_date <- as.Date(all_survey_early_date$Start_date)

all_first_date <- rbind(all_survey_early_date, all_metadata_first_date_temp)

all_first_date <-
  all_first_date %>%
  group_by(`User ID`) %>%
  arrange(Start_date) %>%
  slice(1)

# combine first day to survival infor
day_in_study_all <- merge(day_in_study_all,all_first_date)

day_in_study_all_before <- subset(day_in_study_all, as.Date(Start_date) < "2020-08-30")
day_in_study_all_after <- subset(day_in_study_all, as.Date(Start_date) > "2020-08-29")

before_demo <- merge(wash_survey_baseline_merged, day_in_study_all_before, all.y = T)

after_demo <- merge(wash_survey_baseline_merged, day_in_study_all_after, all.y = T)

# create Table for participants enrolled before relaunch
tab_before <- CreateTableOne(vars = myvars, data = before_demo)
print(tab_before, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# export Table 1
tab_beforeMat <- print(tab_before, showAllLevels = TRUE, nonnormal = biomarkers, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab_beforeMat, file = "Table before.csv")

summary(as.numeric(before_demo$pos_panas_score_1))
summary(as.numeric(before_demo$neg_panas_score_1))


# get number and % of invalid and missing data (need to manually adjust % for invalid data)
tab1_before_sup <- CreateTableOne(vars = myvars_sup, data = before_demo)
summary(tab1_before_sup)

sum(is.na(before_demo$pos_panas_score))
sum(is.na(before_demo$neg_panas_score))

# create Table for participants enrolled after relaunch
tab_after <- CreateTableOne(vars = myvars, data = after_demo)
print(tab_after, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# export Table 1
tab_afterMat <- print(tab_after, showAllLevels = TRUE, nonnormal = biomarkers, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab_afterMat, file = "Table after.csv")

summary(as.numeric(after_demo$pos_panas_score_1))
summary(as.numeric(after_demo$neg_panas_score_1))

# get number and % of invalid and missing data (need to manually adjust % for invalid data)
tab1_after_sup <- CreateTableOne(vars = myvars_sup, data = after_demo)
summary(tab1_after_sup)

sum(is.na(after_demo$pos_panas_score))
sum(is.na(after_demo$neg_panas_score))



# create demo table for Android and iOS users
sensor_demo <- merge(wash_survey_baseline_merged, id_sensor_pair, all = T)
sensor_demo_android_before <- subset(sensor_demo, Device_Type == "a" & `User ID` %in% day_in_study_all_before$`User ID`)
sensor_demo_android_after <- subset(sensor_demo, Device_Type == "a" & `User ID` %in% day_in_study_all_after$`User ID`)

sensor_demo_ios_before <- subset(sensor_demo, Device_Type == "i" & `User ID` %in% day_in_study_all_before$`User ID`)
sensor_demo_ios_after <- subset(sensor_demo, Device_Type == "i" & `User ID` %in% day_in_study_all_after$`User ID`)


# create Table for android users
# phase 1
tab_android_before <- CreateTableOne(vars = myvars, data = sensor_demo_android_before)
print(tab_android_before, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# export Table 1
tab_androidMat_before <- print(tab_android_before, showAllLevels = TRUE, nonnormal = biomarkers, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab_androidMat_before, file = "Table Android_before.csv")

# phase 2
tab_android_after <- CreateTableOne(vars = myvars, data = sensor_demo_android_after)
print(tab_android_after, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# export Table 1
tab_androidMat_after <- print(tab_android_after, showAllLevels = TRUE, nonnormal = biomarkers, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab_androidMat_after, file = "Table Android_after.csv")



# create Table for ios users
# phase 1
tab_ios_before <- CreateTableOne(vars = myvars, data = sensor_demo_ios_before)
print(tab_ios_before, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# export Table 1
tab_iosMat_before <- print(tab_ios_before, showAllLevels = TRUE, nonnormal = biomarkers, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab_iosMat_before, file = "Table ios_before.csv")

# phase 2
tab_ios_after <- CreateTableOne(vars = myvars, data = sensor_demo_ios_after)
print(tab_ios_after, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# export Table 1
tab_iosMat_after <- print(tab_ios_after, showAllLevels = TRUE, nonnormal = biomarkers, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab_iosMat_after, file = "Table ios_after.csv")


######### long term enagagement heatmap cohorts ######################
# Phase 1 cohorts
id_by_clusters_before_demo <- merge(id_by_clusters_before, wash_survey_baseline_merged, all.x = T)

# c1
c1_before <- subset(id_by_clusters_before_demo, cluster == "C1")

# create Table for participants enrolled before relaunch
tab_c1_before <- CreateTableOne(vars = myvars, data = c1_before)
print(tab_c1_before, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# export Table 1
tab_c1beforeMat <- print(tab_c1_before, showAllLevels = TRUE, nonnormal = biomarkers, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab_c1beforeMat, file = "Table c1 before.csv")

summary(as.numeric(c1_before$pos_panas_score_1))
summary(as.numeric(c1_before$neg_panas_score_1))


# c2
c2_before <- subset(id_by_clusters_before_demo, cluster == "C2")

# create Table for participants enrolled before relaunch
tab_c2_before <- CreateTableOne(vars = myvars, data = c2_before)
print(tab_c2_before, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# export Table 1
tab_c2beforeMat <- print(tab_c2_before, showAllLevels = TRUE, nonnormal = biomarkers, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab_c2beforeMat, file = "Table c2 before.csv")


summary(as.numeric(c2_before$pos_panas_score_1))
summary(as.numeric(c2_before$neg_panas_score_1))


# c3
c3_before <- subset(id_by_clusters_before_demo, cluster == "C3")

# create Table for participants enrolled before relaunch
tab_c3_before <- CreateTableOne(vars = myvars, data = c3_before)
print(tab_c3_before, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# export Table 1
tab_c3beforeMat <- print(tab_c3_before, showAllLevels = TRUE, nonnormal = biomarkers, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab_c3beforeMat, file = "Table c3 before.csv")

summary(as.numeric(c3_before$pos_panas_score_1))
summary(as.numeric(c3_before$neg_panas_score_1))


# c4
c4_before <- subset(id_by_clusters_before_demo, cluster == "C4")

# create Table for participants enrolled before relaunch
tab_c4_before <- CreateTableOne(vars = myvars, data = c4_before)
print(tab_c4_before, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# export Table 1
tab_c4beforeMat <- print(tab_c4_before, showAllLevels = TRUE, nonnormal = biomarkers, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab_c4beforeMat, file = "Table c4 before.csv")


summary(as.numeric(c4_before$pos_panas_score_1))
summary(as.numeric(c4_before$neg_panas_score_1))



# Phase 2 cohorts
id_by_clusters_after_demo <- merge(id_by_clusters_after, wash_survey_baseline_merged, all.x = T)

# c1
c1_after <- subset(id_by_clusters_after_demo, cluster == "C1")

# create Table for participants enrolled after relaunch
tab_c1_after <- CreateTableOne(vars = myvars, data = c1_after)
print(tab_c1_after, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# export Table 1
tab_c1afterMat <- print(tab_c1_after, showAllLevels = TRUE, nonnormal = biomarkers, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab_c1afterMat, file = "Table c1 after.csv")


summary(as.numeric(c1_after$pos_panas_score_1))
summary(as.numeric(c1_after$neg_panas_score_1))



# c2
c2_after <- subset(id_by_clusters_after_demo, cluster == "C2")

# create Table for participants enrolled after relaunch
tab_c2_after <- CreateTableOne(vars = myvars, data = c2_after)
print(tab_c2_after, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# export Table 2
tab_c2afterMat <- print(tab_c2_after, showAllLevels = TRUE, nonnormal = biomarkers, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab_c2afterMat, file = "Table c2 after.csv")

summary(as.numeric(c2_after$pos_panas_score_1))
summary(as.numeric(c2_after$neg_panas_score_1))


# c3
c3_after <- subset(id_by_clusters_after_demo, cluster == "C3")

# create Table for participants enrolled after relaunch
tab_c3_after <- CreateTableOne(vars = myvars, data = c3_after)
print(tab_c3_after, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# export Table 3
tab_c3afterMat <- print(tab_c3_after, showAllLevels = TRUE, nonnormal = biomarkers, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab_c3afterMat, file = "Table c3 after.csv")

summary(as.numeric(c3_after$pos_panas_score_1))
summary(as.numeric(c3_after$neg_panas_score_1))


# c4
c4_after <- subset(id_by_clusters_after_demo, cluster == "C4")


# create Table for participants enrolled after relaunch
tab_c4_after <- CreateTableOne(vars = myvars, data = c4_after)
print(tab_c4_after, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# export Table 4
tab_c4afterMat <- print(tab_c4_after, showAllLevels = TRUE, nonnormal = biomarkers, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab_c4afterMat, file = "Table c4 after.csv")

summary(as.numeric(c4_after$pos_panas_score_1))
summary(as.numeric(c4_after$neg_panas_score_1))


# not facet by phases
id_by_clusters_demo <- merge(id_by_clusters, wash_survey_baseline_merged, all.x = T)

# c1
c1 <- subset(id_by_clusters_demo, cluster == "C1")

# create Table for participants enrolled before relaunch
tab_c1 <- CreateTableOne(vars = myvars, data = c1)
print(tab_c1, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# export Table 1
tab_c1Mat <- print(tab_c1, showAllLevels = TRUE, nonnormal = biomarkers, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab_c1Mat, file = "Table c1.csv")

summary(as.numeric(c1$pos_panas_score_1))
summary(as.numeric(c1$neg_panas_score_1))


# c2
c2 <- subset(id_by_clusters_demo, cluster == "C2")

# create Table for participants enrolled before relaunch
tab_c2 <- CreateTableOne(vars = myvars, data = c2)
print(tab_c2, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# export Table 1
tab_c2Mat <- print(tab_c2, showAllLevels = TRUE, nonnormal = biomarkers, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab_c2Mat, file = "Table c2.csv")

summary(as.numeric(c2$pos_panas_score_1))
summary(as.numeric(c2$neg_panas_score_1))


# c3
c3 <- subset(id_by_clusters_demo, cluster == "C3")

# create Table for participants enrolled before relaunch
tab_c3 <- CreateTableOne(vars = myvars, data = c3)
print(tab_c3, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# export Table 1
tab_c3Mat <- print(tab_c3, showAllLevels = TRUE, nonnormal = biomarkers, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab_c3Mat, file = "Table c3.csv")

summary(as.numeric(c3$pos_panas_score_1))
summary(as.numeric(c3$neg_panas_score_1))


# c4
c4 <- subset(id_by_clusters_demo, cluster == "C4")

# create Table for participants enrolled before relaunch
tab_c4 <- CreateTableOne(vars = myvars, data = c4)
print(tab_c4, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# export Table 1
tab_c4Mat <- print(tab_c4, showAllLevels = TRUE, nonnormal = biomarkers, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(tab_c4Mat, file = "Table c4.csv")

summary(as.numeric(c4$pos_panas_score_1))
summary(as.numeric(c4$neg_panas_score_1))


# Geospatial information
# 3514 shared data
wash_survey_baseline_merged_geo <- subset(wash_survey_baseline_merged, select = c("User ID", "city","state","zip_code"))
plot_na_pareto(wash_survey_baseline_merged_geo)

# data cleanup, merge 
wash_survey_baseline_merged_geo$city_recode <- tolower(wash_survey_baseline_merged_geo$city)
wash_survey_baseline_merged_geo$state_recode <- tolower(wash_survey_baseline_merged_geo$state)

# obtain frequency of each city/state/zip code
city_table <- as.data.frame(table(wash_survey_baseline_merged_geo$city_recode))
state_table <- as.data.frame(table(wash_survey_baseline_merged_geo$state_recode))
zipcode_table <- as.data.frame(table(wash_survey_baseline_merged_geo$`zip code`))

# calculate prop
state_table$Prop <- round(state_table$Freq / sum(! is.na(wash_survey_baseline_merged_geo$state_recode)) * 100,1)

