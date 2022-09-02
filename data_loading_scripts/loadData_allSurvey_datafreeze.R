## To be able to run this script you will need to import "20220110 - Bad-Actor Writer IDs.txt" 

## Load data from Synapse #######################################

## Packages ########################################

library(synapser)
library(data.table)
library(magrittr)
library(dplyr)
library(tidyr)

## Functions #######################################

# load data from Synapse
loadSynapseData <- function(dataset_id) {
  # download from Synapse using syn_id;
  # relies on secrets
  files <- synGet(entity=dataset_id)
  # get cache file path
  filePath <- files$path
  # read csv
  data <- fread(filePath)
  return(data)
}

## Run script ######################################
# Synapse credentials
synLogin()

# WASH Project Survey ACBDEFG+end of survey 
wash_survey_a <- loadSynapseData('syn26715740')
wash_survey_b <- loadSynapseData('syn26715742')
wash_survey_c <- loadSynapseData('syn26715744')
wash_survey_d <- loadSynapseData('syn26715746')
wash_survey_e <- loadSynapseData('syn26715748')
wash_survey_end_of_study <- loadSynapseData('syn26715754')
wash_survey_f <- loadSynapseData('syn26715750')
wash_survey_g <- loadSynapseData('syn26715752')

# Rename header
# Rationale: the first 2 rows contain different lvl of detail of a question
# Header: question UUID
# First row: original question --> too long for a header
# Second row: shortened question with key words only
# Exception: End of Study survey, only contains one row with question UUID and original question
# step 1: Copy second/first row to header:
colnames(wash_survey_a) <- as.character(wash_survey_a[2, ])  
colnames(wash_survey_b) <- as.character(wash_survey_b[2, ])   
colnames(wash_survey_c) <- as.character(wash_survey_c[2, ])   
colnames(wash_survey_d) <- as.character(wash_survey_d[2, ])   
colnames(wash_survey_e) <- as.character(wash_survey_e[2, ])   
colnames(wash_survey_end_of_study) <- as.character(wash_survey_end_of_study[1, ])   
colnames(wash_survey_f) <- as.character(wash_survey_f[2, ])   
colnames(wash_survey_g) <- as.character(wash_survey_g[2, ])   

# step 2: fill in colnames for column 2, 3 and 4
colnames(wash_survey_a)[2] <- "User ID"
colnames(wash_survey_a)[3] <- "date taken"
colnames(wash_survey_a)[4] <- "SID"

colnames(wash_survey_b)[2] <- "User ID"
colnames(wash_survey_b)[3] <- "date taken"
colnames(wash_survey_b)[4] <- "SID"

colnames(wash_survey_c)[2] <- "User ID"
colnames(wash_survey_c)[3] <- "date taken"
colnames(wash_survey_c)[4] <- "SID"

colnames(wash_survey_d)[2] <- "User ID"
colnames(wash_survey_d)[3] <- "date taken"
colnames(wash_survey_d)[4] <- "SID"

colnames(wash_survey_e)[2] <- "User ID"
colnames(wash_survey_e)[3] <- "date taken"
colnames(wash_survey_e)[4] <- "SID"

colnames(wash_survey_end_of_study)[2] <- "User ID"
colnames(wash_survey_end_of_study)[3] <- "date taken"
colnames(wash_survey_end_of_study)[4] <- "SID"

colnames(wash_survey_f)[2] <- "User ID"
colnames(wash_survey_f)[3] <- "date taken"
colnames(wash_survey_f)[4] <- "SID"

colnames(wash_survey_g)[2] <- "User ID"
colnames(wash_survey_g)[3] <- "date taken"
colnames(wash_survey_g)[4] <- "SID"

# step 3: delete rows that contain unnecessary header info
wash_survey_a <- wash_survey_a[-c(1,2),]
wash_survey_b <- wash_survey_b[-c(1,2),]
wash_survey_c <- wash_survey_c[-c(1,2),]
wash_survey_d <- wash_survey_d[-c(1,2),]
wash_survey_e <- wash_survey_e[-c(1,2),]
wash_survey_end_of_study <- wash_survey_end_of_study[-c(1),]
wash_survey_f <- wash_survey_f[-c(1,2),]
wash_survey_g <- wash_survey_g[-c(1,2),]

# step 4: Column 14 of Survey B is missing a header
colnames(wash_survey_b)[14] <- "last_24_hours_10+_crowds"

# create one data freeze variable and one data variable to filter out test data
cutoff1 <- as.Date("2022-02-09")
cutoff2 <- as.Date("2020-03-16")

# subset dataset by data freeze and test data cutoff time
#wash_survey_baseline <- subset(wash_survey_baseline, `date taken` <= cutoff1)
#wash_survey_baseline <- subset(wash_survey_baseline, `date taken` >= cutoff2)
wash_survey_a <- subset(wash_survey_a, as.Date(`date taken`) <= cutoff1)
wash_survey_b <- subset(wash_survey_b, as.Date(`date taken`) <= cutoff1)
wash_survey_c <- subset(wash_survey_c, as.Date(`date taken`) <= cutoff1)
wash_survey_d <- subset(wash_survey_d, as.Date(`date taken`) <= cutoff1)
wash_survey_e <- subset(wash_survey_e, as.Date(`date taken`) <= cutoff1)
wash_survey_end_of_study <- subset(wash_survey_end_of_study, as.Date(`date taken`) <= cutoff1)
wash_survey_f <- subset(wash_survey_f, as.Date(`date taken`) <= cutoff1)
wash_survey_g <- subset(wash_survey_g, as.Date(`date taken`) <= cutoff1)

wash_survey_a <- subset(wash_survey_a, as.Date(`date taken`) >= cutoff2)
wash_survey_b <- subset(wash_survey_b, as.Date(`date taken`) >= cutoff2)
wash_survey_c <- subset(wash_survey_c, as.Date(`date taken`) >= cutoff2)
wash_survey_d <- subset(wash_survey_d,as.Date(`date taken`) >= cutoff2)
wash_survey_e <- subset(wash_survey_e, as.Date(`date taken`) >= cutoff2)
wash_survey_end_of_study <- subset(wash_survey_end_of_study, as.Date(`date taken`) >= cutoff2)
wash_survey_f <- subset(wash_survey_f, as.Date(`date taken`) >= cutoff2)
wash_survey_g <- subset(wash_survey_g, as.Date(`date taken`) >= cutoff2)

# filter out bad actors
# first replace single quote with double quote in bad_actor_Jan2022.rds
bad_actor_Jan2022$V1 <- gsub("\'","\"", bad_actor_Jan2022$V1)

# exclude bad actor records from all surveys
wash_survey_a_ba <- subset(wash_survey_a, !(`User ID` %in% c(bad_actor_Jan2022)))
wash_survey_b_ba <- subset(wash_survey_b, !(`User ID` %in% c(bad_actor_Jan2022)))
wash_survey_c_ba <- subset(wash_survey_c, !(`User ID` %in% c(bad_actor_Jan2022)))
wash_survey_d_ba <- subset(wash_survey_d, !(`User ID` %in% c(bad_actor_Jan2022)))
wash_survey_e_ba <- subset(wash_survey_e, !(`User ID` %in% c(bad_actor_Jan2022)))
wash_survey_end_of_study_ba <- subset(wash_survey_end_of_study, !(`User ID` %in% c(bad_actor_Jan2022)))
wash_survey_f_ba <- subset(wash_survey_f, !(`User ID` %in% c(bad_actor_Jan2022)))
wash_survey_g_ba <- subset(wash_survey_g, !(`User ID` %in% c(bad_actor_Jan2022)))

# keep column name consistent across Survey ACEG, use Survey A as template
header_aceg <- colnames(wash_survey_a_ba)
colnames(wash_survey_c_ba) <- header_aceg
colnames(wash_survey_e_ba) <- header_aceg
colnames(wash_survey_g_ba) <- header_aceg

# keep column name consistent across Survey BDF
header_bdf <- colnames(wash_survey_b_ba)
colnames(wash_survey_d_ba) <- header_bdf
colnames(wash_survey_f_ba) <- header_bdf

## Data cleaning Function #########################
data_cleaning_colrm <- function(df) {
  # remove empty columns
  subset(df, select = -which(colnames(df) %in% 
                               c("1","NA", "0", "nan")))}

## Run script ######################################
# apply data_cleaning_colrm to survey ABCDEFG
wash_survey_a_ba <- data_cleaning_colrm(wash_survey_a_ba)
wash_survey_b_ba <- data_cleaning_colrm(wash_survey_b_ba)
wash_survey_c_ba <- data_cleaning_colrm(wash_survey_c_ba)
wash_survey_d_ba <- data_cleaning_colrm(wash_survey_d_ba)
wash_survey_e_ba <- data_cleaning_colrm(wash_survey_e_ba)
wash_survey_f_ba <- data_cleaning_colrm(wash_survey_f_ba)
wash_survey_g_ba <- data_cleaning_colrm(wash_survey_g_ba)
wash_survey_end_of_study_ba <- data_cleaning_colrm(wash_survey_end_of_study_ba)

# re-format responses for survey A
# variable: difficulty: chores
wash_survey_a_ba$difficulty_chores[wash_survey_a_ba$difficulty_chores == "Not at all"] <-
  "1 - Not at all"
wash_survey_a_ba$difficulty_chores[wash_survey_a_ba$difficulty_chores == "A little bit"] <-
  "2 - A little bit"
wash_survey_a_ba$difficulty_chores[wash_survey_a_ba$difficulty_chores == "Somewhat"] <-
  "3 - Somewhat"
wash_survey_a_ba$difficulty_chores[wash_survey_a_ba$difficulty_chores == "Quite a bit"] <-
  "4 - Quite a bit"
wash_survey_a_ba$difficulty_chores[wash_survey_a_ba$difficulty_chores == "Very much"] <-
  "5 - Very much"
# variable: how you felt
wash_survey_a_ba$how_you_felt[wash_survey_a_ba$how_you_felt == "Worst ever"] <-
  "1 - Worst ever"
wash_survey_a_ba$how_you_felt[wash_survey_a_ba$how_you_felt == "Bad mood"] <-
  "2 - Bad mood"
wash_survey_a_ba$how_you_felt[wash_survey_a_ba$how_you_felt == "Average"] <-
  "3 - Average"
wash_survey_a_ba$how_you_felt[wash_survey_a_ba$how_you_felt == "Good mood"] <-
  "4 - Good mood"
wash_survey_a_ba$how_you_felt[wash_survey_a_ba$how_you_felt == "Best ever"] <-
  "5 - Best ever"
# variable:yesterday: felt
Encoding(wash_survey_a_ba$yesterday_felt) <- "UTF-8"
wash_survey_a_ba$yesterday_felt[wash_survey_a_ba$yesterday_felt %in% c("1 - Worst ever", "1 - Worst ever")] <-
  "1 - Worst ever"
wash_survey_a_ba$yesterday_felt[wash_survey_a_ba$yesterday_felt %in% c("2 - Low energy", "2 - Bad cold/flu symptoms", "2 - Bad cold/flu symptoms")] <-
  "2 - Bad cold/flu symptoms"
wash_survey_a_ba$yesterday_felt[wash_survey_a_ba$yesterday_felt %in% c("4 - Good energy", "4 - Minor cold/flu symptoms", "4 - Minor cold/flu symptoms")] <-
  "4 - Minor cold/flu symptoms"
wash_survey_a_ba$yesterday_felt[wash_survey_a_ba$yesterday_felt %in% c("5 - No cold/flu symptoms", "5 - No cold/flu symptoms", "5 - Best ever")] <-
  "5 - No cold/flu symptoms"

# re-format responses for survey B
# variable:last night: difficulty to sleep
wash_survey_b_ba$last_night_difficulty_to_sleep[wash_survey_b_ba$last_night_difficulty_to_sleep == "Not at all"] <-
  "1 - Not at all"
wash_survey_b_ba$last_night_difficulty_to_sleep[wash_survey_b_ba$last_night_difficulty_to_sleep == "A little bit"] <-
  "2 - A little bit"
wash_survey_b_ba$last_night_difficulty_to_sleep[wash_survey_b_ba$last_night_difficulty_to_sleep == "Somewhat"] <-
  "3 - Somewhat"
wash_survey_b_ba$last_night_difficulty_to_sleep[wash_survey_b_ba$last_night_difficulty_to_sleep == "Quite a bit"] <-
  "4 - Quite a bit"
wash_survey_b_ba$last_night_difficulty_to_sleep[wash_survey_b_ba$last_night_difficulty_to_sleep == "Very much"] <-
  "5 - Very much"
# variable:today: refreshed
wash_survey_b_ba$today_refreshed[wash_survey_b_ba$today_refreshed == "Not at all"] <-
  "1 - Not at all"
wash_survey_b_ba$today_refreshed[wash_survey_b_ba$today_refreshed == "A little bit"] <-
  "2 - A little bit"
wash_survey_b_ba$today_refreshed[wash_survey_b_ba$today_refreshed == "Somewhat"] <-
  "3 - Somewhat"
wash_survey_b_ba$today_refreshed[wash_survey_b_ba$today_refreshed == "Quite a bit"] <-
  "4 - Quite a bit"
wash_survey_b_ba$today_refreshed[wash_survey_b_ba$today_refreshed == "Very much"] <-
  "5 - Very much"
# variable:how you felt
Encoding(wash_survey_b_ba$how_you_felt) <- "UTF-8"
wash_survey_b_ba$how_you_felt[wash_survey_b_ba$how_you_felt %in% c("1 - Worst ever", "1 - Worst ever")] <-
  "1 - Worst ever"
wash_survey_b_ba$how_you_felt[wash_survey_b_ba$how_you_felt %in% c("2 - Low energy", "2 - Bad cold/flu symptoms", "2 - Bad cold/flu symptoms")] <-
  "2 - Bad cold/flu symptoms"
wash_survey_b_ba$how_you_felt[wash_survey_b_ba$how_you_felt %in% c("4 - Good energy", "4 - Minor cold/flu symptoms", "4 - Minor cold/flu symptoms")] <-
  "4 - Minor cold/flu symptoms"
wash_survey_b_ba$how_you_felt[wash_survey_b_ba$how_you_felt %in% c("5 - No cold/flu symptoms", "5 - No cold/flu symptoms", "5 - Best ever")] <-
  "5 - No cold/flu symptoms"

# re-format responses for survey C
# variable: difficulty: chores
wash_survey_c_ba$difficulty_chores[wash_survey_c_ba$difficulty_chores == "Not at all"] <-
  "1 - Not at all"
wash_survey_c_ba$difficulty_chores[wash_survey_c_ba$difficulty_chores == "A little bit"] <-
  "2 - A little bit"
wash_survey_c_ba$difficulty_chores[wash_survey_c_ba$difficulty_chores == "Somewhat"] <-
  "3 - Somewhat"
wash_survey_c_ba$difficulty_chores[wash_survey_c_ba$difficulty_chores == "Quite a bit"] <-
  "4 - Quite a bit"
wash_survey_c_ba$difficulty_chores[wash_survey_c_ba$difficulty_chores == "Very much"] <-
  "5 - Very much"
# variable: how you felt
wash_survey_c_ba$how_you_felt[wash_survey_c_ba$how_you_felt == "Worst ever"] <-
  "1 - Worst ever"
wash_survey_c_ba$how_you_felt[wash_survey_c_ba$how_you_felt == "Bad mood"] <-
  "2 - Bad mood"
wash_survey_c_ba$how_you_felt[wash_survey_c_ba$how_you_felt == "Average"] <-
  "3 - Average"
wash_survey_c_ba$how_you_felt[wash_survey_c_ba$how_you_felt == "Good mood"] <-
  "4 - Good mood"
wash_survey_c_ba$how_you_felt[wash_survey_c_ba$how_you_felt == "Best ever"] <-
  "5 - Best ever"
# variable:yesterday: felt
Encoding(wash_survey_c_ba$yesterday_felt) <- "UTF-8"
wash_survey_c_ba$yesterday_felt[wash_survey_c_ba$yesterday_felt %in% c("1 - Worst ever", "1 - Worst ever")] <-
  "1 - Worst ever"
wash_survey_c_ba$yesterday_felt[wash_survey_c_ba$yesterday_felt %in% c("2 - Low energy", "2 - Bad cold/flu symptoms", "2 - Bad cold/flu symptoms")] <-
  "2 - Bad cold/flu symptoms"
wash_survey_c_ba$yesterday_felt[wash_survey_c_ba$yesterday_felt %in% c("4 - Good energy", "4 - Minor cold/flu symptoms", "4 - Minor cold/flu symptoms")] <-
  "4 - Minor cold/flu symptoms"
wash_survey_c_ba$yesterday_felt[wash_survey_c_ba$yesterday_felt %in% c("5 - No cold/flu symptoms", "5 - No cold/flu symptoms", "5 - Best ever")] <-
  "5 - No cold/flu symptoms"

# re-format responses for survey D
# variable:last night: difficulty to sleep
wash_survey_d_ba$last_night_difficulty_to_sleep[wash_survey_d_ba$last_night_difficulty_to_sleep == "Not at all"] <-
  "1 - Not at all"
wash_survey_d_ba$last_night_difficulty_to_sleep[wash_survey_d_ba$last_night_difficulty_to_sleep == "A little bit"] <-
  "2 - A little bit"
wash_survey_d_ba$last_night_difficulty_to_sleep[wash_survey_d_ba$last_night_difficulty_to_sleep == "Somewhat"] <-
  "3 - Somewhat"
wash_survey_d_ba$last_night_difficulty_to_sleep[wash_survey_d_ba$last_night_difficulty_to_sleep == "Quite a bit"] <-
  "4 - Quite a bit"
wash_survey_d_ba$last_night_difficulty_to_sleep[wash_survey_d_ba$last_night_difficulty_to_sleep == "Very much"] <-
  "5 - Very much"
# variable:today: refreshed
wash_survey_d_ba$today_refreshed[wash_survey_d_ba$today_refreshed == "Not at all"] <-
  "1 - Not at all"
wash_survey_d_ba$today_refreshed[wash_survey_d_ba$today_refreshed == "A little bit"] <-
  "2 - A little bit"
wash_survey_d_ba$today_refreshed[wash_survey_d_ba$today_refreshed == "Somewhat"] <-
  "3 - Somewhat"
wash_survey_d_ba$today_refreshed[wash_survey_d_ba$today_refreshed == "Quite a bit"] <-
  "4 - Quite a bit"
wash_survey_d_ba$today_refreshed[wash_survey_d_ba$today_refreshed == "Very much"] <-
  "5 - Very much"
# variable:how you felt
Encoding(wash_survey_d_ba$how_you_felt) <- "UTF-8"
wash_survey_d_ba$how_you_felt[wash_survey_d_ba$how_you_felt %in% c("1 - Worst ever", "1 - Worst ever")] <-
  "1 - Worst ever"
wash_survey_d_ba$how_you_felt[wash_survey_d_ba$how_you_felt %in% c("2 - Low energy", "2 - Bad cold/flu symptoms", "2 - Bad cold/flu symptoms")] <-
  "2 - Bad cold/flu symptoms"
wash_survey_d_ba$how_you_felt[wash_survey_d_ba$how_you_felt %in% c("4 - Good energy", "4 - Minor cold/flu symptoms", "4 - Minor cold/flu symptoms")] <-
  "4 - Minor cold/flu symptoms"
wash_survey_d_ba$how_you_felt[wash_survey_d_ba$how_you_felt %in% c("5 - No cold/flu symptoms", "5 - No cold/flu symptoms", "5 - Best ever")] <-
  "5 - No cold/flu symptoms"

# re-format responses for survey E
# variable: difficulty: chores
wash_survey_e_ba$difficulty_chores[wash_survey_e_ba$difficulty_chores == "Not at all"] <-
  "1 - Not at all"
wash_survey_e_ba$difficulty_chores[wash_survey_e_ba$difficulty_chores == "A little bit"] <-
  "2 - A little bit"
wash_survey_e_ba$difficulty_chores[wash_survey_e_ba$difficulty_chores == "Somewhat"] <-
  "3 - Somewhat"
wash_survey_e_ba$difficulty_chores[wash_survey_e_ba$difficulty_chores == "Quite a bit"] <-
  "4 - Quite a bit"
wash_survey_e_ba$difficulty_chores[wash_survey_e_ba$difficulty_chores == "Very much"] <-
  "5 - Very much"
# variable: how you felt
wash_survey_e_ba$how_you_felt[wash_survey_e_ba$how_you_felt == "Worst ever"] <-
  "1 - Worst ever"
wash_survey_e_ba$how_you_felt[wash_survey_e_ba$how_you_felt == "Bad mood"] <-
  "2 - Bad mood"
wash_survey_e_ba$how_you_felt[wash_survey_e_ba$how_you_felt == "Average"] <-
  "3 - Average"
wash_survey_e_ba$how_you_felt[wash_survey_e_ba$how_you_felt == "Good mood"] <-
  "4 - Good mood"
wash_survey_e_ba$how_you_felt[wash_survey_e_ba$how_you_felt == "Best ever"] <-
  "5 - Best ever"
# variable:yesterday: felt
Encoding(wash_survey_e_ba$yesterday_felt) <- "UTF-8"
wash_survey_e_ba$yesterday_felt[wash_survey_e_ba$yesterday_felt %in% c("1 - Worst ever", "1 - Worst ever")] <-
  "1 - Worst ever"
wash_survey_e_ba$yesterday_felt[wash_survey_e_ba$yesterday_felt %in% c("2 - Low energy", "2 - Bad cold/flu symptoms", "2 - Bad cold/flu symptoms")] <-
  "2 - Bad cold/flu symptoms"
wash_survey_e_ba$yesterday_felt[wash_survey_e_ba$yesterday_felt %in% c("4 - Good energy", "4 - Minor cold/flu symptoms", "4 - Minor cold/flu symptoms")] <-
  "4 - Minor cold/flu symptoms"
wash_survey_e_ba$yesterday_felt[wash_survey_e_ba$yesterday_felt %in% c("5 - No cold/flu symptoms", "5 - No cold/flu symptoms", "5 - Best ever")] <-
  "5 - No cold/flu symptoms"

# re-format responses for survey F
# variable:last night: difficulty to sleep
wash_survey_f_ba$last_night_difficulty_to_sleep[wash_survey_f_ba$last_night_difficulty_to_sleep == "Not at all"] <-
  "1 - Not at all"
wash_survey_f_ba$last_night_difficulty_to_sleep[wash_survey_f_ba$last_night_difficulty_to_sleep == "A little bit"] <-
  "2 - A little bit"
wash_survey_f_ba$last_night_difficulty_to_sleep[wash_survey_f_ba$last_night_difficulty_to_sleep == "Somewhat"] <-
  "3 - Somewhat"
wash_survey_f_ba$last_night_difficulty_to_sleep[wash_survey_f_ba$last_night_difficulty_to_sleep == "Quite a bit"] <-
  "4 - Quite a bit"
wash_survey_f_ba$last_night_difficulty_to_sleep[wash_survey_f_ba$last_night_difficulty_to_sleep == "Very much"] <-
  "5 - Very much"
# variable:today: refreshed
wash_survey_f_ba$today_refreshed[wash_survey_f_ba$today_refreshed == "Not at all"] <-
  "1 - Not at all"
wash_survey_f_ba$today_refreshed[wash_survey_f_ba$today_refreshed == "A little bit"] <-
  "2 - A little bit"
wash_survey_f_ba$today_refreshed[wash_survey_f_ba$today_refreshed == "Somewhat"] <-
  "3 - Somewhat"
wash_survey_f_ba$today_refreshed[wash_survey_f_ba$today_refreshed == "Quite a bit"] <-
  "4 - Quite a bit"
wash_survey_f_ba$today_refreshed[wash_survey_f_ba$today_refreshed == "Very much"] <-
  "5 - Very much"
# variable:how you felt
Encoding(wash_survey_f_ba$how_you_felt) <- "UTF-8"
wash_survey_f_ba$how_you_felt[wash_survey_f_ba$how_you_felt %in% c("1 - Worst ever", "1 - Worst ever")] <-
  "1 - Worst ever"
wash_survey_f_ba$how_you_felt[wash_survey_f_ba$how_you_felt %in% c("2 - Low energy", "2 - Bad cold/flu symptoms", "2 - Bad cold/flu symptoms")] <-
  "2 - Bad cold/flu symptoms"
wash_survey_f_ba$how_you_felt[wash_survey_f_ba$how_you_felt %in% c("4 - Good energy", "4 - Minor cold/flu symptoms", "4 - Minor cold/flu symptoms")] <-
  "4 - Minor cold/flu symptoms"
wash_survey_f_ba$how_you_felt[wash_survey_f_ba$how_you_felt %in% c("5 - No cold/flu symptoms", "5 - No cold/flu symptoms", "5 - Best ever")] <-
  "5 - No cold/flu symptoms"

# re-format responses for survey G
# variable: difficulty: chores
wash_survey_g_ba$difficulty_chores[wash_survey_g_ba$difficulty_chores == "Not at all"] <-
  "1 - Not at all"
wash_survey_g_ba$difficulty_chores[wash_survey_g_ba$difficulty_chores == "A little bit"] <-
  "2 - A little bit"
wash_survey_g_ba$difficulty_chores[wash_survey_g_ba$difficulty_chores == "Somewhat"] <-
  "3 - Somewhat"
wash_survey_g_ba$difficulty_chores[wash_survey_g_ba$difficulty_chores == "Quite a bit"] <-
  "4 - Quite a bit"
wash_survey_g_ba$difficulty_chores[wash_survey_g_ba$difficulty_chores == "Very much"] <-
  "5 - Very much"
# variable: how you felt
wash_survey_g_ba$how_you_felt[wash_survey_g_ba$how_you_felt == "Worst ever"] <-
  "1 - Worst ever"
wash_survey_g_ba$how_you_felt[wash_survey_g_ba$how_you_felt == "Bad mood"] <-
  "2 - Bad mood"
wash_survey_g_ba$how_you_felt[wash_survey_g_ba$how_you_felt == "Average"] <-
  "3 - Average"
wash_survey_g_ba$how_you_felt[wash_survey_g_ba$how_you_felt == "Good mood"] <-
  "4 - Good mood"
wash_survey_g_ba$how_you_felt[wash_survey_g_ba$how_you_felt == "Best ever"] <-
  "5 - Best ever"
# variable:yesterday: felt
Encoding(wash_survey_g_ba$yesterday_felt) <- "UTF-8"
wash_survey_g_ba$yesterday_felt[wash_survey_g_ba$yesterday_felt %in% c("1 - Worst ever", "1 - Worst ever")] <-
  "1 - Worst ever"
wash_survey_g_ba$yesterday_felt[wash_survey_g_ba$yesterday_felt %in% c("2 - Low energy", "2 - Bad cold/flu symptoms", "2 - Bad cold/flu symptoms")] <-
  "2 - Bad cold/flu symptoms"
wash_survey_g_ba$yesterday_felt[wash_survey_g_ba$yesterday_felt %in% c("4 - Good energy", "4 - Minor cold/flu symptoms", "4 - Minor cold/flu symptoms")] <-
  "4 - Minor cold/flu symptoms"
wash_survey_g_ba$yesterday_felt[wash_survey_g_ba$yesterday_felt %in% c("5 - No cold/flu symptoms", "5 - No cold/flu symptoms", "5 - Best ever")] <-
  "5 - No cold/flu symptoms"

# check if multiple entries submitted by same participant on the same day
# select the latest submission if there are multiple entries
# End of Study Survey
wash_survey_end_of_study_cleaned_duprm <- 
  wash_survey_end_of_study_ba %>% 
  group_by(`User ID`) %>%  
  arrange(desc(`date taken`)) %>%
  slice(1)

# Survey A
wash_survey_a_ba$Y_M_D <- format(as.Date(wash_survey_a_ba$`date taken`), "%Y-%m-%d")

wash_survey_a_cleaned <- 
  wash_survey_a_ba %>% 
  group_by(`User ID`,Y_M_D) %>%  
  arrange(desc(`date taken`)) %>%
  slice(1)

# Survey B
wash_survey_b_ba$Y_M_D <- format(as.Date(wash_survey_b_ba$`date taken`), "%Y-%m-%d")

wash_survey_b_cleaned <- 
  wash_survey_b_ba %>% 
  group_by(`User ID`,Y_M_D) %>%  
  arrange(desc(`date taken`)) %>%
  slice(1)

# Survey c
wash_survey_c_ba$Y_M_D <- format(as.Date(wash_survey_c_ba$`date taken`), "%Y-%m-%d")

wash_survey_c_cleaned <- 
  wash_survey_c_ba %>% 
  group_by(`User ID`,Y_M_D) %>%  
  arrange(desc(`date taken`)) %>%
  slice(1)

# Survey D
wash_survey_d_ba$Y_M_D <- format(as.Date(wash_survey_d_ba$`date taken`), "%Y-%m-%d")

wash_survey_d_cleaned <- 
  wash_survey_d_ba %>% 
  group_by(`User ID`,Y_M_D) %>%  
  arrange(desc(`date taken`)) %>%
  slice(1)

# Survey E
wash_survey_e_ba$Y_M_D <- format(as.Date(wash_survey_e_ba$`date taken`), "%Y-%m-%d")

wash_survey_e_cleaned <- 
  wash_survey_e_ba %>% 
  group_by(`User ID`,Y_M_D) %>%  
  arrange(desc(`date taken`)) %>%
  slice(1)

# Survey F
wash_survey_f_ba$Y_M_D <- format(as.Date(wash_survey_f_ba$`date taken`), "%Y-%m-%d")

wash_survey_f_cleaned <- 
  wash_survey_f_ba %>% 
  group_by(`User ID`,Y_M_D) %>%  
  arrange(desc(`date taken`)) %>%
  slice(1)

# Survey G
wash_survey_g_ba$Y_M_D <- format(as.Date(wash_survey_g_ba$`date taken`), "%Y-%m-%d")

wash_survey_g_cleaned <- 
  wash_survey_g_ba %>% 
  group_by(`User ID`,Y_M_D) %>%  
  arrange(desc(`date taken`)) %>%
  slice(1)

# filter out duplicated/triplicated entries in SurveyABCDEFG
wash_survey_a_cleaned <- unique(wash_survey_a_cleaned)
wash_survey_b_cleaned <- unique(wash_survey_b_cleaned)
wash_survey_c_cleaned <- unique(wash_survey_c_cleaned)
wash_survey_d_cleaned <- unique(wash_survey_d_cleaned)
wash_survey_e_cleaned <- unique(wash_survey_e_cleaned)
wash_survey_f_cleaned <- unique(wash_survey_f_cleaned)
wash_survey_g_cleaned <- unique(wash_survey_g_cleaned)
wash_survey_end_of_study_cleaned_duprm <- unique(wash_survey_end_of_study_cleaned_duprm)

# set date taken as date variable
wash_survey_a_cleaned$`date taken` <- as.Date(wash_survey_a_cleaned$`date taken`)
wash_survey_b_cleaned$`date taken` <- as.Date(wash_survey_b_cleaned$`date taken`)
wash_survey_c_cleaned$`date taken` <- as.Date(wash_survey_c_cleaned$`date taken`)
wash_survey_d_cleaned$`date taken` <- as.Date(wash_survey_d_cleaned$`date taken`)
wash_survey_e_cleaned$`date taken` <- as.Date(wash_survey_e_cleaned$`date taken`)
wash_survey_f_cleaned$`date taken` <- as.Date(wash_survey_f_cleaned$`date taken`)
wash_survey_g_cleaned$`date taken` <- as.Date(wash_survey_g_cleaned$`date taken`)
wash_survey_end_of_study_cleaned_duprm$`date taken` <- as.Date(wash_survey_end_of_study_cleaned_duprm$`date taken`)

# set missing to NA
wash_survey_a_cleaned[wash_survey_a_cleaned == ""] <- NA
wash_survey_b_cleaned[wash_survey_b_cleaned == ""] <- NA
wash_survey_c_cleaned[wash_survey_c_cleaned == ""] <- NA
wash_survey_d_cleaned[wash_survey_d_cleaned == ""] <- NA
wash_survey_e_cleaned[wash_survey_e_cleaned == ""] <- NA
wash_survey_f_cleaned[wash_survey_f_cleaned == ""] <- NA
wash_survey_g_cleaned[wash_survey_g_cleaned == ""] <- NA
wash_survey_end_of_study_cleaned_duprm[wash_survey_end_of_study_cleaned_duprm == ""] <- NA
