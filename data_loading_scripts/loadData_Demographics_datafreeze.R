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

# WASH Project Baseline Survey 
wash_survey_baseline <- loadSynapseData('syn26715757')

# Make third row to be header
# Rationale: the first 3 rows contain different lvl of detail of a question
# First row: question UUID
# Second row: original question --> too long for a header
# Third row: shortened question with key words only
# step 1: Copy third row to header:
colnames(wash_survey_baseline) <- as.character(wash_survey_baseline[3, ])  

# step 2: delete rows that contain unnecessary header info
wash_survey_baseline <- wash_survey_baseline[-c(1,2,3),]

# step 3: 
# column headers of 32 and 39 are the same, differentiate them 
colnames(wash_survey_baseline)[32] <- "active duty active combat"
colnames(wash_survey_baseline)[39] <- "veteran active combat"

# step 4: fill in colnames for column 2, 3 and 4
colnames(wash_survey_baseline)[2] <- "User ID"
colnames(wash_survey_baseline)[3] <- "date taken"
colnames(wash_survey_baseline)[4] <- "SID"

# create one data freeze variable and one data variable to filter out test data
cutoff1 <- as.Date("2022-02-09")
cutoff2 <- as.Date("2020-03-16")

# subset dataset by data freeze and test data cutoff time
wash_survey_baseline <- subset(wash_survey_baseline, `date taken` <= cutoff1)
wash_survey_baseline <- subset(wash_survey_baseline, `date taken` >= cutoff2)

# filter out bad actors
# first replace single quote with double quote in bad_actor_Jan2022.rds
bad_actor_Jan2022$V1 <- gsub("\'","\"", bad_actor_Jan2022$V1)

# exclude bad actor records from baseline survey
wash_survey_baseline_ba <- subset(wash_survey_baseline, !(`User ID`  %in% c(bad_actor_Jan2022)))

## Data cleaning Function #########################
# remove empty columns for baseline survey
wash_survey_baseline_ba <- subset(wash_survey_baseline_ba, 
                                       select = -which(colnames(wash_survey_baseline_ba)%in% 
                                                         c("1","NA", "0", "nan",NA, NaN,"")))

# if multiple entries submitted by same participant in baseline survey
# strategy to select one submission: TBD
wash_survey_baseline_cleaned_duprm <- 
  wash_survey_baseline_ba %>% 
   group_by(`User ID` ) %>%  
   arrange(desc(`date taken`)) %>%
   slice(1)

# filter out duplicated/triplicated entries in baseline
wash_survey_baseline_cleaned_duprm <- unique(wash_survey_baseline_cleaned_duprm)

# set date taken as date variable
wash_survey_baseline_cleaned_duprm$`date taken` <- as.Date(wash_survey_baseline_cleaned_duprm$`date taken`)

# set missing to NA
wash_survey_baseline_cleaned_duprm[wash_survey_baseline_cleaned_duprm == ""] <- NA
