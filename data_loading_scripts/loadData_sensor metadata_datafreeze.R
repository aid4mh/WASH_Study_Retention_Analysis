## To be able to run this script you will need to open "bad_actor_Jan2022.rds" in the same folder

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
all_metadata <- loadSynapseData('syn27363919')

# create one data freeze variable and one data variable to filter out test data
cutoff1 <- as.Date("2022-02-09")
cutoff2 <- as.Date("2020-03-16")

# subset dataset by data freeze and test data cutoff time
all_metadata$Record_creation_ts <- as.Date(all_metadata$Record_creation_ts, "%Y-%m-%d")
all_metadata <- subset(all_metadata, Record_creation_ts <= cutoff1)
all_metadata <- subset(all_metadata, Record_creation_ts >= cutoff2)

# filter out bad actors
# first replace single quote with double quote in bad_actor_Jan2022.rds
bad_actor_Jan2022$V1 <- gsub("\'","\"", bad_actor_Jan2022$V1)

# exclude bad actor records from all surveys
all_metadata <- subset(all_metadata, !(`User ID` %in% c(bad_actor_Jan2022)))

# filter out duplicated entries in metadata
all_metadata_cleaned <- unique(all_metadata)
