
###########################################
##SETUP
rm(list=ls())
options(stringsAsFactors = F)
#install.packages("install.load")
library("install.load")
library("tidyverse")
install_load("factoextra")
install_load("reshape2")
install_load("tableone")
install_load("broom")
install_load("report")
############################################


### Load Data 
passive.data.sharing.status <- readRDS("tmp_data/tbl_common passive data sharing.rds") ### Shared by Sophia 
passive.data.sharing.status["participantId"] = passive.data.sharing.status$`User ID`
passive.data.sharing.status$`User ID` <- NULL
dim(passive.data.sharing.status)

demogs<- readRDS("tmp_data/tbl_demographic for participants shared passive data.rds")
demogs["participantId"] = demogs$`User ID`
demogs$`User ID` <- NULL
dim(demogs)

demogs <- dplyr::full_join(
  demogs,
  passive.data.sharing.status %>% select(phase, participantId, Device_Type))
dim(demogs)


### Derive percent of sensor data shared 
tmp.data <- passive.data.sharing.status %>%
  #select(-shared.Barometer) %>%
  mutate(numSensorShared = rowSums( across(starts_with("shared") ) ),
         percentShared = numSensorShared / 8, 
         shared25Percent = ifelse(percentShared >= 0.25, 'yes', 'no'),
         shared50Percent = ifelse(percentShared >= 0.50, 'yes', 'no'),
         shared75Percent = ifelse(percentShared >= 0.75, 'yes', 'no')) %>%
  full_join(demogs)
dim(tmp.data)

tmp.data[tmp.data == 'NA'] = NA

ggplot(data=tmp.data, aes(x=percentShared)) + geom_density()

#ggplot(data=passive.data.sharing.status, aes(x=Device_Type, y=percentShared)) + geom_boxplot()

#Model SHARED 75 PERCENT OF COMMON SENSORS
logit.model.1 <- glm( as.factor(shared75Percent) ~ phase + Device_Type + race_hispanic + Age + gender  +
                    education_level_recode + income_level_recode, family="binomial", data=tmp.data)

#Model SHARED 50 PERCENT OF COMMON SENSORS
logit.model.2 <- glm( as.factor(shared50Percent) ~ phase + Device_Type + race_hispanic + Age + gender  +
                        education_level_recode + income_level_recode, family="binomial", data=tmp.data)

#Model SHARED 25 PERCENT OF COMMON SENSORS
logit.model.3 <- glm( as.factor(shared25Percent) ~ phase + Device_Type + race_hispanic + Age + gender  +
                        education_level_recode + income_level_recode, family="binomial", data=tmp.data)


colnames(logit.model.1.df)

logit.model.1.df <- tidy(logit.model.1, conf.int = T, exponentiate = T, std.error=F) %>%
  mutate(conf.low = round(conf.low, digits=2),
         conf.high = round(conf.high, digits=2),
         estimate = round(estimate, digits=2)) %>%
  mutate(conf.int = paste0(conf.low, " - ", conf.high)) %>%
  select(-conf.low, -conf.high)


logit.model.2.df <- tidy(logit.model.2, conf.int = T, exponentiate = T) %>%
  mutate(conf.low = round(conf.low, digits=2),
         conf.high = round(conf.high, digits=2),
         estimate = round(estimate, digits=2)) %>%
  mutate(conf.int = paste0(conf.low, " - ", conf.high)) %>%
  select(-conf.low, -conf.high)

logit.model.3.df <- tidy(logit.model.3, conf.int = T, exponentiate = T)  %>%
  mutate(conf.low = round(conf.low, digits=2),
         conf.high = round(conf.high, digits=2),
         estimate = round(estimate, digits=2)) %>%
  mutate(conf.int = paste0(conf.low, " - ", conf.high)) %>%
  select(-conf.low, -conf.high)


report(logit.model.3)

write_csv(logit.model.1.df, file = "passive_data_75_shared.csv")
write_csv(logit.model.2.df, file = "passive_data_50_shared.csv")
write_csv(logit.model.3.df, file = "passive_data_25_shared.csv")


### Checking multicollinearity - https://www.statology.org/variance-inflation-factor-r/ 
car::vif(logit.model.1)
car::vif(logit.model.2)
car::vif(logit.model.3)
# vif.values
# vif.values[,1]
# barplot(vif.values[,1], main = "VIF Values", horiz = F, col = "steelblue")




