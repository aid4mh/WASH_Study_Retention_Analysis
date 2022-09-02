## Important dataframe used for retention analysis: 
## 1) wash_survey_baseline_cleaned
## 2) wash_survey_a_cleaned and same for survey BCDEFG, EOS

## Packages ########################################
library(magrittr)
library(scales)
library(survival)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(survminer)
library(Rcpp)

## Run script ######################################
# Missingness sensitivity analysis for baseline characteristics
wash_survey_baseline_cleaned_duprm_sentivity <- subset(wash_survey_baseline_merged, select = c(`User ID`,
                                                                                               age,
                                                                                               gender,
                                                                                               race,
                                                                                               Hispanic,
                                                                                               maritial_status,
                                                                                               income_level,
                                                                                               `education_leve;`,
                                                                                               currently_in_military,
                                                                                               veteran,
                                                                                               level_of_physical_activity,
                                                                                               always_have_acccess_to_phone,
                                                                                               pos_panas_score,
                                                                                               neg_panas_score))

# create variable for Age data existed or no
wash_survey_baseline_cleaned_duprm_sentivity$Age_exist <- "No"
wash_survey_baseline_cleaned_duprm_sentivity$Age_exist[!is.na(wash_survey_baseline_cleaned_duprm_sentivity$age)] <- "Yes"

# Gender
wash_survey_baseline_cleaned_duprm_sentivity$Gender_exist <- "No"
wash_survey_baseline_cleaned_duprm_sentivity$Gender_exist[!is.na(wash_survey_baseline_cleaned_duprm_sentivity$gender)] <- "Yes"

# Race
wash_survey_baseline_cleaned_duprm_sentivity$Race_exist <- "No"
wash_survey_baseline_cleaned_duprm_sentivity$Race_exist[!is.na(wash_survey_baseline_cleaned_duprm_sentivity$race)] <- "Yes"

# Hispanic
wash_survey_baseline_cleaned_duprm_sentivity$Hispanic_exist <- "No"
wash_survey_baseline_cleaned_duprm_sentivity$Hispanic_exist[!is.na(wash_survey_baseline_cleaned_duprm_sentivity$Hispanic)] <- "Yes"

# Maritial status
wash_survey_baseline_cleaned_duprm_sentivity$Maritial_exist <- "No"
wash_survey_baseline_cleaned_duprm_sentivity$Maritial_exist[!is.na(wash_survey_baseline_cleaned_duprm_sentivity$maritial_status)] <- "Yes"

# Income level
wash_survey_baseline_cleaned_duprm_sentivity$Income_exist <- "No"
wash_survey_baseline_cleaned_duprm_sentivity$Income_exist[!is.na(wash_survey_baseline_cleaned_duprm_sentivity$income_level)] <- "Yes"

# Education level
wash_survey_baseline_cleaned_duprm_sentivity$Edu_exist <- "No"
wash_survey_baseline_cleaned_duprm_sentivity$Edu_exist[!is.na(wash_survey_baseline_cleaned_duprm_sentivity$`education_leve;`)] <- "Yes"

# Active duty
wash_survey_baseline_cleaned_duprm_sentivity$Military_exist <- "No"
wash_survey_baseline_cleaned_duprm_sentivity$Military_exist[!is.na(wash_survey_baseline_cleaned_duprm_sentivity$currently_in_military)] <- "Yes"

# Veteran
wash_survey_baseline_cleaned_duprm_sentivity$Veteran_exist <- "No"
wash_survey_baseline_cleaned_duprm_sentivity$Veteran_exist[!is.na(wash_survey_baseline_cleaned_duprm_sentivity$veteran)] <- "Yes"

# Physical activity
wash_survey_baseline_cleaned_duprm_sentivity$Physical_exist <- "No"
wash_survey_baseline_cleaned_duprm_sentivity$Physical_exist[!is.na(wash_survey_baseline_cleaned_duprm_sentivity$level_of_physical_activity)] <- "Yes"

# Always have acsess to phone
wash_survey_baseline_cleaned_duprm_sentivity$Access_exist <- "No"
wash_survey_baseline_cleaned_duprm_sentivity$Access_exist[!is.na(wash_survey_baseline_cleaned_duprm_sentivity$always_have_acccess_to_phone)] <- "Yes"

# Positive PANAS score
wash_survey_baseline_cleaned_duprm_sentivity$PosPanas_exist <- "No"
wash_survey_baseline_cleaned_duprm_sentivity$PosPanas_exist[!is.na(wash_survey_baseline_cleaned_duprm_sentivity$pos_panas_score)] <- "Yes"
wash_survey_baseline_cleaned_duprm_sentivity$PosPanas_exist[wash_survey_baseline_cleaned_duprm_sentivity$pos_panas_score == "incomplete"] <- "No"

# Negative PANAS score
wash_survey_baseline_cleaned_duprm_sentivity$NegPanas_exist <- "No"
wash_survey_baseline_cleaned_duprm_sentivity$NegPanas_exist[!is.na(wash_survey_baseline_cleaned_duprm_sentivity$neg_panas_score)] <- "Yes"
wash_survey_baseline_cleaned_duprm_sentivity$NegPanas_exist[wash_survey_baseline_cleaned_duprm_sentivity$neg_panas_score == "incomplete"] <- "No"

# merge the above dataset with survival infomation
baseline_sensitivity <- merge(all_survey_last_date, wash_survey_baseline_cleaned_duprm_sentivity)

# create KM curves
# Age
km_fit_sensitivity_age <- survfit(Surv(duration_in_Study, censorStatus) ~ Age_exist, data = baseline_sensitivity)

p.sensitivity.age <- ggsurvplot(km_fit_sensitivity_age,
                       data = baseline_sensitivity, 
                      # risk.table = TRUE,
                       surv.median.line = "hv",
                       pval = TRUE,
                       xlab="Time in Days",
                       xlim = c(0, 84),
                       break.x.by = 7)

summary(km_fit_sensitivity_age, times = c(84))
summary(km_fit_sensitivity_age)$table

# Gender
km_fit_sensitivity_gender <- survfit(Surv(duration_in_Study, censorStatus) ~ Gender_exist, data = baseline_sensitivity)
p.sensitivity.gender <- ggsurvplot(km_fit_sensitivity_gender,
                                data = baseline_sensitivity, 
                                # risk.table = TRUE,
                                surv.median.line = "hv",
                                pval = TRUE,
                                xlab="Time in Days",
                                xlim = c(0, 84),
                                break.x.by = 7)

summary(km_fit_sensitivity_gender, times = c(84))
summary(km_fit_sensitivity_gender)$table


# Race
km_fit_sensitivity_race <- survfit(Surv(duration_in_Study, censorStatus) ~ Race_exist, data = baseline_sensitivity)
p.sensitivity.race <- ggsurvplot(km_fit_sensitivity_race,
                                   data = baseline_sensitivity, 
                                   # risk.table = TRUE,
                                   surv.median.line = "hv",
                                   pval = TRUE,
                                   xlab="Time in Days",
                                   xlim = c(0, 84),
                                   break.x.by = 7)

summary(km_fit_sensitivity_race, times = c(84))
summary(km_fit_sensitivity_race)$table


# Race
km_fit_sensitivity_race <- survfit(Surv(duration_in_Study, censorStatus) ~ Race_exist, data = baseline_sensitivity)
p.sensitivity.race <- ggsurvplot(km_fit_sensitivity_race,
                                 data = baseline_sensitivity, 
                                 # risk.table = TRUE,
                                 surv.median.line = "hv",
                                 pval = TRUE,
                                 xlab="Time in Days",
                                 xlim = c(0, 84),
                                 break.x.by = 7)

summary(km_fit_sensitivity_race, times = c(84))
summary(km_fit_sensitivity_race)$table

# Hispanic
km_fit_sensitivity_hispanic <- survfit(Surv(duration_in_Study, censorStatus) ~ Hispanic_exist, data = baseline_sensitivity)

p.sensitivity.hispanic <- ggsurvplot(km_fit_sensitivity_hispanic,
                                 data = baseline_sensitivity, 
                                 # risk.table = TRUE,
                                 surv.median.line = "hv",
                                 pval = TRUE,
                                 xlab="Time in Days",
                                 xlim = c(0, 84),
                                 break.x.by = 7)

summary(km_fit_sensitivity_hispanic, times = c(84))
summary(km_fit_sensitivity_hispanic)$table


# Maritial Status
km_fit_sensitivity_maritial <- survfit(Surv(duration_in_Study, censorStatus) ~ Maritial_exist, data = baseline_sensitivity)
p.sensitivity.maritial <- ggsurvplot(km_fit_sensitivity_maritial,
                                     data = baseline_sensitivity, 
                                     # risk.table = TRUE,
                                     surv.median.line = "hv",
                                     pval = TRUE,
                                     xlab="Time in Days",
                                     xlim = c(0, 84),
                                     break.x.by = 7)

summary(km_fit_sensitivity_maritial, times = c(84))
summary(km_fit_sensitivity_maritial)$table

# Income level
km_fit_sensitivity_income <- survfit(Surv(duration_in_Study, censorStatus) ~ Income_exist, data = baseline_sensitivity)
p.sensitivity.income <- ggsurvplot(km_fit_sensitivity_income,
                                     data = baseline_sensitivity, 
                                     # risk.table = TRUE,
                                     surv.median.line = "hv",
                                     pval = TRUE,
                                     xlab="Time in Days",
                                     xlim = c(0, 84),
                                     break.x.by = 7)

summary(km_fit_sensitivity_income, times = c(84))
summary(km_fit_sensitivity_income)$table

# Education level
km_fit_sensitivity_edu <- survfit(Surv(duration_in_Study, censorStatus) ~ Edu_exist, data = baseline_sensitivity)
p.sensitivity.edu <- ggsurvplot(km_fit_sensitivity_edu,
                                   data = baseline_sensitivity, 
                                   # risk.table = TRUE,
                                   surv.median.line = "hv",
                                   pval = TRUE,
                                   xlab="Time in Days",
                                   xlim = c(0, 84),
                                   break.x.by = 7)

summary(km_fit_sensitivity_edu, times = c(84))
summary(km_fit_sensitivity_edu)$table


# Active duty
km_fit_sensitivity_military <- survfit(Surv(duration_in_Study, censorStatus) ~ Military_exist, data = baseline_sensitivity)

p.sensitivity.military <- ggsurvplot(km_fit_sensitivity_military,
                                data = baseline_sensitivity, 
                                # risk.table = TRUE,
                                surv.median.line = "hv",
                                pval = TRUE,
                                xlab="Time in Days",
                                xlim = c(0, 84),
                                break.x.by = 7)

summary(km_fit_sensitivity_military, times = c(84))
summary(km_fit_sensitivity_military)$table


# Veteran
km_fit_sensitivity_veteran <- survfit(Surv(duration_in_Study, censorStatus) ~ Veteran_exist, data = baseline_sensitivity)
p.sensitivity.veteran <- ggsurvplot(km_fit_sensitivity_veteran,
                                     data = baseline_sensitivity, 
                                     # risk.table = TRUE,
                                     surv.median.line = "hv",
                                     pval = TRUE,
                                     xlab="Time in Days",
                                     xlim = c(0, 84),
                                     break.x.by = 7)

summary(km_fit_sensitivity_veteran, times = c(84))
summary(km_fit_sensitivity_veteran)$table

# Physical activity
km_fit_sensitivity_physical <- survfit(Surv(duration_in_Study, censorStatus) ~ Physical_exist, data = baseline_sensitivity)

p.sensitivity.physical <- ggsurvplot(km_fit_sensitivity_physical,
                                    data = baseline_sensitivity, 
                                    # risk.table = TRUE,
                                    surv.median.line = "hv",
                                    pval = TRUE,
                                    xlab="Time in Days",
                                    xlim = c(0, 84),
                                    break.x.by = 7)

summary(km_fit_sensitivity_physical, times = c(84))
summary(km_fit_sensitivity_physical)$table

# Always have access to phones
km_fit_sensitivity_access <- survfit(Surv(duration_in_Study, censorStatus) ~ Access_exist, data = baseline_sensitivity)

p.sensitivity.access <- ggsurvplot(km_fit_sensitivity_access,
                                     data = baseline_sensitivity, 
                                     # risk.table = TRUE,
                                     surv.median.line = "hv",
                                     pval = TRUE,
                                     xlab="Time in Days",
                                     xlim = c(0, 84),
                                     break.x.by = 7)

summary(km_fit_sensitivity_access, times = c(84))
summary(km_fit_sensitivity_access)$table

# Positive PANAS score
km_fit_sensitivity_pospanas <- survfit(Surv(duration_in_Study, censorStatus) ~ PosPanas_exist, data = baseline_sensitivity)
p.sensitivity.pospanas <- ggsurvplot(km_fit_sensitivity_pospanas,
                                   data = baseline_sensitivity, 
                                   # risk.table = TRUE,
                                   surv.median.line = "hv",
                                   pval = TRUE,
                                   xlab="Time in Days",
                                   xlim = c(0, 84),
                                   break.x.by = 7)

summary(km_fit_sensitivity_pospanas, times = c(84))
summary(km_fit_sensitivity_pospanas)$table

# Negative PANAS score
km_fit_sensitivity_negpanas <- survfit(Surv(duration_in_Study, censorStatus) ~ NegPanas_exist, data = baseline_sensitivity)

p.sensitivity.negpanas <- ggsurvplot(km_fit_sensitivity_negpanas,
                                     data = baseline_sensitivity, 
                                     # risk.table = TRUE,
                                     surv.median.line = "hv",
                                     pval = TRUE,
                                     xlab="Time in Days",
                                     xlim = c(0, 84),
                                     break.x.by = 7)

summary(km_fit_sensitivity_negpanas, times = c(84))
summary(km_fit_sensitivity_negpanas)$table


# Survival curve for all survey combined for full cohort
# get number of users who leave within the 84 days 
sum(all_survey_last_date$duration_in_Study < 84, na.rm = T)
hist(all_survey_last_date$duration_in_Study)
summary(all_survey_last_date$duration_in_Study)

# create censor variable
# censor variable 0 = alive if >= 84 days in the app OR 1 = dead 
all_survey_last_date <- all_survey_last_date %>% 
  mutate(censorStatus = ifelse(duration_in_Study < 84, 1, 0))

# Kaplan Meier Survival Curve
km <- with(all_survey_last_date, Surv(duration_in_Study, censorStatus))
km_fit <- survfit(Surv(duration_in_Study, censorStatus) ~ 1, data=all_survey_last_date)

ggsurvplot(km_fit, 
           data = all_survey_last_date, 
           risk.table = TRUE,
           xlab="Time in Days")

summary(km_fit)$table

# get retention rate at timepoint of interest
summary(km_fit, times = c(1,7,14,30,60,84))


# Survival curve by survey type
# first subset duration in study by survey type
all_survey_last_date_type <- 
  all_survey %>% 
  group_by(`User ID`, Survey_Type) %>%  
  arrange(desc(`date taken`)) %>%
  slice(1)

# create censor variable
# censor variable 0 = alive if >= 84 days in the app OR 1 = dead 
all_survey_last_date_type <- all_survey_last_date_type %>% 
  mutate(censorStatus = ifelse(Day < 84, 1, 0))

# plot curve
km_fit_type <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ Survey_Type, data = all_survey_last_date_type)
ggsurvplot(km_fit_type, 
           data = all_survey_last_date, 
           conf.int = TRUE,
           risk.table = TRUE,
           pval = TRUE,  
           xlab="Time in Days")

summary(km_fit_type)$table

# test survival curve differences 
fit.test.type <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ Survey_Type, data = all_survey_last_date_type)
fit.test.type

# Survival curve for ppl who submitted sensor passive data
# get number of users who leave within the  days 
sum(all_metadata_day_in_study$Day < 84, na.rm = T)
sum(all_metadata_day_in_study$Day >= 84, na.rm = T)
hist(all_metadata_day_in_study$Day)
summary(all_metadata_day_in_study$Day)

# create censor variable
# censor variable 0 = alive if >= 84 days in the app OR 1 = dead 
all_metadata_day_in_study <- all_metadata_day_in_study %>% 
  mutate(censorStatus = ifelse(Day < 84, 1, 0))

# Kaplan Meier Survival Curve
km <- with(all_metadata_day_in_study, Surv(Day, censorStatus))
km_fit <- survfit(Surv(Day, censorStatus) ~ 1, data=all_metadata_day_in_study)
km_fit

ggsurvplot(km_fit, 
           data = all_metadata_day_in_study, 
           risk.table = TRUE,
           surv.median.line = "hv",
           xlab="Time in Days-Sensor")

summary(km_fit)$table

# get retention rate at timepoint of interest
summary(km_fit, times = c(1,7,14,30,60,84))


# Survival curve for ppl who submitted at least one survey or sensor passive data for full cohort
all_survey_last_date_sub <- subset(all_survey_last_date, select = c("User ID", "duration_in_Study", "censorStatus"))
all_metadata_day_in_study_sub <- subset(all_metadata_day_in_study, select = c("User ID","Day", "censorStatus"))
day_in_study_all <- merge(all_metadata_day_in_study_sub, all_survey_last_date_sub, by = "User ID", all = T)

# select the max duration in study
day_in_study_all$Retained_longer[day_in_study_all$Day > day_in_study_all$duration_in_Study] <- "Sensor"
day_in_study_all$Retained_longer[is.na(day_in_study_all$duration_in_Study)] <- "Sensor"

day_in_study_all$Retained_longer[day_in_study_all$Day < day_in_study_all$duration_in_Study] <- "Survey"
day_in_study_all$Retained_longer[is.na(day_in_study_all$Day)] <- "Survey"

day_in_study_all$Retained_longer[day_in_study_all$Day == day_in_study_all$duration_in_Study] <- "Same"

day_in_study_all$censorStatus[day_in_study_all$Retained_longer == "Sensor"] <- day_in_study_all$censorStatus.x[which(day_in_study_all$Retained_longer == "Sensor")]
day_in_study_all$censorStatus[day_in_study_all$Retained_longer == "Survey"] <- day_in_study_all$censorStatus.y[which(day_in_study_all$Retained_longer == "Survey")]
day_in_study_all$censorStatus[day_in_study_all$Retained_longer == "Same"] <- day_in_study_all$censorStatus.x[which(day_in_study_all$Retained_longer == "Same")]

day_in_study_all$duration_max[day_in_study_all$Retained_longer == "Sensor"] <- day_in_study_all$Day[which(day_in_study_all$Retained_longer == "Sensor")]
day_in_study_all$duration_max[day_in_study_all$Retained_longer == "Survey"] <- day_in_study_all$duration_in_Study[which(day_in_study_all$Retained_longer == "Survey")]
day_in_study_all$duration_max[day_in_study_all$Retained_longer == "Same"] <- day_in_study_all$duration_in_Study[which(day_in_study_all$Retained_longer == "Same")]

# plot 3 curves on the same plot
# survey only
all_survey_last_date_sub <- subset(all_survey_last_date, select = c("User ID", "duration_in_Study", "censorStatus"))
all_survey_last_date_sub$Type <- "Survey"
all_survey_last_date_sub$duration_in_Study.survey <- all_survey_last_date_sub$duration_in_Study

# sensor only
all_metadata_day_in_study_sub <- subset(all_metadata_day_in_study, select = c("User ID","Day","censorStatus"))
colnames(all_metadata_day_in_study_sub)[2] <- "duration_in_Study"
all_metadata_day_in_study_sub$Type <- "Sensor"
all_metadata_day_in_study_sub$duration_in_Study.sensor <- all_metadata_day_in_study_sub$duration_in_Study

# survey and sensor combined, depending on the max duration in study
day_in_study_all_sub <- subset(day_in_study_all, select = c("User ID","duration_max","censorStatus"))
colnames(day_in_study_all_sub)[2] <- "duration_in_Study"
day_in_study_all_sub$Type <- "Combined"

survcurve_all <- rbind(all_survey_last_date_sub, all_metadata_day_in_study_sub)
survcurve_all <- rbind(survcurve_all, day_in_study_all_sub)

# survival rate at Day 84
km_fit_all <- survfit(Surv(duration_in_Study, censorStatus) ~ Type, data = survcurve_all)
p.all.84 <- ggsurvplot(km_fit_all,
                       data = survcurve_all, 
                       risk.table = TRUE,
                       surv.median.line = "hv",
                       xlab="Time in Days",
                       xlim = c(0, 84),
                       break.x.by = 7)

p.all.84 <- p.all.84$plot + 
  scale_color_manual(values = c("#2b8cbe", "#e66101", "#5e3c99"))

summary(km_fit_all, times = c(84))
summary(km_fit_all)$table

# create censor variable
# censor variable 0 = alive if >= 98 days in the app OR 1 = dead 
all_survey_last_date <- all_survey_last_date %>% 
  mutate(censorStatus = ifelse(duration_in_Study < 98, 1, 0))

all_metadata_day_in_study <- all_metadata_day_in_study %>% 
  mutate(censorStatus = ifelse(Day < 98, 1, 0))

# Survival curve for ppl who submitted at least one survey or sensor passive data for full cohort
all_survey_last_date_sub <- subset(all_survey_last_date, select = c("User ID", "duration_in_Study", "censorStatus"))
all_metadata_day_in_study_sub <- subset(all_metadata_day_in_study, select = c("User ID","Day", "censorStatus"))
day_in_study_all <- merge(all_metadata_day_in_study_sub, all_survey_last_date_sub, by = "User ID", all = T)

# select the max duration in study
day_in_study_all$Retained_longer[day_in_study_all$Day > day_in_study_all$duration_in_Study] <- "Sensor"
day_in_study_all$Retained_longer[is.na(day_in_study_all$duration_in_Study)] <- "Sensor"

day_in_study_all$Retained_longer[day_in_study_all$Day < day_in_study_all$duration_in_Study] <- "Survey"
day_in_study_all$Retained_longer[is.na(day_in_study_all$Day)] <- "Survey"

day_in_study_all$Retained_longer[day_in_study_all$Day == day_in_study_all$duration_in_Study] <- "Same"

day_in_study_all$censorStatus[day_in_study_all$Retained_longer == "Sensor"] <- day_in_study_all$censorStatus.x[which(day_in_study_all$Retained_longer == "Sensor")]
day_in_study_all$censorStatus[day_in_study_all$Retained_longer == "Survey"] <- day_in_study_all$censorStatus.y[which(day_in_study_all$Retained_longer == "Survey")]
day_in_study_all$censorStatus[day_in_study_all$Retained_longer == "Same"] <- day_in_study_all$censorStatus.x[which(day_in_study_all$Retained_longer == "Same")]

day_in_study_all$duration_max[day_in_study_all$Retained_longer == "Sensor"] <- day_in_study_all$Day[which(day_in_study_all$Retained_longer == "Sensor")]
day_in_study_all$duration_max[day_in_study_all$Retained_longer == "Survey"] <- day_in_study_all$duration_in_Study[which(day_in_study_all$Retained_longer == "Survey")]
day_in_study_all$duration_max[day_in_study_all$Retained_longer == "Same"] <- day_in_study_all$duration_in_Study[which(day_in_study_all$Retained_longer == "Same")]

# plot 3 curves on the same plot
# survey only
all_survey_last_date_sub <- subset(all_survey_last_date, select = c("User ID", "duration_in_Study", "censorStatus"))
all_survey_last_date_sub$Type <- "Survey"
#all_survey_last_date_sub$duration_in_Study.survey <- all_survey_last_date_sub$duration_in_Study

# sensor only
all_metadata_day_in_study_sub <- subset(all_metadata_day_in_study, select = c("User ID","Day","censorStatus"))
colnames(all_metadata_day_in_study_sub)[2] <- "duration_in_Study"
all_metadata_day_in_study_sub$Type <- "Sensor"
#all_metadata_day_in_study_sub$duration_in_Study.sensor <- all_metadata_day_in_study_sub$duration_in_Study

# survey and sensor combined, depending on the max duration in study
day_in_study_all_sub <- subset(day_in_study_all, select = c("User ID","duration_max","censorStatus"))
colnames(day_in_study_all_sub)[2] <- "duration_in_Study"
day_in_study_all_sub$Type <- "Combined"

survcurve_all2 <- rbind(all_survey_last_date_sub, all_metadata_day_in_study_sub)
survcurve_all2 <- rbind(survcurve_all2, day_in_study_all_sub)

km_fit_all2 <- survfit(Surv(duration_in_Study, censorStatus) ~ Type, data = survcurve_all2)

p.all.98 <- ggsurvplot(km_fit_all2, 
           data = survcurve_all2, 
           risk.table = TRUE,
           surv.median.line = "hv",
           xlab="Time in Days",
           xlim = c(0, 98),
           break.x.by = 7)

p.all.98 <- p.all.98$plot + 
  scale_color_manual(values = c("#2b8cbe", "#e66101", "#5e3c99"))

summary(km_fit_all2, times = c(98))
summary(km_fit_all2)$table


# create survive curve with survey and sensor
survcurve_both <- rbind(all_survey_last_date_sub, all_metadata_day_in_study_sub)

km_fit_both <- survfit(Surv(duration_in_Study, censorStatus) ~ Type, data = survcurve_both)
p.both.84 <- ggsurvplot(km_fit_both,
                       data = survcurve_both, 
                       #risk.table = TRUE,
                       surv.median.line = "hv",
                       xlab="Time in Days",
                       xlim = c(0, 84),
                       break.x.by = 7)

p.both.84 <- p.both.84$plot + 
  scale_color_manual(values = c("#2b8cbe", "#e66101"))

summary(km_fit_both, times = c(84))
summary(km_fit_both)$table

# create survive curve for ppl recruited before and after relaunch
survcurve_both_before <- subset(survcurve_both, `User ID` %in% day_in_study_all_before$`User ID`)

# before relaunch
km_fit_both_before <- survfit(Surv(duration_in_Study, censorStatus) ~ Type, data = survcurve_both_before)
p.both.before.84 <- ggsurvplot(km_fit_both_before,
                        data = survcurve_both_before, 
                        #risk.table = TRUE,
                        surv.median.line = "hv",
                        xlab="Time in Days",
                        xlim = c(0, 84),
                        break.x.by = 7)

p.both.before.84 <- p.both.before.84$plot + 
  scale_color_manual(values = c("#2b8cbe", "#e66101"))

summary(km_fit_both_before, times = c(84))
summary(km_fit_both_before)$table


# after relaunch
survcurve_both_after <- subset(survcurve_both, `User ID` %in% day_in_study_all_after$`User ID`)

km_fit_both_after <- survfit(Surv(duration_in_Study, censorStatus) ~ Type, data = survcurve_both_after)
p.both.after.84 <- ggsurvplot(km_fit_both_after,
                        data = survcurve_both_after, 
                        #risk.table = TRUE,
                        surv.median.line = "hv",
                        xlab="Time in Days",
                        xlim = c(0, 84),
                        break.x.by = 7)

p.both.after.84 <- p.both.after.84$plot + 
  scale_color_manual(values = c("#2b8cbe", "#e66101"))

summary(km_fit_both_after, times = c(84))
summary(km_fit_both_after)$table


# produce survival curve for only those completed baseline survey 
survcurve_both_before_baseline <- subset(survcurve_both_before, `User ID` %in% wash_survey_baseline_cleaned_duprm$`User ID`)

survcurve_both_after_baseline <- subset(survcurve_both_after, `User ID` %in% wash_survey_baseline_cleaned_duprm$`User ID`)

# survival rate at Day 84
km_fit_both_after_baseline <- survfit(Surv(duration_in_Study, censorStatus) ~ Type, data = survcurve_both_after_baseline)

p.both.after.84.baseline <- ggsurvplot(km_fit_both_after_baseline,
                                      data = survcurve_both_after_baseline, 
                                      risk.table = TRUE,
                                      surv.median.line = "hv",
                                      xlab="Time in Days",
                                      xlim = c(0, 84),
                                      break.x.by = 7)

p.both.after.84.baseline <- p.both.after.84.baseline$plot + 
  scale_color_manual(values = c("#2b8cbe", "#e66101"))

summary(km_fit_both_after_baseline, times = c(84))
summary(km_fit_both_after_baseline)$table


# survival rate at Day 84
km_fit_both_before_baseline <- survfit(Surv(duration_in_Study, censorStatus) ~ Type, data = survcurve_both_before_baseline)

p.both.before.84.baseline <- ggsurvplot(km_fit_both_before_baseline,
                                       data = survcurve_both_before_baseline, 
                                       risk.table = TRUE,
                                       surv.median.line = "hv",
                                       xlab="Time in Days",
                                       xlim = c(0, 84),
                                       break.x.by = 7)

p.both.before.84.baseline <- p.both.before.84.baseline$plot + 
  scale_color_manual(values = c("#2b8cbe", "#e66101"))

summary(km_fit_both_before_baseline, times = c(84))
summary(km_fit_both_before_baseline)$table

# produce survival curve for only those did not complete baseline survey 
survcurve_both_before_baseline_no <- subset(survcurve_both_before, ! (`User ID` %in% wash_survey_baseline_cleaned_duprm$`User ID`))

survcurve_both_after_baseline_no <- subset(survcurve_both_after, ! (`User ID` %in% wash_survey_baseline_cleaned_duprm$`User ID`))

# survival rate at Day 84
km_fit_both_after_baseline_no <- survfit(Surv(duration_in_Study, censorStatus) ~ Type, data = survcurve_both_after_baseline_no)

p.both.after.84.baseline.no <- ggsurvplot(km_fit_both_after_baseline_no,
                                       data = survcurve_both_after_baseline_no, 
                                       risk.table = TRUE,
                                       surv.median.line = "hv",
                                       xlab="Time in Days",
                                       xlim = c(0, 84),
                                       break.x.by = 7)

p.both.after.84.baseline.no <- p.both.after.84.baseline.no$plot + 
  scale_color_manual(values = c("#2b8cbe", "#e66101"))

summary(km_fit_both_after_baseline_no, times = c(84))
summary(km_fit_both_after_baseline_no)$table


# survival rate at Day 84
km_fit_both_before_baseline_no <- survfit(Surv(duration_in_Study, censorStatus) ~ Type, data = survcurve_both_before_baseline_no)

p.both.before.84.baseline.no <- ggsurvplot(km_fit_both_before_baseline_no,
                                        data = survcurve_both_before_baseline_no, 
                                        risk.table = TRUE,
                                        surv.median.line = "hv",
                                        xlab="Time in Days",
                                        xlim = c(0, 84),
                                        break.x.by = 7)

p.both.before.84.baseline.no <- p.both.before.84.baseline.no$plot + 
  scale_color_manual(values = c("#2b8cbe", "#e66101"))

summary(km_fit_both_before_baseline_no, times = c(84))
summary(km_fit_both_before_baseline_no)$table

# incorporate payment schedule in survival curve - before and after relaunch
p.both.before.84 <- p.both.before.84 + 
  geom_vline(xintercept = c(7,14,21,28,35,42,49,56,63,70,77,84), colour = "purple")

p.both.after.84 <- p.both.after.84 + 
  annotate("rect", fill = "purple", alpha = 0.5, 
         xmin = 21, xmax = 35,
         ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "purple", alpha = 0.5, 
           xmin = 42, xmax = 56,
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "purple", alpha = 0.5, 
            xmin = 63, xmax = 77,
            ymin = -Inf, ymax = Inf) +
  
  annotate("rect", fill = "purple", alpha = 0.5, 
           xmin = 84, xmax = 98,
           ymin = -Inf, ymax = Inf)
  
# survival curve by baseline variables - pre and post relaunch
wash_survey_baseline_merged$income_level_1[wash_survey_baseline_merged$income_level_1 %in% c("More than $150,000",
                                                                                             "$125,000 to $150,000",
                                                                                             "$100,000 to $124,999")] <- "More than $100,000"

wash_survey_baseline_merged$always_have_acccess_to_phone_1[wash_survey_baseline_merged$always_have_acccess_to_phone_1 %in% c("Other",
                                                                                             "other")] <- "Other"



survcurve_baseline <- subset(wash_survey_baseline_merged, select = c(`User ID`,
                                                                     Age_1,
                                                                     gender_1,
                                                                     race_1,
                                                                     Hispanic_1,
                                                                     maritial_status_1,
                                                                     income_level_1,
                                                                     education_level_1,
                                                                     currently_in_military_1,
                                                                     veteran_1,
                                                                     level_of_physical_activity_1,
                                                                     always_have_acccess_to_phone_1,
                                                                     pos_panas_highlow,
                                                                     neg_panas_highlow))

survcurve_baseline <- merge(all_survey_last_date, survcurve_baseline)

# remove/combine levels with less than 5% population
# Gender - remove non-binary
survcurve_baseline$gender_2 <- survcurve_baseline$gender_1
survcurve_baseline$gender_2[survcurve_baseline$gender_2 == "Non-binary"] <- NA

# Race - White vs non white
survcurve_baseline$race_2 <- survcurve_baseline$race_1

survcurve_baseline$race_2[survcurve_baseline$race_2 == "American Indian or Alaska Native"] <- "Non-white"
survcurve_baseline$race_2[survcurve_baseline$race_2 == "Asian"] <- "Non-white"
survcurve_baseline$race_2[survcurve_baseline$race_2 == "Black or African American"] <- "Non-white"
survcurve_baseline$race_2[survcurve_baseline$race_2 == "Native Hawaiian or Other Pacific Islander"] <- "Non-white"
survcurve_baseline$race_2[survcurve_baseline$race_2 == "Other"] <- "Non-white"

# Race - White, asian, black vs others
survcurve_baseline$race_3 <- survcurve_baseline$race_1

survcurve_baseline$race_3[survcurve_baseline$race_3 == "American Indian or Alaska Native"] <- "Other"
survcurve_baseline$race_3[survcurve_baseline$race_3 == "Native Hawaiian or Other Pacific Islander"] <- "Other"
survcurve_baseline$race_3[survcurve_baseline$race_3 == "Other"] <- "Other"


# Hispanic - remove I dont know
survcurve_baseline$Hispanic_2 <- survcurve_baseline$Hispanic_1
survcurve_baseline$Hispanic_2[survcurve_baseline$Hispanic_2 == "I don't know"] <- NA


# Maritial status - married, single, others
survcurve_baseline$maritial_status_2 <- survcurve_baseline$maritial_status_1
survcurve_baseline$maritial_status_2[survcurve_baseline$maritial_status_2 == "Divorced"] <- "Other"
survcurve_baseline$maritial_status_2[survcurve_baseline$maritial_status_2 == "Separated"] <- "Other"
survcurve_baseline$maritial_status_2[survcurve_baseline$maritial_status_2 == "Widowed"] <- "Other"

# Maritial status - married, single, divorced and others
survcurve_baseline$maritial_status_3 <- survcurve_baseline$maritial_status_1
survcurve_baseline$maritial_status_3[survcurve_baseline$maritial_status_3 == "Separated"] <- "Other"
survcurve_baseline$maritial_status_3[survcurve_baseline$maritial_status_3 == "Widowed"] <- "Other"


# Education - college, grad, high school or lower
survcurve_baseline$education_level_2 <- survcurve_baseline$education_level_1
survcurve_baseline$education_level_2[survcurve_baseline$education_level_2 == "High School"] <- "High School and lower"
survcurve_baseline$education_level_2[survcurve_baseline$education_level_2 == "Elementary/Grade School"] <- "High School and lower"
survcurve_baseline$education_level_2[survcurve_baseline$education_level_2 == "Middle School/Junior High"] <- "High School and lower"


# Access to phone- remove Other
survcurve_baseline$always_have_acccess_to_phone_2 <- survcurve_baseline$always_have_acccess_to_phone_1
survcurve_baseline$always_have_acccess_to_phone_2[survcurve_baseline$always_have_acccess_to_phone_2 == "Other"] <- NA

# pre relaunch
survcurve_baseline_before <- subset(survcurve_baseline, `User ID` %in% day_in_study_all_before$`User ID`)

#age
km_fit_age <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ Age_1, data = survcurve_baseline_before)

ggsurvplot(km_fit_age, 
           data = survcurve_baseline_before, 
           conf.int = TRUE,
          # risk.table = TRUE,
           #pval = TRUE,  
          surv.median.line = "hv",
           xlab="Time in Days",
          xlim = c(0, 84),
          break.x.by = 7)

summary(km_fit_age)$table

# test survival curve differences 
fit.test.age <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ Age_1, data = survcurve_baseline_before)
fit.test.age

#gender
km_fit_gender <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ gender_2, data = survcurve_baseline_before)

ggsurvplot(km_fit_gender, 
           data = survcurve_baseline_before, 
           conf.int = TRUE,
           # risk.table = TRUE,
           #pval = TRUE,  
           surv.median.line = "hv",
           xlab="Time in Days",
           xlim = c(0, 84),
           break.x.by = 7)

summary(km_fit_gender)$table

# test survival curve differences 
fit.test.gender <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ gender_1, data = survcurve_baseline_before)
fit.test.gender

#race - v1
km_fit_race <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ race_2, data = survcurve_baseline_before)

ggsurvplot(km_fit_race, 
           data = survcurve_baseline_before, 
           conf.int = TRUE,
           # risk.table = TRUE,
           #pval = TRUE,  
           surv.median.line = "hv",
           xlab="Time in Days",
           xlim = c(0, 84),
           break.x.by = 7)

summary(km_fit_race)$table

# test survival curve differences 
fit.test.race <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ race_1, data = survcurve_baseline_before)
fit.test.race


#race - v2
km_fit_race <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ race_3, data = survcurve_baseline_before)

ggsurvplot(km_fit_race, 
           data = survcurve_baseline_before, 
           conf.int = TRUE,
           # risk.table = TRUE,
           #pval = TRUE,  
           surv.median.line = "hv",
           xlab="Time in Days",
           xlim = c(0, 84),
           break.x.by = 7)

summary(km_fit_race)$table

# test survival curve differences 
fit.test.race <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ race_1, data = survcurve_baseline_before)
fit.test.race

#hispanic
km_fit_hispanic <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ Hispanic_2, data = survcurve_baseline_before)

ggsurvplot(km_fit_hispanic, 
           data = survcurve_baseline_before, 
           conf.int = TRUE,
           # risk.table = TRUE,
           #pval = TRUE,  
           surv.median.line = "hv",
           xlab="Time in Days",
           xlim = c(0, 84),
           break.x.by = 7)

summary(km_fit_hispanic)$table

# test survival curve differences 
fit.test.hispanic <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ Hispanic_1, data = survcurve_baseline_before)
fit.test.hispanic


#maritial - V1
km_fit_maritial <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ maritial_status_2, data = survcurve_baseline_before)

ggsurvplot(km_fit_maritial, 
           data = survcurve_baseline_before, 
           conf.int = TRUE,
           # risk.table = TRUE,
           #pval = TRUE,  
           surv.median.line = "hv",
           xlab="Time in Days",
           xlim = c(0, 84),
           break.x.by = 7) +
  guides(colour = guide_legend(nrow = 2))

summary(km_fit_maritial)$table

# test survival curve differences 
fit.test.maritial <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ maritial_status_1, data = survcurve_baseline_before)
fit.test.maritial


#maritial - V2
km_fit_maritial <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ maritial_status_3, data = survcurve_baseline_before)

ggsurvplot(km_fit_maritial, 
           data = survcurve_baseline_before, 
           conf.int = TRUE,
           # risk.table = TRUE,
           #pval = TRUE,  
           surv.median.line = "hv",
           xlab="Time in Days",
           xlim = c(0, 84),
           break.x.by = 7) +
  guides(colour = guide_legend(nrow = 2))

summary(km_fit_maritial)$table

# test survival curve differences 
fit.test.maritial <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ maritial_status_1, data = survcurve_baseline_before)
fit.test.maritial


#income
km_fit_income <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ income_level_1, data = survcurve_baseline_before)

ggsurvplot(km_fit_income, 
           data = survcurve_baseline_before, 
           conf.int = TRUE,
           # risk.table = TRUE,
           #pval = TRUE,  
           #surv.median.line = "hv",
           xlab="Time in Days",
           xlim = c(0, 84),
           break.x.by = 7) +
  guides(colour = guide_legend(nrow = 2))

summary(km_fit_income)$table

# test survival curve differences 
fit.test.income <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ income_level_1, data = survcurve_baseline_before)
fit.test.income

#education
km_fit_education <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ education_level_2, data = survcurve_baseline_before)

ggsurvplot(km_fit_education, 
           data = survcurve_baseline_before, 
           conf.int = TRUE,
           # risk.table = TRUE,
           #pval = TRUE,  
           surv.median.line = "hv",
           xlab="Time in Days",
           xlim = c(0, 84),
           break.x.by = 7) +
  guides(colour = guide_legend(nrow = 3))

summary(km_fit_education)$table

# test survival curve differences 
fit.test.education <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ education_level_1, data = survcurve_baseline_before)
fit.test.education


# #military - no responses in pre-launch baseline survey - no plot produced
# km_fit_military <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ currently_in_military_1, data = survcurve_baseline_before)
# 
# ggsurvplot(km_fit_military, 
#            data = survcurve_baseline_before, 
#            conf.int = TRUE,
#            # risk.table = TRUE,
#            #pval = TRUE,  
#            surv.median.line = "hv",
#            xlab="Time in Days",
#            xlim = c(0, 84),
#            break.x.by = 7) 
# 
# summary(km_fit_military)$table
# 
# # test survival curve differences 
# fit.test.military <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ currently_in_military_1, data = survcurve_baseline_before)
# fit.test.military
# 
# #veteran
# km_fit_veteran <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ veteran_1, data = survcurve_baseline_before)
# 
# ggsurvplot(km_fit_veteran, 
#            data = survcurve_baseline_before, 
#            conf.int = TRUE,
#            # risk.table = TRUE,
#            #pval = TRUE,  
#            surv.median.line = "hv",
#            xlab="Time in Days",
#            xlim = c(0, 84),
#            break.x.by = 7) 
# 
# summary(km_fit_veteran)$table
# 
# # test survival curve differences 
# fit.test.veteran <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ veteran_1, data = survcurve_baseline_before)
# fit.test.veteran
# 
# 
# #physicial activity
# km_fit_physical <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ level_of_physical_activity_1, data = survcurve_baseline_before)
# 
# ggsurvplot(km_fit_physical, 
#            data = survcurve_baseline_before, 
#            conf.int = TRUE,
#            # risk.table = TRUE,
#            #pval = TRUE,  
#            surv.median.line = "hv",
#            xlab="Time in Days",
#            xlim = c(0, 84),
#            break.x.by = 7) +
#   guides(colour = guide_legend(nrow = 4))
# 
# summary(km_fit_physical)$table
# 
# # test survival curve differences 
# fit.test.physical <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ level_of_physical_activity_1, data = survcurve_baseline_before)
# fit.test.physical

#access to phone
km_fit_access <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ always_have_acccess_to_phone_2, data = survcurve_baseline_before)

ggsurvplot(km_fit_access, 
           data = survcurve_baseline_before, 
           conf.int = TRUE,
           # risk.table = TRUE,
           #pval = TRUE,  
           surv.median.line = "hv",
           xlab="Time in Days",
           xlim = c(0, 84),
           break.x.by = 7) +
  guides(colour = guide_legend(nrow = 3))

summary(km_fit_access)$table

# test survival curve differences 
fit.test.access <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ always_have_acccess_to_phone_1, data = survcurve_baseline_before)
fit.test.access


#positive panas
km_fit_pos_panas <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ pos_panas_highlow, data = survcurve_baseline_before)

ggsurvplot(km_fit_pos_panas, 
           data = survcurve_baseline_before, 
           conf.int = TRUE,
           # risk.table = TRUE,
           #pval = TRUE,  
           surv.median.line = "hv",
           xlab="Time in Days",
           xlim = c(0, 84),
           break.x.by = 7) 

summary(km_fit_pos_panas)$table

# test survival curve differences 
fit.test.pos.panas <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ pos_panas_highlow, data = survcurve_baseline_before)
fit.test.pos.panas


#negative panas
km_fit_neg_panas <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ neg_panas_highlow, data = survcurve_baseline_before)

ggsurvplot(km_fit_neg_panas, 
           data = survcurve_baseline_before, 
           conf.int = TRUE,
           # risk.table = TRUE,
           #pval = TRUE,  
           surv.median.line = "hv",
           xlab="Time in Days",
           xlim = c(0, 84),
           break.x.by = 7) 

summary(km_fit_neg_panas)$table

# test survival curve differences 
fit.test.neg.panas <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ neg_panas_highlow, data = survcurve_baseline_before)
fit.test.neg.panas

# post relaunch
survcurve_baseline_after <- subset(survcurve_baseline, `User ID` %in% day_in_study_all_after$`User ID`)

#age
km_fit_age <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ Age_1, data = survcurve_baseline_after)

ggsurvplot(km_fit_age, 
           data = survcurve_baseline_after, 
           conf.int = TRUE,
           # risk.table = TRUE,
           #pval = TRUE,  
           surv.median.line = "hv",
           xlab="Time in Days",
           xlim = c(0, 84),
           break.x.by = 7)

summary(km_fit_age)$table

# test survival curve differences 
fit.test.age <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ Age_1, data = survcurve_baseline_after)
fit.test.age

#gender
km_fit_gender <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ gender_2, data = survcurve_baseline_after)

ggsurvplot(km_fit_gender, 
           data = survcurve_baseline_after, 
           conf.int = TRUE,
           # risk.table = TRUE,
           #pval = TRUE,  
           surv.median.line = "hv",
           xlab="Time in Days",
           xlim = c(0, 84),
           break.x.by = 7)

summary(km_fit_gender)$table

# test survival curve differences 
fit.test.gender <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ gender_1, data = survcurve_baseline_after)
fit.test.gender

#race - v1
km_fit_race <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ race_2, data = survcurve_baseline_after)

ggsurvplot(km_fit_race, 
           data = survcurve_baseline_after, 
           conf.int = TRUE,
           # risk.table = TRUE,
           #pval = TRUE,  
           surv.median.line = "hv",
           xlab="Time in Days",
           xlim = c(0, 84),
           break.x.by = 7)

summary(km_fit_race)$table

# test survival curve differences 
fit.test.race <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ race_1, data = survcurve_baseline_after)
fit.test.race


#race - v2
km_fit_race <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ race_3, data = survcurve_baseline_after)

ggsurvplot(km_fit_race, 
           data = survcurve_baseline_after, 
           conf.int = TRUE,
           # risk.table = TRUE,
           #pval = TRUE,  
           surv.median.line = "hv",
           xlab="Time in Days",
           xlim = c(0, 84),
           break.x.by = 7)

summary(km_fit_race)$table

# test survival curve differences 
fit.test.race <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ race_1, data = survcurve_baseline_after)
fit.test.race

#hispanic
km_fit_hispanic <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ Hispanic_2, data = survcurve_baseline_after)

ggsurvplot(km_fit_hispanic, 
           data = survcurve_baseline_after, 
           conf.int = TRUE,
           # risk.table = TRUE,
           #pval = TRUE,  
           surv.median.line = "hv",
           xlab="Time in Days",
           xlim = c(0, 84),
           break.x.by = 7)

summary(km_fit_hispanic)$table

# test survival curve differences 
fit.test.hispanic <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ Hispanic_1, data = survcurve_baseline_after)
fit.test.hispanic


#maritial - V1
km_fit_maritial <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ maritial_status_2, data = survcurve_baseline_after)

ggsurvplot(km_fit_maritial, 
           data = survcurve_baseline_after, 
           conf.int = TRUE,
           # risk.table = TRUE,
           #pval = TRUE,  
           surv.median.line = "hv",
           xlab="Time in Days",
           xlim = c(0, 84),
           break.x.by = 7) +
  guides(colour = guide_legend(nrow = 2))

summary(km_fit_maritial)$table

# test survival curve differences 
fit.test.maritial <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ maritial_status_1, data = survcurve_baseline_after)
fit.test.maritial


#maritial - V2
km_fit_maritial <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ maritial_status_3, data = survcurve_baseline_after)

ggsurvplot(km_fit_maritial, 
           data = survcurve_baseline_after, 
           conf.int = TRUE,
           # risk.table = TRUE,
           #pval = TRUE,  
           surv.median.line = "hv",
           xlab="Time in Days",
           xlim = c(0, 84),
           break.x.by = 7) +
  guides(colour = guide_legend(nrow = 2))

summary(km_fit_maritial)$table

# test survival curve differences 
fit.test.maritial <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ maritial_status_1, data = survcurve_baseline_after)
fit.test.maritial


#income
km_fit_income <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ income_level_1, data = survcurve_baseline_after)

ggsurvplot(km_fit_income, 
           data = survcurve_baseline_after, 
           conf.int = TRUE,
           # risk.table = TRUE,
           #pval = TRUE,  
           #surv.median.line = "hv",
           xlab="Time in Days",
           xlim = c(0, 84),
           break.x.by = 7) +
  guides(colour = guide_legend(nrow = 2))

summary(km_fit_income)$table

# test survival curve differences 
fit.test.income <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ income_level_1, data = survcurve_baseline_after)
fit.test.income

#education
km_fit_education <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ education_level_2, data = survcurve_baseline_after)

ggsurvplot(km_fit_education, 
           data = survcurve_baseline_after, 
           conf.int = TRUE,
           # risk.table = TRUE,
           #pval = TRUE,  
           surv.median.line = "hv",
           xlab="Time in Days",
           xlim = c(0, 84),
           break.x.by = 7) +
  guides(colour = guide_legend(nrow = 3))

summary(km_fit_education)$table

# test survival curve differences 
fit.test.education <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ education_level_1, data = survcurve_baseline_after)
fit.test.education


# #military - no responses in pre-launch baseline survey - no plot produced
# km_fit_military <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ currently_in_military_1, data = survcurve_baseline_after)
# 
# ggsurvplot(km_fit_military, 
#            data = survcurve_baseline_after, 
#            conf.int = TRUE,
#            # risk.table = TRUE,
#            #pval = TRUE,  
#            surv.median.line = "hv",
#            xlab="Time in Days",
#            xlim = c(0, 84),
#            break.x.by = 7) 
# 
# summary(km_fit_military)$table
# 
# # test survival curve differences 
# fit.test.military <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ currently_in_military_1, data = survcurve_baseline_after)
# fit.test.military
# 
# #veteran
# km_fit_veteran <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ veteran_1, data = survcurve_baseline_after)
# 
# ggsurvplot(km_fit_veteran, 
#            data = survcurve_baseline_after, 
#            conf.int = TRUE,
#            # risk.table = TRUE,
#            #pval = TRUE,  
#            surv.median.line = "hv",
#            xlab="Time in Days",
#            xlim = c(0, 84),
#            break.x.by = 7) 
# 
# summary(km_fit_veteran)$table
# 
# # test survival curve differences 
# fit.test.veteran <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ veteran_1, data = survcurve_baseline_after)
# fit.test.veteran
# 
# 
# #physicial activity
# km_fit_physical <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ level_of_physical_activity_1, data = survcurve_baseline_after)
# 
# ggsurvplot(km_fit_physical, 
#            data = survcurve_baseline_after, 
#            conf.int = TRUE,
#            # risk.table = TRUE,
#            #pval = TRUE,  
#            surv.median.line = "hv",
#            xlab="Time in Days",
#            xlim = c(0, 84),
#            break.x.by = 7) +
#   guides(colour = guide_legend(nrow = 4))
# 
# summary(km_fit_physical)$table
# 
# # test survival curve differences 
# fit.test.physical <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ level_of_physical_activity_1, data = survcurve_baseline_after)
# fit.test.physical

#access to phone
km_fit_access <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ always_have_acccess_to_phone_2, data = survcurve_baseline_after)

ggsurvplot(km_fit_access, 
           data = survcurve_baseline_after, 
           conf.int = TRUE,
           # risk.table = TRUE,
           #pval = TRUE,  
           surv.median.line = "hv",
           xlab="Time in Days",
           xlim = c(0, 84),
           break.x.by = 7) +
  guides(colour = guide_legend(nrow = 3))

summary(km_fit_access)$table

# test survival curve differences 
fit.test.access <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ always_have_acccess_to_phone_1, data = survcurve_baseline_after)
fit.test.access


#positive panas
km_fit_pos_panas <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ pos_panas_highlow, data = survcurve_baseline_after)

ggsurvplot(km_fit_pos_panas, 
           data = survcurve_baseline_after, 
           conf.int = TRUE,
           # risk.table = TRUE,
           #pval = TRUE,  
           surv.median.line = "hv",
           xlab="Time in Days",
           xlim = c(0, 84),
           break.x.by = 7) 

summary(km_fit_pos_panas)$table

# test survival curve differences 
fit.test.pos.panas <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ pos_panas_highlow, data = survcurve_baseline_after)
fit.test.pos.panas


#negative panas
km_fit_neg_panas <- survfit(Surv(time=Day, event=censorStatus, type = "right") ~ neg_panas_highlow, data = survcurve_baseline_after)

ggsurvplot(km_fit_neg_panas, 
           data = survcurve_baseline_after, 
           conf.int = TRUE,
           # risk.table = TRUE,
           #pval = TRUE,  
           surv.median.line = "hv",
           xlab="Time in Days",
           xlim = c(0, 84),
           break.x.by = 7) 

summary(km_fit_neg_panas)$table

# test survival curve differences 
fit.test.neg.panas <- survdiff(Surv(time=Day, event=censorStatus, type = "right") ~ neg_panas_highlow, data = survcurve_baseline_after)
fit.test.neg.panas
