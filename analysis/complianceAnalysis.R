## Packages ########################################
library(magrittr)
library(scales)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(ggbreak)
library(plotly)
library(RVAideMemoire)
library(tidyquant)
library(rstatix)

## Run script ######################################
############################# Survey data only ##################
# create a table with frequency of completing each survey for each user
df1 <- as.data.frame(table(wash_survey_baseline_cleaned_duprm$`User ID`))
colnames(df1)[1] <- "User ID"
colnames(df1)[2] <- "Baseline_Freq"

df2 <- as.data.frame(table(wash_survey_a_cleaned$`User ID`))
colnames(df2)[1] <- "User ID"
colnames(df2)[2] <- "Survey_A_Freq"

df3 <- as.data.frame(table(wash_survey_b_cleaned$`User ID`))
colnames(df3)[1] <- "User ID"
colnames(df3)[2] <- "Survey_B_Freq"

df4 <- as.data.frame(table(wash_survey_c_cleaned$`User ID`))
colnames(df4)[1] <- "User ID"
colnames(df4)[2] <- "Survey_C_Freq"

df5 <- as.data.frame(table(wash_survey_d_cleaned$`User ID`))
colnames(df5)[1] <- "User ID"
colnames(df5)[2] <- "Survey_D_Freq"

df6 <- as.data.frame(table(wash_survey_e_cleaned$`User ID`))
colnames(df6)[1] <- "User ID"
colnames(df6)[2] <- "Survey_E_Freq"

df7 <- as.data.frame(table(wash_survey_f_cleaned$`User ID`))
colnames(df7)[1] <- "User ID"
colnames(df7)[2] <- "Survey_F_Freq"

df8 <- as.data.frame(table(wash_survey_g_cleaned$`User ID`))
colnames(df8)[1] <- "User ID"
colnames(df8)[2] <- "Survey_G_Freq"

df9 <- as.data.frame(table(wash_survey_end_of_study_cleaned_duprm$`User ID`))
colnames(df9)[1] <- "User ID"
colnames(df9)[2] <- "EOS_Freq"

# full join all df
merge_1 <- merge(df1, df2, all = T)
merge_2 <- merge(merge_1, df3, all = T)
merge_3 <- merge(merge_2, df4, all = T)
merge_4 <- merge(merge_3, df5, all = T)
merge_5 <- merge(merge_4, df6, all = T)
merge_6 <- merge(merge_5, df7, all = T)
merge_7 <- merge(merge_6, df8, all = T)
merge_8 <- merge(merge_7, df9, all = T)

# calculate how many surveys each user completed
merge_8$Total_Survey_Freq <- 
  rowSums(merge_8[which(colnames(merge_8)%in% c("Baseline_Freq",
                                                "Survey_A_Freq",
                                                "Survey_B_Freq",
                                                "Survey_C_Freq",
                                                "Survey_D_Freq",
                                                "Survey_E_Freq",
                                                "Survey_F_Freq",
                                                "Survey_G_Freq",
                                                "EOS_Freq"))], na.rm = T)


# subset users who completed more than expected
# Survey schedule: 1 baseline + 11 survey A + 12 Survey B + 12 Survey C + 12 D + 12 E + 12 F + 11 G+ 1 end of study survey → 84 surveys each user
merge_8_sub <- subset(merge_8,
                      Survey_A_Freq > 11 |
                        Survey_B_Freq > 12 | 
                        Survey_C_Freq > 12 |
                        Survey_D_Freq > 12 |
                        Survey_E_Freq > 12 |
                        Survey_F_Freq > 12 |
                        Survey_G_Freq > 11)

#write.csv(merge_8_sub, "Users with extra submission.csv",row.names = F)

# merge all submission 
wash_survey_a_cleaned_sub <- subset(wash_survey_a_cleaned, select = c("User ID","date taken"))
wash_survey_a_cleaned_sub$Survey_Type <- "Survey A"

wash_survey_b_cleaned_sub <- subset(wash_survey_b_cleaned, select = c("User ID","date taken"))
wash_survey_b_cleaned_sub$Survey_Type <- "Survey B"

wash_survey_c_cleaned_sub <- subset(wash_survey_c_cleaned, select = c("User ID","date taken"))
wash_survey_c_cleaned_sub$Survey_Type <- "Survey C"

wash_survey_d_cleaned_sub <- subset(wash_survey_d_cleaned, select = c("User ID","date taken"))
wash_survey_d_cleaned_sub$Survey_Type <- "Survey D"

wash_survey_e_cleaned_sub <- subset(wash_survey_e_cleaned, select = c("User ID","date taken"))
wash_survey_e_cleaned_sub$Survey_Type <- "Survey E"

wash_survey_f_cleaned_sub <- subset(wash_survey_f_cleaned, select = c("User ID","date taken"))
wash_survey_f_cleaned_sub$Survey_Type <- "Survey F"

wash_survey_g_cleaned_sub <- subset(wash_survey_g_cleaned, select = c("User ID","date taken"))
wash_survey_g_cleaned_sub$Survey_Type <- "Survey G"

wash_survey_baseline_cleaned_sub <- subset(wash_survey_baseline_cleaned_duprm, select = c("User ID","date taken"))
wash_survey_baseline_cleaned_sub$Survey_Type <- "Baseline"

wash_survey_end_of_study_cleaned_sub <- subset(wash_survey_end_of_study_cleaned_duprm, select = c("User ID","date taken"))
wash_survey_end_of_study_cleaned_sub$Survey_Type <- "EOS"

all_survey <- rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(wash_survey_a_cleaned_sub, wash_survey_b_cleaned_sub),
                                                        wash_survey_c_cleaned_sub),
                                                  wash_survey_d_cleaned_sub),
                                            wash_survey_e_cleaned_sub),
                                      wash_survey_f_cleaned_sub),
                                wash_survey_g_cleaned_sub),
                          wash_survey_baseline_cleaned_sub),
                    wash_survey_end_of_study_cleaned_sub)

# select the earliest date for each user
all_survey_early_date <- 
  all_survey %>% 
  group_by(`User ID` ) %>%  
  arrange(`date taken`) %>%
  slice(1)

all_survey_early_date$Start_date <- format(as.Date(all_survey_early_date$`date taken`), "%Y-%m-%d")

# see which survey was the earliest survey most user submitted 
#table(all_survey_early_date$Survey_Type)

# calculate Day-in-Study
all_survey_early_date <- subset(all_survey_early_date, select = c("User ID", "Start_date"))
all_survey <- merge(all_survey,all_survey_early_date)
all_survey$Y_M_D <- format(as.Date(all_survey$`date taken`), "%Y-%m-%d")
all_survey$Day <- as.numeric(as.Date(all_survey$Y_M_D) - as.Date(all_survey$Start_date) + 1)

# calculate duration_in_Study
all_survey_last_date <- 
  all_survey %>% 
  group_by(`User ID` ) %>%  
  arrange(desc(`date taken`)) %>%
  slice(1)

all_survey_last_date$duration_in_Study <- all_survey_last_date$Day


# explore survey completion pattern
pattern_table <- as.data.frame(table(all_survey$Survey_Type, all_survey$Day))
colnames(pattern_table)[1] <- "Survey Type"
colnames(pattern_table)[2] <- "Day in Study"

# produce plot to show how many survey were completed each day, combined survey ####
pattern_table_all <- as.data.frame(table(all_survey$Day))
colnames(pattern_table_all)[1] <- "Day in Study"

p_all <-
  ggplot(pattern_table_all, aes(x=`Day in Study`, y=Freq)) +
  geom_line(group = 1, linetype = "dashed",size=0.3) + 
  geom_point(size=0.8) +
  theme_bw() +
  ylab("Number of Survey Submitted")

# produce the same plot but with Day in Study within 84 days
pattern_table_all$`Day in Study Recode` <- pattern_table_all$`Day in Study`
pattern_table_all$`Day in Study Recode` <- as.numeric(as.character(pattern_table_all$`Day in Study Recode`))

pattern_table_all %>%
  filter(`Day in Study Recode` <= 84) %>%
  ggplot(aes(x=`Day in Study`, y=Freq)) +
  geom_line(group = 1, linetype = "dashed",size=0.3) + 
  geom_point(size=0.8) +
  theme_bw() +
  ylab("Number of Survey Submitted") +
  scale_x_discrete(breaks = c(1,7,14, 21,28, 35, 42, 49, 56, 63,70,77,84))

# produce plot to show how many survey were completed each day by survey type 
# create data frame to add vline on the plot (survey schedule)
dummy <- data.frame(X = c("Baseline",
                          "EOS",
                          "Survey A","Survey A","Survey A","Survey A","Survey A","Survey A","Survey A","Survey A","Survey A","Survey A","Survey A",
                          "Survey B","Survey B","Survey B","Survey B","Survey B","Survey B","Survey B","Survey B","Survey B","Survey B","Survey B","Survey B",
                          "Survey C","Survey C","Survey C","Survey C","Survey C","Survey C","Survey C","Survey C","Survey C","Survey C","Survey C","Survey C",
                          "Survey D","Survey D","Survey D","Survey D","Survey D","Survey D","Survey D","Survey D","Survey D","Survey D","Survey D","Survey D",
                          "Survey E","Survey E","Survey E","Survey E","Survey E","Survey E","Survey E","Survey E","Survey E","Survey E","Survey E","Survey E",
                          "Survey F","Survey F","Survey F","Survey F","Survey F","Survey F","Survey F","Survey F","Survey F","Survey F","Survey F","Survey F",
                          "Survey G","Survey G","Survey G","Survey G","Survey G","Survey G","Survey G","Survey G","Survey G","Survey G","Survey G"), 
                    D = c(1, 
                          c(84),
                          c(8,15,22,29,36,43,50,57,64,71,78),
                          c(2,9,16,23,30,37,44,51,58,65,72,79),
                          c(3,10,17,24,31,38,45,52,59,66,73,80),
                          c(4,11,18,25,32,39,46,53,60,67,74,81),
                          c(5,12,19,26,33,40,47,54,61,68,75,82),
                          c(6,13,20,27,34,41,48,55,62,69,76,83),
                          c(7,14,21,28,35,42,49,56,63,70,77)
                          ))
colnames(dummy)[1] <- "Survey Type"

p1.xlabel.full <-
ggplot(pattern_table, aes(x=`Day in Study`, y=Freq)) +
  geom_line(group = 1, linetype = "dashed",size=0.3) + 
  geom_point(size=0.8) +
  theme_bw() +
  ylab("Number of Survey Submitted")+
  facet_wrap(~`Survey Type`, scales = "free_y",nrow = 9,strip.position="right") +
  geom_vline(data = dummy, aes(xintercept = D),colour = "red",size=0.3) 

p1.xlabel.selected <-
  ggplot(pattern_table, aes(x=`Day in Study`, y=Freq)) +
  geom_line(group = 1, linetype = "dashed",size=0.3) + 
  geom_point(size=0.8) +
  theme_bw() +
  ylab("Number of Survey Submitted")+
  facet_wrap(~`Survey Type`, scales = "free_y",nrow = 9,strip.position="right") +
  geom_vline(data = dummy, aes(xintercept = D),colour = "red",size=0.3) +
  scale_x_discrete(breaks = c(1,7,14, 21,28, 35, 42, 49, 56, 63,70,77,84))

#ggsave("Number of survey submitted per day.pdf", p1, width = 20, height = 15)

# Due to users may contribute to more entries than they suppose to do, % completion rate is not an accurate measure
# thus, produce plot with Y axis of % total cohort
pcent_cohort <- as.data.frame(table(all_survey$`User ID`, all_survey$Survey_Type, all_survey$Day))
colnames(pcent_cohort)[1] <- "User ID"
colnames(pcent_cohort)[2] <- "Survey_Type"
colnames(pcent_cohort)[3] <- "Day"

# filter out rows with Freq = 0
# Freq = 0 means that this user did not make a submission for this type of survey on this day
pcent_cohort_2 <- subset(pcent_cohort, Freq != 0)

pcent_cohort_3 <- as.data.frame(table(pcent_cohort_2$Survey_Type, pcent_cohort_2$Day))
colnames(pcent_cohort_3)[1] <- "Survey Type"
colnames(pcent_cohort_3)[2] <- "Day"

pcent_cohort_3$pcent <- pcent_cohort_3$Freq / nrow(merge_8) * 100

p2 <-
ggplot(pcent_cohort_3, aes(x=Day, y=pcent)) +
  geom_line(group = 1, linetype = "dashed",size=0.3) + 
  geom_point(size=0.8) +
  ylab("% Total Cohort")+
  xlab("Day in Study")+
  theme_bw() +
  facet_wrap(~ `Survey Type`,scales = "free_y", nrow = 9,strip.position="right") +
  geom_vline(data = dummy, aes(xintercept = D),colour = "red",size=0.3)

#ggsave("Pcent Total Cohort Activity per day.pdf", p2, width = 20, height = 15)

# summarize how many surveys a user completed each day by survey type
all_survey_byDate <- as.data.frame(table(all_survey$`User ID`, all_survey$Survey_Type, all_survey$Day))
colnames(all_survey_byDate)[1] <- "User ID"
colnames(all_survey_byDate)[2] <- "Survey_Type"
colnames(all_survey_byDate)[3] <- "Day"
all_survey_byDate <- all_survey_byDate[order(all_survey_byDate$`User ID`, decreasing = F), ]

# Missing data analysis 
# recruitment overtime, plot Day 1 for each user
recruitment_table <- as.data.frame(table(all_survey_early_date$Start_date))
colnames(recruitment_table)[1] <- "First Day in Study"
recruitment_table$`First Day in Study` <- as.Date(recruitment_table$`First Day in Study`)

p_recruitment <-
  ggplot(recruitment_table, aes(x=`First Day in Study`, y=Freq)) +
  geom_line(group = 1, linetype = "dashed",size=0.3) + 
  geom_point(size=0.8) +
  theme_bw() +
  ylab("Number of Participants Recruited Over Time N = 9938") +
  scale_x_date(date_breaks = "month", labels = label_date_short())

# Baseline survey submission over time
# subset date completed baseline survey
wash_survey_baseline_cleaned_duprm_recruitment <- subset(wash_survey_baseline_cleaned_duprm, select = c("date taken"))
wash_survey_baseline_cleaned_duprm_recruitment$`date taken` <- format(as.Date(wash_survey_baseline_cleaned_duprm_recruitment$`date taken`), "%Y-%m-%d")
recruitment_table_baseline <- as.data.frame(table(wash_survey_baseline_cleaned_duprm_recruitment$`date taken`))
colnames(recruitment_table_baseline)[1] <- "Day submitted baseline survey"
recruitment_table_baseline$`Day submitted baseline survey` <- as.Date(recruitment_table_baseline$`Day submitted baseline survey`)

p_recruitment2 <-
  ggplot(recruitment_table_baseline, aes(x=`Day submitted baseline survey`, y=Freq)) +
  geom_line(group = 1, linetype = "dashed",size=0.3) + 
  geom_point(size=0.8) +
  theme_bw() +
  ylab("Number of Participants completed Baseline Survey") +
  scale_x_date(date_breaks = "month", labels = label_date_short())

colnames(recruitment_table)[1] <-"Date"
colnames(recruitment_table_baseline)[1] <- "Date"

# Overlay the above two plots to see missingness in baseline
p_recruitment3 <- 
  ggplot(recruitment_table, aes(x= Date, y=Freq)) +
  geom_line(group = 1, linetype = "dashed",size=0.3) + 
  geom_ma(data = recruitment_table, ma_fun = SMA, n = 7, size = 1, color = "black") +
  geom_line(data = recruitment_table_baseline, 
             aes(x= Date, y=Freq),
             group = 1, linetype = "dashed",size=0.3,
             color = "red") +
  geom_ma(data = recruitment_table_baseline, ma_fun = SMA, n = 7, size = 1, color = "red") +
  geom_point(size=0.8) +
  theme_bw() +
  ylab("Number of Participants") +
  xlab("Date") +
  scale_x_date(date_breaks = "month", labels = label_date_short()) +
  scale_y_break(c(250, 500)) +
  scale_y_break(c(550, 750)) +
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = as.Date("2020-07-11"), xmax = as.Date("2020-08-29"),
           ymin = 0, ymax = Inf) +
  theme(text = element_text(size = 20))  +             
  annotate("text", x = as.Date("2020-05-11"), y = 230, label = "Phase 1", size = 6) +
  annotate("text", x = as.Date("2020-03-26"), y = 230, label = "<----", size = 5) +
  annotate("text", x = as.Date("2020-06-29"), y = 230, label = "---->", size = 5) +
  annotate("text", x = as.Date("2021-06-10"), y = 230, label = "Phase 2", size = 6) +
  annotate("text", x = as.Date("2020-10-21"), y = 230, label = "<-------------------------", size = 5) +
  annotate("text", x = as.Date("2021-12-17"), y = 230, label = "------------------------->", size = 5) +
  annotation_custom(grid::textGrob("Recruitment Pause", rot = 90), 
                    xmin = as.Date("2020-08-05"), xmax = as.Date("2020-08-05"), ymin = 150) 
  
  
# Moving average only
# Overlay the above two plots to see missingness in baseline
  ggplot(recruitment_table, aes(x= Date, y=Freq)) +
  #geom_line(group = 1, linetype = "dashed",size=0.3) + 
  geom_ma(data = recruitment_table, ma_fun = SMA, n = 7, size = 1, color = "black") +
  # geom_line(data = recruitment_table_baseline, 
  #           aes(x= Date, y=Freq),
  #           group = 1, linetype = "dashed",size=0.3,
  #           color = "red") +
  geom_ma(data = recruitment_table_baseline, ma_fun = SMA, n = 7, size = 1, color = "red") +
  #geom_point(size=0.8) +
  theme_bw() +
  ylab("Number of Participants") +
  xlab("Date") +
  scale_x_date(date_breaks = "month", labels = label_date_short()) +
  #scale_y_break(c(250, 770)) +
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = as.Date("2020-07-11"), xmax = as.Date("2020-08-29"),
           ymin = 0, ymax = Inf) +
  theme(text = element_text(size = 20))  +             
  annotate("text", x = as.Date("2020-05-11"), y = 230, label = "Phase 1", size = 6) +
    annotate("text", x = as.Date("2020-03-26"), y = 230, label = "<----", size = 5) +
    annotate("text", x = as.Date("2020-06-29"), y = 230, label = "---->", size = 5) +
  annotate("text", x = as.Date("2021-06-10"), y = 230, label = "Phase 2", size = 6) +
    annotate("text", x = as.Date("2020-10-21"), y = 230, label = "<-------------------------", size = 5) +
    annotate("text", x = as.Date("2021-12-17"), y = 230, label = "------------------------->", size = 5) +
  annotation_custom(grid::textGrob("Recruitment Pause", rot = 90), 
                    xmin = as.Date("2020-08-05"), xmax = as.Date("2020-08-05"), ymin = 20) 



# to test sig difference between socio demo in each phase
# Age: Chi square
# subset Age variables
compare_age1 <- as.data.frame(table(before_demo$Age_1))
colnames(compare_age1)[2] <- "Freq_1"
compare_age2 <- as.data.frame(table(after_demo$Age_1))
colnames(compare_age2)[2] <- "Freq_2"
  
# prepare fisher contingency table
compare_age <- merge(compare_age1, compare_age2)
# convert first column to be row name
rownames(compare_age) <- compare_age[,1]
# unselect unwanted column
compare_age <- subset(compare_age, select = -c(Var1))
  
  
# run chi square test
test.age <- chisq.test(compare_age)
test.age

library(RVAideMemoire)

chisq.multcomp(as.vector(merge(compare_age1, compare_age2)), p.method = "fdr")


# Gender: Chi square
# subset Gender variables
compare_gender1 <- as.data.frame(table(before_demo$gender_1))
colnames(compare_gender1)[2] <- "Freq_1"
compare_gender2 <- as.data.frame(table(after_demo$gender_1))
colnames(compare_gender2)[2] <- "Freq_2"

# prepare fisher contingency table
compare_gender <- merge(compare_gender1, compare_gender2)
# convert first column to be row name
rownames(compare_gender) <- compare_gender[,1]
# unselect unwanted column
compare_gender <- subset(compare_gender, select = -c(Var1))


# run chi square test
test.gender <- chisq.test(compare_gender)
test.gender
  

# Race: Chi square
# subset Race variables
compare_race1 <- as.data.frame(table(before_demo$race_hispanic))
colnames(compare_race1)[2] <- "Freq_1"
compare_race2 <- as.data.frame(table(after_demo$race_hispanic))
colnames(compare_race2)[2] <- "Freq_2"

# prepare fisher contingency table
compare_race <- merge(compare_race1, compare_race2)
# convert first column to be row name
rownames(compare_race) <- compare_race[,1]
# unselect unwanted column
compare_race <- subset(compare_race, select = -c(Var1))


# run chi square test
test.race <- chisq.test(compare_race)
test.race

  
# Marital: Chi square
# subset Marital variables
compare_marital1 <- as.data.frame(table(before_demo$maritial_status_1))
colnames(compare_marital1)[2] <- "Freq_1"
compare_marital2 <- as.data.frame(table(after_demo$maritial_status_1))
colnames(compare_marital2)[2] <- "Freq_2"

# prepare fisher contingency table
compare_marital <- merge(compare_marital1, compare_marital2)
# convert first column to be row name
rownames(compare_marital) <- compare_marital[,1]
# unselect unwanted column
compare_marital <- subset(compare_marital, select = -c(Var1))


# run chi square test
test.marital <- chisq.test(compare_marital)
test.marital


# Income: Chi square
# subset Income variables
compare_income1 <- as.data.frame(table(before_demo$income_level_1))
colnames(compare_income1)[2] <- "Freq_1"
compare_income2 <- as.data.frame(table(after_demo$income_level_1))
colnames(compare_income2)[2] <- "Freq_2"

# prepare fisher contingency table
compare_income <- merge(compare_income1, compare_income2)
# convert first column to be row name
rownames(compare_income) <- compare_income[,1]
# unselect unwanted column
compare_income <- subset(compare_income, select = -c(Var1))


# run chi square test
test.income <- chisq.test(compare_income)
test.income


# Education: Chi square
# subset Education variables
compare_education1 <- as.data.frame(table(before_demo$education_level_1))
colnames(compare_education1)[2] <- "Freq_1"
compare_education2 <- as.data.frame(table(after_demo$education_level_1))
colnames(compare_education2)[2] <- "Freq_2"

# prepare fisher contingency table
compare_education <- merge(compare_education1, compare_education2)
# convert first column to be row name
rownames(compare_education) <- compare_education[,1]
# unselect unwanted column
compare_education <- subset(compare_education, select = -c(Var1))


# run chi square test
test.education <- chisq.test(compare_education)
test.education



# veteran: Chi square
# subset veteran variables
compare_veteran1 <- as.data.frame(table(before_demo$veteran_1))
colnames(compare_veteran1)[2] <- "Freq_1"
compare_veteran2 <- as.data.frame(table(after_demo$veteran_1))
colnames(compare_veteran2)[2] <- "Freq_2"

# prepare fisher contingency table
compare_veteran <- merge(compare_veteran1, compare_veteran2)
# convert first column to be row name
rownames(compare_veteran) <- compare_veteran[,1]
# unselect unwanted column
compare_veteran <- subset(compare_veteran, select = -c(Var1))


# run chi square test
test.veteran <- fisher.test(compare_veteran)
test.veteran


# PA: Chi square
# subset PA variables
compare_physical1 <- as.data.frame(table(before_demo$level_of_physical_activity_1))
colnames(compare_physical1)[2] <- "Freq_1"
compare_physical2 <- as.data.frame(table(after_demo$level_of_physical_activity_1))
colnames(compare_physical2)[2] <- "Freq_2"

# prepare fisher contingency table
compare_physical <- merge(compare_physical1, compare_physical2)
# convert first column to be row name
rownames(compare_physical) <- compare_physical[,1]
# unselect unwanted column
compare_physical <- subset(compare_physical, select = -c(Var1))


# run chi square test
test.physical <- chisq.test(compare_physical)
test.physical

# Access to phone: Chi square
# subset Access to phone variables
compare_access1 <- as.data.frame(table(before_demo$always_have_acccess_to_phone_1))
colnames(compare_access1)[2] <- "Freq_1"
compare_access2 <- as.data.frame(table(after_demo$always_have_acccess_to_phone_1))
colnames(compare_access2)[2] <- "Freq_2"

# prepare fisher contingency table
compare_access <- merge(compare_access1, compare_access2)
# convert first column to be row name
rownames(compare_access) <- compare_access[,1]
# unselect unwanted column
compare_access <- subset(compare_access, select = -c(Var1))


# run chi square test
test.access <- chisq.test(compare_access)
test.access


# PANAS_pos: Mann-Whitney U test
# subset PANAS_pos variables
compare_panas1_pos <- subset(before_demo, select = c("pos_panas_score_1"))
compare_panas1_pos$cohort <- "Phase 1"

compare_panas2_pos <- subset(after_demo, select = c("pos_panas_score_1"))
compare_panas2_pos$cohort <- "Phase 2"

# prepare table
compare_panas_pos <- rbind(compare_panas1_pos, compare_panas2_pos)
compare_panas_pos$pos_panas_score_1 <- as.numeric(compare_panas_pos$pos_panas_score_1)

# Mann Whitney test
test.panas_pos <- wilcox.test(pos_panas_score_1 ~ cohort, data=compare_panas_pos, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
test.panas_pos


# PANAS_neg: Mann-Whitney U test
# subset PANAS_neg variables
compare_panas1_neg <- subset(before_demo, select = c("neg_panas_score_1"))
compare_panas1_neg$cohort <- "Phase 1"

compare_panas2_neg <- subset(after_demo, select = c("neg_panas_score_1"))
compare_panas2_neg$cohort <- "Phase 2"

# prepare table
compare_panas_neg <- rbind(compare_panas1_neg, compare_panas2_neg)
compare_panas_neg$neg_panas_score_1 <- as.numeric(compare_panas_neg$neg_panas_score_1)

# Mann Whitney test
test.panas_neg <- wilcox.test(neg_panas_score_1 ~ cohort, data=compare_panas_neg, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
test.panas_neg

  


# comparison num of iOS and Android users
sensor_demo_before <- subset(sensor_demo, `User ID` %in% day_in_study_all_before$`User ID`)
sensor_demo_after <- subset(sensor_demo, `User ID` %in% day_in_study_all_after$`User ID`)

# subset device type variables
compare_device1 <- as.data.frame(table(sensor_demo_before$Device_Type))
colnames(compare_device1)[2] <- "Freq_1"
compare_device2 <- as.data.frame(table(sensor_demo_after$Device_Type))
colnames(compare_device2)[2] <- "Freq_2"

# prepare fisher contingency table
compare_device <- merge(compare_device1, compare_device2)
# convert first column to be row name
rownames(compare_device) <- compare_device[,1]
# unselect unwanted column
compare_device <- subset(compare_device, select = -c(Var1))


# run chi square test
test.device <- chisq.test(compare_device)
test.device

  


############################# Sensor passive data only ##################
# bias analysis
# show what is the first date on which the first sensor data is collected from any participant (by sensor type)
# a good proxy to know whether this type of sensor data is available to submit throughout the study or added later 
# a-kry-sensor-ambienttemperature:2020-04-10
# a-kry-sensor-relativehumidity: 2020-04-10
# a-kry-sensor-relativehumidity: 2021-04-15
# a-kry-sensor-relativehumidity: 2021-05-03
all_metadata_bias <- 
  all_metadata %>% 
  group_by(Sensor_Type) %>%  
  arrange(Record_creation_ts) %>%
  slice(1)

all_metadata_bias <- subset(all_metadata_bias, select = c("Sensor_Type",
                                                         "Record_creation_ts"))

# get last day in study for sensor data per participant
all_metadata_last_date <- 
  all_metadata %>% 
  group_by(`User ID`) %>%  
  arrange(desc(Record_creation_ts)) %>%
  slice(1)

all_metadata_last_date <- subset(all_metadata_last_date, select = c("User ID", "Record_creation_ts"))
colnames(all_metadata_last_date)[2] <- "Sensor last date" 

# get first day in study for sensor data per participant
all_metadata_first_date <- 
  all_metadata %>% 
  group_by(`User ID`) %>%  
  arrange(Record_creation_ts) %>%
  slice(1)

all_metadata_first_date <- subset(all_metadata_first_date, select = c("User ID", "Record_creation_ts"))
colnames(all_metadata_first_date)[2] <- "Sensor first date" 

# calculate Last Day-in-Study
all_metadata_day_in_study <- merge(all_metadata_last_date, all_metadata_first_date)
all_metadata_day_in_study$`Sensor first date` <- as.Date(all_metadata_day_in_study$`Sensor first date`, "%Y-%m-%d")
all_metadata_day_in_study$`Sensor last date` <- as.Date(all_metadata_day_in_study$`Sensor last date`, "%Y-%m-%d")

all_metadata_day_in_study$Day <- as.numeric(as.Date(all_metadata_day_in_study$`Sensor last date`) - as.Date(all_metadata_day_in_study$`Sensor first date`) + 1)

# produce plot to show how many sensor data were submitted each day ####
pattern_table_sensor <- as.data.frame(table(all_metadata_day_in_study$Day))
colnames(pattern_table_sensor)[1] <- "Day in Study"

p_sensor <-
  ggplot(pattern_table_sensor, aes(x=`Day in Study`, y=Freq)) +
  geom_line(group = 1, linetype = "dashed",size=0.3) + 
  geom_point(size=0.8) +
  theme_bw() +
  ylab("Number of Sensor Data Submitted")

# produce the same plot but with Day in Study within 84 days
pattern_table_sensor$`Day in Study Recode` <- pattern_table_sensor$`Day in Study`
pattern_table_sensor$`Day in Study Recode` <- as.numeric(as.character(pattern_table_sensor$`Day in Study Recode`))

pattern_table_sensor %>%
  filter(`Day in Study Recode` <= 84) %>%
  ggplot(aes(x=`Day in Study`, y=Freq)) +
  geom_line(group = 1, linetype = "dashed",size=0.3) + 
  geom_point(size=0.8) +
  theme_bw() +
  ylab("Number of Sensor Data Submitted")+
  scale_x_discrete(breaks = c(1,7,14, 21,28, 35, 42, 49, 56, 63,70,77,84))


# explore sensor submission pattern by sensor type
all_metadata_merged <- merge(all_metadata, all_metadata_first_date, by = "User ID")
split <- strsplit(as.vector(all_metadata_merged$Sensor_Type), split = "-")
all_metadata_merged$Sensor <- sapply(split, function(x) x[4])
all_metadata_merged$Device <- sapply(split, function(x) x[1])
all_metadata_merged$Day <- as.numeric(as.Date(all_metadata_merged$Record_creation_ts) - as.Date(all_metadata_merged$`Sensor first date`) + 1)

pattern_table_sensor2 <- as.data.frame(table(all_metadata_merged$Sensor, all_metadata_merged$Day))
colnames(pattern_table_sensor2)[1] <- "Sensor Type"
colnames(pattern_table_sensor2)[2] <- "Day in Study"
pattern_table_sensor2$`Day in Study Recode` <- pattern_table_sensor2$`Day in Study`
pattern_table_sensor2$`Day in Study Recode` <- as.numeric(as.character(pattern_table_sensor2$`Day in Study Recode`))


# produce plot to show how many sensor data were submitted each day by sensor type with 84 days
pattern_table_sensor2 %>%
  filter(`Day in Study Recode` <= 84) %>%
  ggplot(aes(x=`Day in Study`, y=Freq)) +
  geom_line(group = 1, linetype = "dashed",size=0.3) + 
  geom_point(size=0.8) +
  theme_bw() +
  ylab("Number of Sensor data Submitted")+
  facet_wrap(~`Sensor Type`, scales = "free_y",nrow = 35,strip.position="right") +
  scale_x_discrete(breaks = c(1,7,14, 21,28, 35, 42, 49, 56, 63,70,77,84))

# produce plot to show how many sensor data were submitted each day by sensor type, X-axis: Date
pattern_table_sensor3 <- as.data.frame(table(all_metadata_merged$Sensor, all_metadata_merged$`Sensor first date`))
colnames(pattern_table_sensor3)[1] <- "Sensor Type"
colnames(pattern_table_sensor3)[2] <- "First Day in Study"
pattern_table_sensor3$`First Day in Study` <- as.Date(pattern_table_sensor3$`First Day in Study`)
pattern_table_sensor3$pcent <- pattern_table_sensor3$Freq / nrow(all_metadata_first_date) * 100

p_sensor <-
  ggplot(pattern_table_sensor3, aes(x=`First Day in Study`, y=pcent)) +
  geom_line(group = 1, linetype = "dashed",size=0.3) + 
  geom_point(size=0.8) +
  theme_bw() +
  ylab("% cohort submitted sensor data N = 10583") +
  facet_wrap(~`Sensor Type`, scales = "free_y",nrow = 35,strip.position="right") +
  scale_x_date(date_breaks = "month", labels = label_date_short())

                                     
# obtain how many ppl were iOS or Android users in Phase 1 and 2
id_sensor_pair_before <- subset(id_sensor_pair, `User ID` %in% day_in_study_all_before$`User ID`)
id_sensor_pair_after <- subset(id_sensor_pair, `User ID` %in% day_in_study_all_after$`User ID`)
# a: 2536, i: 3958
table(id_sensor_pair_before$Device_Type)
# a: 2164, i: 1925
table(id_sensor_pair_after$Device_Type)
                                     
                                     
# produce a barplot showing proportion of participants (within 10583) submitted each sensor type 
tab_sensor_type <- as.data.frame(table(all_metadata_merged$`User ID`, all_metadata_merged$Device, all_metadata_merged$Sensor))
tab_sensor_type <- tab_sensor_type[! (tab_sensor_type$Freq == 0),]

# recode sensor type
tab_sensor_type$Var4[tab_sensor_type$Var3 == "accelerometer"] <- "*Accelerometer"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "accelerometeruncalibrated"] <- "Accelerometer Uncalibrated"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "accessibility"] <- "Accessibility"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "altitude"] <- "Altimeter"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "ambienttemperature"] <- "Ambient"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "audio"] <- "Microphone-Voice Survey"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "batterystate"] <- "Battery State"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "bluetooth"] <- "Bluetooth"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "compass"] <- "Compass"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "gamerotationvector"] <- "Game Rotation Vector"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "geomagneticrotationvector"] <- "Geomagnetic Rotation Vector"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "gps"] <- "GPS"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "gravity"] <- "Gravity"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "gyroscope"] <- "*Gyroscope"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "gyroscopeuncalibrated"] <- "Gyroscope Uncalibrated"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "historicalactivitydata"] <- "Historical Activity Data"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "light"] <- "Photodetector"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "linearacceleration"] <- "Linear Accelerator"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "location"] <- "GPS"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "magneticfield"] <- "Magnetometer"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "magneticfielduncalibrated"] <- "Magnetometer Uncalibrated"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "magnetometer"] <- "Magnetometer"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "mfcc"] <- "Microphone"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "orientation"] <- "Compass"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "pedometer"] <- "Pedometer"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "pressure"] <- "Barometer"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "proximity"] <- "Proximity"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "relativehumidity"] <- "Humidity"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "rotationvector"] <- "Rotation Vector"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "sms"] <- "SMS"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "stepcounter"] <- "Pedometer"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "stepdetector"] <- "Step Detector"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "survey"] <- "Active Task"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "tiltsensor"] <- "Tilt"
tab_sensor_type$Var4[tab_sensor_type$Var3 == "wifi"] <- "WiFi"


# facet by phases
tab_sensor_type_before <- subset(tab_sensor_type, Var1 %in% day_in_study_all_before$`User ID`)
tab_sensor_type_after <- subset(tab_sensor_type, Var1 %in% day_in_study_all_after$`User ID`)

# Phase 1 table
# produce a table with all sensor type
tab_sensor_prop_before <- as.data.frame(table(tab_sensor_type_before$Var2,tab_sensor_type_before$Var4))
tab_sensor_prop_before$`Device Type`[tab_sensor_prop_before$Var1 == "i"] <- "iOS"
tab_sensor_prop_before$`Device Type`[tab_sensor_prop_before$Var1 == "a"] <- "Android"
tab_sensor_prop_before$Var2 <- as.character(tab_sensor_prop_before$Var2)

# Phase 2 table
# produce a table with all sensor type
tab_sensor_prop_after <- as.data.frame(table(tab_sensor_type_after$Var2,tab_sensor_type_after$Var4))
tab_sensor_prop_after$`Device Type`[tab_sensor_prop_after$Var1 == "i"] <- "iOS"
tab_sensor_prop_after$`Device Type`[tab_sensor_prop_after$Var1 == "a"] <- "Android"
tab_sensor_prop_after$Var2 <- as.character(tab_sensor_prop_after$Var2)

# unselect unwanted sensor type: uncalibrated and audio, avoid noise
tab_sensor_prop_before <- tab_sensor_prop_before[! (tab_sensor_prop_before$Var2 %in% c("Accelerometer Uncalibrated",
                                                                  "Gyroscope Uncalibrated",
                                                                  "Magnetometer Uncalibrated",
                                                                  "Microphone-Voice Survey")),]


tab_sensor_prop_after <- tab_sensor_prop_after[! (tab_sensor_prop_after$Var2 %in% c("Accelerometer Uncalibrated",
                                                                                       "Gyroscope Uncalibrated",
                                                                                       "Magnetometer Uncalibrated",
                                                                                       "Microphone-Voice Survey")),]



# sensor types that are available in iOS
#phase 1
tab_sensor_prop_before_i <- subset(tab_sensor_prop_before, Var1 == "i" & Var2 %in% c("Accessibility",
                                                         "Altimeter",
                                                         "Battery State",
                                                         "Historical Activity Data",
                                                         "*Accelerometer",
                                                         "*Gyroscope",
                                                         "Magnetometer",
                                                         "Pedometer",
                                                         "GPS",
                                                         "Compass",
                                                         "Microphone",
                                                         "Active Task",
                                                         "Barometer"))


tab_sensor_prop_before_i[nrow(tab_sensor_prop_before_i) + 1 ,] <- c("i", "Camera", 0 , "iOS")
tab_sensor_prop_before_i$Freq <- as.numeric(tab_sensor_prop_before_i$Freq)
tab_sensor_prop_before_i$Prop <- round(tab_sensor_prop_before_i$Freq / 3958 * 100,1)
#tab_sensor_prop_before_i <- tab_sensor_prop_before_i[! (tab_sensor_prop_before_i$Freq == 0),]


#graph
brks <- seq(0, 100, by = 25)
lbls <- seq(0, 100, 25)

ggplot(data=tab_sensor_prop_before_i, aes(x=reorder(Var2, abs(Prop)), 
                                   y=Prop,
                                   fill=`Device Type`)) +
  geom_bar(stat="identity", width = 0.6) +
  coord_flip() +
  xlab("Sensor Type")+
  ylab("Proportion (%) of Participants Submitted Sensor Data who were iOS users")+
  ggtitle("Sensor Types Availabile Only in iOS - Phase 1") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_color_manual(values = c("#007AFF")) +
  scale_fill_manual("Device Type", values = c("iOS"="#007AFF"))+
  scale_y_continuous(breaks = brks, labels = lbls)+
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5)) 


#phase 2
tab_sensor_prop_after_i <- subset(tab_sensor_prop_after, Var1 == "i" & Var2 %in% c("Accessibility",
                                                                                     "Altimeter",
                                                                                     "Battery State",
                                                                                     "Historical Activity Data",
                                                                                     "*Accelerometer",
                                                                                     "*Gyroscope",
                                                                                     "Magnetometer",
                                                                                     "Pedometer",
                                                                                     "GPS",
                                                                                     "Compass",
                                                                                     "Microphone",
                                                                                     "Active Task",
                                                                                     "Barometer"))


tab_sensor_prop_after_i[nrow(tab_sensor_prop_after_i) + 1 ,] <- c("i", "Camera", 0 , "iOS")
tab_sensor_prop_after_i$Freq <- as.numeric(tab_sensor_prop_after_i$Freq)
tab_sensor_prop_after_i$Prop <- round(tab_sensor_prop_after_i$Freq / 1925 * 100,1)
#tab_sensor_prop_after_i <- tab_sensor_prop_after_i[! (tab_sensor_prop_after_i$Freq == 0),]


#graph
brks <- seq(0, 100, by = 25)
lbls <- seq(0, 100, 25)

ggplot(data=tab_sensor_prop_after_i, aes(x=reorder(Var2, abs(Prop)), 
                                          y=Prop,
                                          fill=`Device Type`)) +
  geom_bar(stat="identity", width = 0.6) +
  coord_flip() +
  xlab("Sensor Type")+
  ylab("Proportion (%) of Participants Submitted Sensor Data who were iOS users")+
  ggtitle("Sensor Types Availabile Only in iOS - Phase 2") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_color_manual(values = c("#007AFF")) +
  scale_fill_manual("Device Type", values = c("iOS"="#007AFF"))+
  scale_y_continuous(breaks = brks, labels = lbls)+
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5)) 







# sensor types that are available in Android
#phase 1
tab_sensor_prop_before_a <- subset(tab_sensor_prop_before, Var1 == "a" & ! (Var2 %in% c("Accessibility",
                                                                                     "Altimeter",
                                                                                     "Battery State",
                                                                                     "Historical Activity Data"
                                                                                    )))

tab_sensor_prop_before_a[nrow(tab_sensor_prop_before_a) + 1 ,] <- c("a", "Motion", 0 , "Android")
tab_sensor_prop_before_a[nrow(tab_sensor_prop_before_a) + 1 ,] <- c("a", "Stationary Detection", 0 , "Android")
tab_sensor_prop_before_a[nrow(tab_sensor_prop_before_a) + 1 ,] <- c("a", "Thermometer", 0 , "Android")
tab_sensor_prop_before_a[nrow(tab_sensor_prop_before_a) + 1 ,] <- c("a", "Grip", 0 , "Android")
tab_sensor_prop_before_a[nrow(tab_sensor_prop_before_a) + 1 ,] <- c("a", "Device State", 0 , "Android")
tab_sensor_prop_before_a[nrow(tab_sensor_prop_before_a) + 1 ,] <- c("a", "External Storage", 0 , "Android")
tab_sensor_prop_before_a[nrow(tab_sensor_prop_before_a) + 1 ,] <- c("a", "Calendar", 0 , "Android")
tab_sensor_prop_before_a[nrow(tab_sensor_prop_before_a) + 1 ,] <- c("a", "Camera", 0 , "Android")


tab_sensor_prop_before_a$Freq <- as.numeric(tab_sensor_prop_before_a$Freq)
tab_sensor_prop_before_a$Prop <- round(tab_sensor_prop_before_a$Freq / 2536 * 100,1)
#tab_sensor_prop_before_i <- tab_sensor_prop_before_i[! (tab_sensor_prop_before_i$Freq == 0),]

# indicate sensor that requires sharing permission
tab_sensor_prop_before_a$Var3 <- tab_sensor_prop_before_a$Var2
tab_sensor_prop_before_a$Var3[tab_sensor_prop_before_a$Var2 == "WiFi"] <- "**WiFi"
tab_sensor_prop_before_a$Var3[tab_sensor_prop_before_a$Var2 == "GPS"] <- "**GPS"
tab_sensor_prop_before_a$Var3[tab_sensor_prop_before_a$Var2 == "SMS"] <- "**SMS"
tab_sensor_prop_before_a$Var3[tab_sensor_prop_before_a$Var2 == "Microphone"] <- "**Microphone"
tab_sensor_prop_before_a$Var3[tab_sensor_prop_before_a$Var2 == "Bluetooth"] <- "**Bluetooth"
tab_sensor_prop_before_a$Var3[tab_sensor_prop_before_a$Var2 == "External Storage"] <- "**External Storage"
tab_sensor_prop_before_a$Var3[tab_sensor_prop_before_a$Var2 == "Calendar"] <- "**Calendar"
tab_sensor_prop_before_a$Var3[tab_sensor_prop_before_a$Var2 == "Camera"] <- "**Camera"



#graph
brks <- seq(0, 100, by = 25)
lbls <- seq(0, 100, 25)

ggplot(data=tab_sensor_prop_before_a, aes(x=reorder(Var3, abs(Prop)), 
                                          y=Prop,
                                          fill=`Device Type`)) +
  geom_bar(stat="identity", width = 0.6) +
  coord_flip() +
  xlab("Sensor Type")+
  ylab("Proportion (%) of Participants Submitted Sensor Data who were Android users")+
  ggtitle("Sensor Types Availabile Only in Android - Phase 1") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_color_manual(values = c("#a4c639")) +
  scale_fill_manual("Device Type", values = c("Android"="#a4c639"))+
  scale_y_continuous(breaks = brks, labels = lbls)+
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5)) 



#phase 2
tab_sensor_prop_after_a <- subset(tab_sensor_prop_after, Var1 == "a" & ! (Var2 %in% c("Accessibility",
                                                                                        "Altimeter",
                                                                                        "Battery State",
                                                                                        "Historical Activity Data"
)))

tab_sensor_prop_after_a[nrow(tab_sensor_prop_after_a) + 1 ,] <- c("a", "Motion", 0 , "Android")
tab_sensor_prop_after_a[nrow(tab_sensor_prop_after_a) + 1 ,] <- c("a", "Stationary Detection", 0 , "Android")
tab_sensor_prop_after_a[nrow(tab_sensor_prop_after_a) + 1 ,] <- c("a", "Thermometer", 0 , "Android")
tab_sensor_prop_after_a[nrow(tab_sensor_prop_after_a) + 1 ,] <- c("a", "Grip", 0 , "Android")
tab_sensor_prop_after_a[nrow(tab_sensor_prop_after_a) + 1 ,] <- c("a", "Device State", 0 , "Android")
tab_sensor_prop_after_a[nrow(tab_sensor_prop_after_a) + 1 ,] <- c("a", "External Storage", 0 , "Android")
tab_sensor_prop_after_a[nrow(tab_sensor_prop_after_a) + 1 ,] <- c("a", "Calendar", 0 , "Android")
tab_sensor_prop_after_a[nrow(tab_sensor_prop_after_a) + 1 ,] <- c("a", "Camera", 0 , "Android")


tab_sensor_prop_after_a$Freq <- as.numeric(tab_sensor_prop_after_a$Freq)
tab_sensor_prop_after_a$Prop <- round(tab_sensor_prop_after_a$Freq / 2164 * 100,1)
#tab_sensor_prop_after_i <- tab_sensor_prop_after_i[! (tab_sensor_prop_after_i$Freq == 0),]

# indicate sensor that requires sharing permission
tab_sensor_prop_after_a$Var3 <- tab_sensor_prop_after_a$Var2
tab_sensor_prop_after_a$Var3[tab_sensor_prop_after_a$Var2 == "WiFi"] <- "**WiFi"
tab_sensor_prop_after_a$Var3[tab_sensor_prop_after_a$Var2 == "GPS"] <- "**GPS"
tab_sensor_prop_after_a$Var3[tab_sensor_prop_after_a$Var2 == "SMS"] <- "**SMS"
tab_sensor_prop_after_a$Var3[tab_sensor_prop_after_a$Var2 == "Microphone"] <- "**Microphone"
tab_sensor_prop_after_a$Var3[tab_sensor_prop_after_a$Var2 == "Bluetooth"] <- "**Bluetooth"
tab_sensor_prop_after_a$Var3[tab_sensor_prop_after_a$Var2 == "External Storage"] <- "**External Storage"
tab_sensor_prop_after_a$Var3[tab_sensor_prop_after_a$Var2 == "Calendar"] <- "**Calendar"
tab_sensor_prop_after_a$Var3[tab_sensor_prop_after_a$Var2 == "Camera"] <- "**Camera"



#graph
brks <- seq(0, 100, by = 25)
lbls <- seq(0, 100, 25)

ggplot(data=tab_sensor_prop_after_a, aes(x=reorder(Var3, abs(Prop)), 
                                          y=Prop,
                                          fill=`Device Type`)) +
  geom_bar(stat="identity", width = 0.6) +
  coord_flip() +
  xlab("Sensor Type")+
  ylab("Proportion (%) of Participants Submitted Sensor Data who were Android users")+
  ggtitle("Sensor Types Availabile Only in Android - Phase 2") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_color_manual(values = c("#a4c639")) +
  scale_fill_manual("Device Type", values = c("Android"="#a4c639"))+
  scale_y_continuous(breaks = brks, labels = lbls)+
  theme(legend.position="bottom") +
  theme(plot.title = element_text(hjust = 0.5)) 


# make plot to integrate phase 1 and 2 on the same plot
# prepare dataset
# Android
tab_sensor_prop_before_a$Phase <- "Phase 1"
tab_sensor_prop_after_a$Phase <- "Phase 2"

tab_sensor_prop_all_a <- rbind(tab_sensor_prop_before_a, tab_sensor_prop_after_a)
tab_sensor_prop_all_a$Prop_recode <- tab_sensor_prop_all_a$Prop
tab_sensor_prop_all_a$Prop_recode[tab_sensor_prop_all_a$Phase == "Phase 1"] <- tab_sensor_prop_all_a$Prop_recode * (-1)
tab_sensor_prop_all_a <- tab_sensor_prop_all_a[order(-tab_sensor_prop_all_a$Prop_recode),]

t <- list(family = "arial",
          size = 20)

# pyramid plot
pyramid_android <- plot_ly(tab_sensor_prop_all_a, 
                           x = tab_sensor_prop_all_a$Prop_recode, 
                           y = reorder(tab_sensor_prop_all_a$Var3, abs(tab_sensor_prop_all_a$Prop_recode)), 
                           group = tab_sensor_prop_all_a$Phase, 
                           color = tab_sensor_prop_all_a$Phase,
                           colors = c(`Phase 1` = "#e66101", `Phase 2` = "#5e3c99"),
                           type = 'bar', 
                           orientation = 'h') %>%
  layout(bargap = 0.1, barmode = 'overlay',
         xaxis = list(tickmode = 'array', tickvals = c(-100, -75, -50, -25, 0, 25, 50, 75, 100),
                      ticktext = c('100', "75", '50', "25",'0', "25", '50', "75", '100')),
         font = t)

# box plot
ggplot(tab_sensor_prop_all_a, aes(x=reorder(Var3, abs(Prop)), y=Prop, fill=Phase)) + 
  geom_bar(stat="identity", width = 0.6, position=position_dodge()) +
  theme_bw() +
  coord_flip() +
  ylab("Proportion")+
  xlab("Sensor Type")+
  theme(text = element_text(size = 30),
        legend.title=element_blank()) +
  scale_fill_manual(values=c('#e66101','#5e3c99'))+
#  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  geom_hline(yintercept = 50, color = "red") + 
  geom_hline(yintercept = 75, color = "red")


#axis.text.x = element_text(angle = 45))




# iOS
tab_sensor_prop_before_i$Phase <- "Phase 1"
tab_sensor_prop_after_i$Phase <- "Phase 2"

tab_sensor_prop_all_i <- rbind(tab_sensor_prop_before_i, tab_sensor_prop_after_i)
tab_sensor_prop_all_i$Prop_recode <- tab_sensor_prop_all_i$Prop
tab_sensor_prop_all_i$Prop_recode[tab_sensor_prop_all_i$Phase == "Phase 1"] <- tab_sensor_prop_all_i$Prop_recode * (-1)
tab_sensor_prop_all_i <- tab_sensor_prop_all_i[order(-tab_sensor_prop_all_i$Prop_recode),]

t <- list(family = "arial",
          size = 13)

# pyramid plot
pyramid_ios <- plot_ly(tab_sensor_prop_all_i, 
                           x = tab_sensor_prop_all_i$Prop_recode, 
                           y = reorder(tab_sensor_prop_all_i$Var2, abs(tab_sensor_prop_all_i$Prop_recode)), 
                           group = tab_sensor_prop_all_i$Phase, 
                           color = tab_sensor_prop_all_i$Phase,
                           colors = c(`Phase 1` = "#e66101", `Phase 2` = "#5e3c99"),
                           type = 'bar', 
                           orientation = 'h') %>%
  layout(bargap = 0.1, barmode = 'overlay',
         xaxis = list(tickmode = 'array', tickvals = c(-100, -75, -50, -25, 0, 25, 50, 75, 100),
                      ticktext = c('100', "75", '50', "25",'0', "25", '50', "75", '100')),
         font = t,
         layout(shapes = list(vline(c(-75, -50, 50, 75)))))



# box plot
ggplot(tab_sensor_prop_all_i, aes(x=reorder(Var2, abs(Prop)), y=Prop, fill=Phase)) + 
  geom_bar(stat="identity",  width = 0.6,position=position_dodge()) +
  theme_bw() +
  coord_flip() +
  ylab("Proportion")+
  xlab("Sensor Type")+
  theme(text = element_text(size = 20),
        legend.title=element_blank()) +
  scale_fill_manual(values=c('#e66101','#5e3c99'))+
 # theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  geom_hline(yintercept = 50, color = "red") + 
  geom_hline(yintercept = 75, color = "red")


                                     
                                     # chi square test
# accelerometer
chi_a_before <- subset(tab_sensor_type_before, Var2 == "a" & Var3 == "accelerometer") 
chi_a_before$Phase <- "Phase 1"
chi_a_after <- subset(tab_sensor_type_after, Var2 == "a" & Var3 == "accelerometer") 
chi_a_after$Phase <- "Phase 2"
chi_i_before <- subset(tab_sensor_type_before, Var2 == "i" & Var3 == "accelerometer") 
chi_i_before$Phase <- "Phase 1"
chi_i_after <- subset(tab_sensor_type_after, Var2 == "i" & Var3 == "accelerometer") 
chi_i_after$Phase <- "Phase 2"

# android
compare_a1 <- as.data.frame(table(chi_a_before$Phase))
colnames(compare_a1)[2] <- "Freq_1"
compare_a2 <- as.data.frame(table(chi_a_after$Phase))
colnames(compare_a2)[2] <- "Freq_1"

# prepare fisher contingency table
compare_a <- rbind(compare_a1, compare_a2)
# convert first column to be row name
rownames(compare_a) <- compare_a[,1]
# unselect unwanted column
compare_a <- subset(compare_a, select = -c(Var1))


# run chi square test
test.a <- chisq.test(compare_a)
test.a


# iOS
compare_i1 <- as.data.frame(table(chi_i_before$Phase))
colnames(compare_i1)[2] <- "Freq_1"
compare_i2 <- as.data.frame(table(chi_i_after$Phase))
colnames(compare_i2)[2] <- "Freq_1"

# prepare fisher contingency table
compare_i <- rbind(compare_i1, compare_i2)
# convert first column to be row name
rownames(compare_i) <- compare_i[,1]
# unselect unwanted column
compare_i <- subset(compare_i, select = -c(Var1))


# run chi square test
test.i <- chisq.test(compare_i)
test.i


# gyroscope
chi_a_before <- subset(tab_sensor_type_before, Var2 == "a" & Var3 == "gyroscope") 
chi_a_before$Phase <- "Phase 1"
chi_a_after <- subset(tab_sensor_type_after, Var2 == "a" & Var3 == "gyroscope") 
chi_a_after$Phase <- "Phase 2"
chi_i_before <- subset(tab_sensor_type_before, Var2 == "i" & Var3 == "gyroscope") 
chi_i_before$Phase <- "Phase 1"
chi_i_after <- subset(tab_sensor_type_after, Var2 == "i" & Var3 == "gyroscope") 
chi_i_after$Phase <- "Phase 2"

# android
compare_a1 <- as.data.frame(table(chi_a_before$Phase))
colnames(compare_a1)[2] <- "Freq_1"
compare_a2 <- as.data.frame(table(chi_a_after$Phase))
colnames(compare_a2)[2] <- "Freq_1"

# prepare fisher contingency table
compare_a <- rbind(compare_a1, compare_a2)
# convert first column to be row name
rownames(compare_a) <- compare_a[,1]
# unselect unwanted column
compare_a <- subset(compare_a, select = -c(Var1))


# run chi square test
test.a <- chisq.test(compare_a)
test.a


# iOS
compare_i1 <- as.data.frame(table(chi_i_before$Phase))
colnames(compare_i1)[2] <- "Freq_1"
compare_i2 <- as.data.frame(table(chi_i_after$Phase))
colnames(compare_i2)[2] <- "Freq_1"

# prepare fisher contingency table
compare_i <- rbind(compare_i1, compare_i2)
# convert first column to be row name
rownames(compare_i) <- compare_i[,1]
# unselect unwanted column
compare_i <- subset(compare_i, select = -c(Var1))


# run chi square test
test.i <- chisq.test(compare_i)
test.i







# logistic regression
# recode sensor type
all_metadata_merged$Var4[all_metadata_merged$Sensor == "accelerometer"] <- "Accelerometer"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "accelerometeruncalibrated"] <- "Accelerometer Uncalibrated"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "accessibility"] <- "Accessibility"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "altitude"] <- "Altimeter"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "ambienttemperature"] <- "Ambient Sensor"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "audio"] <- "Microphone-Voice Survey"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "batterystate"] <- "Battery State"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "bluetooth"] <- "Bluetooth"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "compass"] <- "Compass"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "gamerotationvector"] <- "Game Rotation Vector Sensor"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "geomagneticrotationvector"] <- "Geomagnetic Rotation Vector Sensor"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "gps"] <- "GPS"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "gravity"] <- "Gravity Sensor"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "gyroscope"] <- "Gyroscope"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "gyroscopeuncalibrated"] <- "Gyroscope Uncalibrated"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "historicalactivitydata"] <- "Historical Activity Data"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "light"] <- "Photodetector"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "linearacceleration"] <- "Linear Accelerator"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "location"] <- "GPS"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "magneticfield"] <- "Magnetometer"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "magneticfielduncalibrated"] <- "Magnetometer Uncalibrated"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "magnetometer"] <- "Magnetometer"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "mfcc"] <- "Microphone"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "orientation"] <- "Compass"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "pedometer"] <- "Pedometer"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "pressure"] <- "Barometer"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "proximity"] <- "Proximity Sensor"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "relativehumidity"] <- "Humidity Sensor"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "rotationvector"] <- "Rotation Vector Sensor"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "sms"] <- "SMS"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "stepcounter"] <- "Pedometer"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "stepdetector"] <- "Step Detector"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "survey"] <- "Active Task Sensor"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "tiltsensor"] <- "Tilt Sensor"
all_metadata_merged$Var4[all_metadata_merged$Sensor == "wifi"] <- "WiFi"


# prepare dataset
tab_log_reg <- as.data.frame(table(all_metadata_merged$`User ID`, all_metadata_merged$Var4))


tab_log_reg <- tab_log_reg[! (tab_log_reg$Var2 %in% c("Accelerometer Uncalibrated",
                                                                                       "Gyroscope Uncalibrated",
                                                                                       "Magnetometer Uncalibrated",
                                                                                       "Microphone-Voice Survey")),]

# combine id_sensor pair
colnames(tab_log_reg)[1] <- "User ID"
colnames(tab_log_reg)[2] <- "Sensor_Type"
tab_log_reg <-  merge(tab_log_reg, id_sensor_pair, all.x = T)

# recode sharing freq --> binary
tab_log_reg$shared[tab_log_reg$Freq >  0] <- 1
tab_log_reg$shared[tab_log_reg$Freq == 0] <- 0
#tab_log_reg$shared <- factor(tab_log_reg$shared,levels=c("Shared","Not Shared"))

# facet by phases
tab_log_reg$phase[tab_log_reg$`User ID` %in% day_in_study_all_before$`User ID`] <- "Phase 1"
tab_log_reg$phase[tab_log_reg$`User ID` %in% day_in_study_all_after$`User ID`] <- "Phase 2"

# combine with baseline info
tab_log_reg <- merge(tab_log_reg, wash_survey_baseline_merged, all.x = T)
tab_log_reg <- subset(tab_log_reg, select = c(`User ID`,Freq,Sensor_Type, Device_Type,
                                              shared, phase, 
                                              Age,
                                              gender,
                                              race_hispanic,
                                              maritial_status_recode,
                                              income_level_recode,
                                              education_level_recode,
                                              always_have_acccess_to_phone_recode,
                                              currently_in_military,
                                              veteran,
                                              level_of_physical_activity
                                              #neg_panas_score,
                                             # pos_panas_score
                                              #pos_panas_highlow,
                                              #neg_panas_highlow
                                              ))
# code empty data with NA
tab_log_reg[is.na(tab_log_reg)] <- "NA"

# combine NA with invalid
tab_log_reg[tab_log_reg == "invalid"] <- "NA"

# reorder levels
tab_log_reg$race_hispanic <- factor(tab_log_reg$race_hispanic, levels = c("White", "Asian", "Black or African American",
                                                                         "Hispanic", "Other", "NA"))

tab_log_reg$income_level_recode <- factor(tab_log_reg$income_level_recode, levels = c("Less than $25,000", "$25,000 to $49,999", 
                                                                               "$50,000 to $74,999", "$75,000 to $99,999",
                                                                         "More than $100,000", "NA"))

tab_log_reg$education_level_recode <- factor(tab_log_reg$education_level_recode, levels = c("High School and lower", "College", "Graduate School",
                                                                       "NA"))


# subset into phase 1 and 2
tab_log_reg_before <- subset(tab_log_reg, `User ID` %in% day_in_study_all_before$`User ID`)
tab_log_reg_after <- subset(tab_log_reg, `User ID` %in% day_in_study_all_after$`User ID`)

# sensor types available in both OS
# Accelerometer
tab_log_reg_before_accelerometer <- subset(tab_log_reg_before, Sensor_Type %in% c("Accelerometer"))
tab_log_reg_after_accelerometer <- subset(tab_log_reg_after, Sensor_Type %in% c("Accelerometer"))


# # log regression
# log_model_accelerometer <- glm(shared ~ phase + Device_Type,
#                       data=tab_log_reg_accelerometer, family=binomial())
# 
# exp(coef(log_model_accelerometer))  
# summary(log_model_accelerometer)

# Gyroscope
tab_log_reg_before_gyroscope <- subset(tab_log_reg_before, Sensor_Type %in% c("Gyroscope"))
tab_log_reg_after_gyroscope <- subset(tab_log_reg_after, Sensor_Type %in% c("Gyroscope"))


# # log regression
# log_model_gyroscope <- glm(shared ~ phase + Device_Type,
#                                data=tab_log_reg_gyroscope, family=binomial())
# 
# exp(coef(log_model_gyroscope))  
# summary(log_model_gyroscope)

# GPS
tab_log_reg_before_gps <- subset(tab_log_reg_before, Sensor_Type %in% c("GPS"))
tab_log_reg_after_gps <- subset(tab_log_reg_after, Sensor_Type %in% c("GPS"))

# # log regression
# log_model_gps <- glm(shared ~ phase + Device_Type,
#                            data=tab_log_reg_gps, family=binomial())
# 
# exp(coef(log_model_gps))  
# summary(log_model_gps)

# Magnetometer
tab_log_reg_before_magnetometer <- subset(tab_log_reg_before, Sensor_Type %in% c("Magnetometer"))
tab_log_reg_after_magnetometer <- subset(tab_log_reg_after, Sensor_Type %in% c("Magnetometer"))

# # log regression
# log_model_magnetometer <- glm(shared ~ phase + Device_Type,
#                      data=tab_log_reg_magnetometer, family=binomial())
# 
# exp(coef(log_model_magnetometer))  
# summary(log_model_magnetometer)

# Pedometer
tab_log_reg_before_pedometer <- subset(tab_log_reg_before, Sensor_Type %in% c("Pedometer"))
tab_log_reg_after_pedometer <- subset(tab_log_reg_after, Sensor_Type %in% c("Pedometer"))

# # log regression
# log_model_pedometer <- glm(shared ~ phase + Device_Type,
#                               data=tab_log_reg_pedometer, family=binomial())
# 
# exp(coef(log_model_pedometer))  
# summary(log_model_pedometer)

# Compass
tab_log_reg_before_compass <- subset(tab_log_reg_before, Sensor_Type %in% c("Compass"))
tab_log_reg_after_compass <- subset(tab_log_reg_after, Sensor_Type %in% c("Compass"))

# # log regression
# log_model_compass <- glm(shared ~ phase + Device_Type,
#                            data=tab_log_reg_compass, family=binomial())
# 
# exp(coef(log_model_compass))  
# summary(log_model_compass)

# Microphone
tab_log_reg_before_microphone <- subset(tab_log_reg_before, Sensor_Type %in% c("Microphone"))
tab_log_reg_after_microphone <- subset(tab_log_reg_after, Sensor_Type %in% c("Microphone"))


# # log regression
# log_model_microphone <- glm(shared ~ phase + Device_Type,
#                          data=tab_log_reg_microphone, family=binomial())
# 
# exp(coef(log_model_microphone))  
# summary(log_model_microphone)

# Barometer
tab_log_reg_before_barometer <- subset(tab_log_reg_before, Sensor_Type %in% c("Barometer"))
tab_log_reg_after_barometer <- subset(tab_log_reg_after, Sensor_Type %in% c("Barometer"))

tab_log_reg_before_barometer <- subset(tab_log_reg_before, Sensor_Type %in% c("Barometer"))
tab_log_reg_after_barometer <- subset(tab_log_reg_after, Sensor_Type %in% c("Barometer"))



# # log regression
# log_model_barometer <- glm(shared ~ phase + Device_Type,
#                             data=tab_log_reg_barometer, family=binomial())
# 
# exp(coef(log_model_barometer))  
# summary(log_model_barometer)

# Active Task Sensor
tab_log_reg_before_active <- subset(tab_log_reg_before, Sensor_Type %in% c("Active Task Sensor"))
tab_log_reg_after_active <- subset(tab_log_reg_after, Sensor_Type %in% c("Active Task Sensor"))

# # log regression
# log_model_active <- glm(shared ~ phase + Device_Type,
#                            data=tab_log_reg_active, family=binomial())
# 
# exp(coef(log_model_active))  
# summary(log_model_active)


# all sensor types - model with baseline var
# Accelerometer
# log regression
# P1
log_model_accelerometer_all_before <- glm(shared ~ Device_Type + Age + race_hispanic +
                                     income_level_recode + education_level_recode,
                               data=tab_log_reg_before_accelerometer, family=binomial())

OR_Accelerometer_before <- as.data.frame(exp(coef(log_model_accelerometer_all_before)))
summary(log_model_accelerometer_all_before)
confint.default(log_model_accelerometer_all_before)

# FDR correction
p.adjust_Accelerometer_before <- as.data.frame(p.adjust(summary(log_model_accelerometer_all_before)$coefficients[,4], method = "fdr"))

# P2
log_model_accelerometer_all_after <- glm(shared ~ Device_Type + Age + race_hispanic +
                                            income_level_recode + education_level_recode,
                                          data=tab_log_reg_after_accelerometer, family=binomial())

OR_Accelerometer_after <- as.data.frame(exp(coef(log_model_accelerometer_all_after)))
summary(log_model_accelerometer_all_after)
confint.default(log_model_accelerometer_all_after)

# FDR correction
p.adjust_Accelerometer_after <- as.data.frame(p.adjust(summary(log_model_accelerometer_all_after)$coefficients[,4], method = "fdr"))





# Accessibility
tab_log_reg_accessibility <- subset(tab_log_reg, Sensor_Type %in% c("Accessibility"))

# log regression
log_model_accessibility_all <- glm(shared ~ phase + Age + race_recode + Hispanic +
                                     income_level_recode + education_level_recode,
                                   data=tab_log_reg_accessibility, family=binomial())

OR_Accessibility <- as.data.frame(exp(coef(log_model_accessibility_all)))
summary(log_model_accessibility_all)

# FDR correction
p.adjust_Accessibility <- as.data.frame(p.adjust(summary(log_model_accessibility_all)$coefficients[,4], method = "fdr", n = 28))


# Altimeter
tab_log_reg_altimeter <- subset(tab_log_reg, Sensor_Type %in% c("Altimeter"))

# log regression
log_model_altimeter_all <- glm(shared ~ phase + Age + race_recode + Hispanic +
                                 income_level_recode + education_level_recode,
                               data=tab_log_reg_altimeter, family=binomial())

OR_Altimeter <- as.data.frame(exp(coef(log_model_altimeter_all)))
summary(log_model_altimeter_all)

# FDR correction
p.adjust_Altimeter <- as.data.frame(p.adjust(summary(log_model_altimeter_all)$coefficients[,4], method = "fdr", n = 28))



# Ambient Sensor
tab_log_reg_ambient <- subset(tab_log_reg, Sensor_Type %in% c("Ambient Sensor"))

# log regression
log_model_ambient_all <- glm(shared ~ phase + Age + race_recode + Hispanic +
                               income_level_recode + education_level_recode,
                               data=tab_log_reg_ambient, family=binomial())

OR_Ambient <- as.data.frame(exp(coef(log_model_ambient_all)))
summary(log_model_ambient_all)

# FDR correction
p.adjust_Ambient <- as.data.frame(p.adjust(summary(log_model_ambient_all)$coefficients[,4], method = "fdr", n = 28))


# Baromter
# log regression
# P1
log_model_barometer_all_before <- glm(shared ~ Device_Type + Age + race_hispanic + 
                               income_level_recode + education_level_recode,
                             data=tab_log_reg_before_barometer, family=binomial())

OR_barometer_before <- as.data.frame(exp(coef(log_model_barometer_all_before)))
summary(log_model_barometer_all_before)

# FDR correction
p.adjust_barometer_before <- as.data.frame(p.adjust(summary(log_model_barometer_all_before)$coefficients[,4], method = "fdr"))


# P2
log_model_barometer_all_after <- glm(shared ~ Device_Type + Age + race_hispanic + 
                                     income_level_recode + education_level_recode,
                                   data=tab_log_reg_after_barometer, family=binomial())

OR_barometer_after <- as.data.frame(exp(coef(log_model_barometer_all_after)))
summary(log_model_barometer_all_after)

# FDR correction
p.adjust_barometer_after <- as.data.frame(p.adjust(summary(log_model_barometer_all_after)$coefficients[,4], method = "fdr"))







# Battery State
tab_log_reg_battery <- subset(tab_log_reg, Sensor_Type %in% c("Battery State"))

# log regression
log_model_battery_all <- glm(shared ~ phase + Age + race_recode + Hispanic +
                               income_level_recode + education_level_recode,
                             data=tab_log_reg_battery, family=binomial())

OR_Battery <- as.data.frame(exp(coef(log_model_battery_all))) 
summary(log_model_battery_all)

# FDR correction
p.adjust_Battery <- as.data.frame(p.adjust(summary(log_model_battery_all)$coefficients[,4], method = "fdr", n = 28))


# Active task sensor
# log regression
# P1
log_model_active_all_before <- glm(shared ~ Device_Type + Age + race_hispanic + 
                                     income_level_recode + education_level_recode,
                                   data=tab_log_reg_before_active, family=binomial())

OR_Active_before <- as.data.frame(exp(coef(log_model_active_all_before)))
summary(log_model_active_all_before)

confint.default(log_model_active_all_before)


# FDR correction
p.adjust_Active_before <- as.data.frame(p.adjust(summary(log_model_active_all_before)$coefficients[,4], method = "fdr"))


# P2
log_model_active_all_after <- glm(shared ~ Device_Type + Age + race_hispanic + 
                                    income_level_recode + education_level_recode,
                                  data=tab_log_reg_after_active, family=binomial())

OR_Active_after <- as.data.frame(exp(coef(log_model_active_all_after)))
summary(log_model_active_all_after)

# FDR correction
p.adjust_Active_after <- as.data.frame(p.adjust(summary(log_model_active_all_after)$coefficients[,4], method = "fdr"))


# Bluetooth
tab_log_reg_bluetooth <- subset(tab_log_reg, Sensor_Type %in% c("Bluetooth"))

# log regression
log_model_bluetooth_all <- glm(shared ~ phase + Age + race_recode + Hispanic +
                               income_level_recode + education_level_recode,
                             data=tab_log_reg_bluetooth, family=binomial())

OR_Bluetooth <- as.data.frame(exp(coef(log_model_bluetooth_all)))  
summary(log_model_bluetooth_all)

# FDR correction
p.adjust_Bluetooth <- as.data.frame(p.adjust(summary(log_model_bluetooth_all)$coefficients[,4], method = "fdr", n = 28))



# Compass
tab_log_reg_compass <- subset(tab_log_reg, Sensor_Type %in% c("Compass"))

# log regression
# P1
log_model_compass_all_before <- glm(shared ~ Device_Type + Age + race_hispanic + 
                                        income_level_recode + education_level_recode,
                                      data=tab_log_reg_before_compass, family=binomial())

OR_compass_before <- as.data.frame(exp(coef(log_model_compass_all_before)))
summary(log_model_compass_all_before)

# FDR correction
p.adjust_compass_before <- as.data.frame(p.adjust(summary(log_model_compass_all_before)$coefficients[,4], method = "fdr"))


# P2
log_model_compass_all_after <- glm(shared ~ Device_Type + Age + race_hispanic + 
                                       income_level_recode + education_level_recode,
                                     data=tab_log_reg_after_compass, family=binomial())

OR_compass_after <- as.data.frame(exp(coef(log_model_compass_all_after)))
summary(log_model_compass_all_after)

# FDR correction
p.adjust_compass_after <- as.data.frame(p.adjust(summary(log_model_compass_all_after)$coefficients[,4], method = "fdr"))





# Game Rotation Vector Sensor
tab_log_reg_grv <- subset(tab_log_reg, Sensor_Type %in% c("Game Rotation Vector Sensor"))

# log regression
log_model_grv_all <- glm(shared ~ phase + Age + race_recode + Hispanic +
                               income_level_recode + education_level_recode,
                             data=tab_log_reg_grv, family=binomial())

OR_grv <- as.data.frame(exp(coef(log_model_grv_all)))  
summary(log_model_grv_all)


# FDR correction
p.adjust_grv <- as.data.frame(p.adjust(summary(log_model_grv_all)$coefficients[,4], method = "fdr", n = 28))


# Geomagnetic Rotation Vector Sensor
tab_log_reg_grv2 <- subset(tab_log_reg, Sensor_Type %in% c("Geomagnetic Rotation Vector Sensor"))

# log regression
log_model_grv2_all <- glm(shared ~ phase + Age + race_recode + Hispanic +
                           income_level_recode + education_level_recode,
                         data=tab_log_reg_grv2, family=binomial())

OR_grv2 <- as.data.frame(exp(coef(log_model_grv2_all)))  
summary(log_model_grv2_all)

# FDR correction
p.adjust_grv2 <- as.data.frame(p.adjust(summary(log_model_grv2_all)$coefficients[,4], method = "fdr", n = 28))


# GPS
# log regression
# P1
log_model_gps_all_before <- glm(shared ~ Device_Type + Age + race_hispanic + 
                                      income_level_recode + education_level_recode,
                                    data=tab_log_reg_before_gps, family=binomial())

OR_gps_before <- as.data.frame(exp(coef(log_model_gps_all_before)))
summary(log_model_gps_all_before)

# FDR correction
p.adjust_gps_before <- as.data.frame(p.adjust(summary(log_model_gps_all_before)$coefficients[,4], method = "fdr"))


# P2
log_model_gps_all_after <- glm(shared ~ Device_Type + Age + race_hispanic + 
                                     income_level_recode + education_level_recode,
                                   data=tab_log_reg_after_gps, family=binomial())

OR_gps_after <- as.data.frame(exp(coef(log_model_gps_all_after)))
summary(log_model_gps_all_after)

# FDR correction
p.adjust_gps_after <- as.data.frame(p.adjust(summary(log_model_gps_all_after)$coefficients[,4], method = "fdr"))


# Gravity Sensor
tab_log_reg_gravity <- subset(tab_log_reg, Sensor_Type %in% c("Gravity Sensor"))

# log regression
log_model_gravity_all <- glm(shared ~ phase + Age + race_recode + Hispanic +
                           income_level_recode + education_level_recode,
                         data=tab_log_reg_gravity, family=binomial())

OR_Gravity <- as.data.frame(exp(coef(log_model_gravity_all)))  
summary(log_model_gravity_all)

# FDR correction
p.adjust_Gravity <- as.data.frame(p.adjust(summary(log_model_gravity_all)$coefficients[,4], method = "fdr", n = 28))


# Gyroscope
# log regression
# P1
log_model_gyroscope_all_before <- glm(shared ~ Device_Type + Age + race_hispanic + 
                                  income_level_recode + education_level_recode,
                                data=tab_log_reg_before_gyroscope, family=binomial())

OR_gyroscope_before <- as.data.frame(exp(coef(log_model_gyroscope_all_before)))
summary(log_model_gyroscope_all_before)

# FDR correction
p.adjust_gyroscope_before <- as.data.frame(p.adjust(summary(log_model_gyroscope_all_before)$coefficients[,4], method = "fdr"))


# P2
log_model_gyroscope_all_after <- glm(shared ~ Device_Type + Age + race_hispanic + 
                                 income_level_recode + education_level_recode,
                               data=tab_log_reg_after_gyroscope, family=binomial())

OR_gyroscope_after <- as.data.frame(exp(coef(log_model_gyroscope_all_after)))
summary(log_model_gyroscope_all_after)

# FDR correction
p.adjust_gyroscope_after <- as.data.frame(p.adjust(summary(log_model_gyroscope_all_after)$coefficients[,4], method = "fdr"))


# Historical Activity Data
tab_log_reg_historical <- subset(tab_log_reg, Sensor_Type %in% c("Historical Activity Data"))

# log regression
log_model_historical_all <- glm(shared ~ phase + Age + race_recode + Hispanic +
                               income_level_recode + education_level_recode,
                             data=tab_log_reg_historical, family=binomial())

OR_Historical <- as.data.frame(exp(coef(log_model_historical_all))) 
summary(log_model_historical_all)

# FDR correction
p.adjust_Historical <- as.data.frame(p.adjust(summary(log_model_historical_all)$coefficients[,4], method = "fdr", n = 28))


# Humidity Sensor
tab_log_reg_humidity <- subset(tab_log_reg, Sensor_Type %in% c("Humidity Sensor"))

# log regression
log_model_humidity_all <- glm(shared ~ phase + Age + race_recode + Hispanic +
                                  income_level_recode + education_level_recode,
                                data=tab_log_reg_humidity, family=binomial())

OR_Humidity <- as.data.frame(exp(coef(log_model_humidity_all)))  
summary(log_model_humidity_all)

# FDR correction
p.adjust_Humidity <- as.data.frame(p.adjust(summary(log_model_humidity_all)$coefficients[,4], method = "fdr", n = 28))


# Photodetector
tab_log_reg_photodetector <- subset(tab_log_reg, Sensor_Type %in% c("Photodetector"))

# log regression
log_model_photodetector_all <- glm(shared ~ phase + Age + race_recode + Hispanic +
                                income_level_recode + education_level_recode,
                              data=tab_log_reg_photodetector, family=binomial())

OR_Photodetector <- as.data.frame(exp(coef(log_model_photodetector_all)))  
summary(log_model_photodetector_all)

# FDR correction
p.adjust_Photodetector <- as.data.frame(p.adjust(summary(log_model_photodetector_all)$coefficients[,4], method = "fdr", n = 28))


# Linear Accelerator
tab_log_reg_la <- subset(tab_log_reg, Sensor_Type %in% c("Linear Accelerator"))

# log regression
log_model_la_all <- glm(shared ~ phase + Age + race_recode + Hispanic +
                                     income_level_recode + education_level_recode,
                                   data=tab_log_reg_la, family=binomial())

OR_Linear <- as.data.frame(exp(coef(log_model_la_all)))  
summary(log_model_la_all)

# FDR correction
p.adjust_Linear <- as.data.frame(p.adjust(summary(log_model_la_all)$coefficients[,4], method = "fdr", n = 28))



# Magnetometer
# log regression
# P1
log_model_magnetometer_all_before <- glm(shared ~ Device_Type + Age + race_hispanic + 
                                        income_level_recode + education_level_recode,
                                      data=tab_log_reg_before_magnetometer, family=binomial())

OR_magnetometer_before <- as.data.frame(exp(coef(log_model_magnetometer_all_before)))
summary(log_model_magnetometer_all_before)

# FDR correction
p.adjust_magnetometer_before <- as.data.frame(p.adjust(summary(log_model_magnetometer_all_before)$coefficients[,4], method = "fdr"))


# P2
log_model_magnetometer_all_after <- glm(shared ~ Device_Type + Age + race_hispanic + 
                                       income_level_recode + education_level_recode,
                                     data=tab_log_reg_after_magnetometer, family=binomial())

OR_magnetometer_after <- as.data.frame(exp(coef(log_model_magnetometer_all_after)))
summary(log_model_magnetometer_all_after)

# FDR correction
p.adjust_magnetometer_after <- as.data.frame(p.adjust(summary(log_model_magnetometer_all_after)$coefficients[,4], method = "fdr"))



# Microphone
# P1
log_model_microphone_all_before <- glm(shared ~ Device_Type + Age + race_hispanic + 
                                           income_level_recode + education_level_recode,
                                         data=tab_log_reg_before_microphone, family=binomial())

OR_microphone_before <- as.data.frame(exp(coef(log_model_microphone_all_before)))
summary(log_model_microphone_all_before)

# FDR correction
p.adjust_microphone_before <- as.data.frame(p.adjust(summary(log_model_microphone_all_before)$coefficients[,4], method = "fdr"))


# P2
log_model_microphone_all_after <- glm(shared ~ Device_Type + Age + race_hispanic + 
                                          income_level_recode + education_level_recode,
                                        data=tab_log_reg_after_microphone, family=binomial())

OR_microphone_after <- as.data.frame(exp(coef(log_model_microphone_all_after)))
summary(log_model_microphone_all_after)

# FDR correction
p.adjust_microphone_after <- as.data.frame(p.adjust(summary(log_model_microphone_all_after)$coefficients[,4], method = "fdr"))




# Pedometer
# P1
log_model_pedometer_all_before <- glm(shared ~ Device_Type + Age + race_hispanic + 
                                         income_level_recode + education_level_recode,
                                       data=tab_log_reg_before_pedometer, family=binomial())

OR_pedometer_before <- as.data.frame(exp(coef(log_model_pedometer_all_before)))
summary(log_model_pedometer_all_before)

# FDR correction
p.adjust_pedometer_before <- as.data.frame(p.adjust(summary(log_model_pedometer_all_before)$coefficients[,4], method = "fdr"))


# P2
log_model_pedometer_all_after <- glm(shared ~ Device_Type + Age + race_hispanic + 
                                        income_level_recode + education_level_recode,
                                      data=tab_log_reg_after_pedometer, family=binomial())

OR_pedometer_after <- as.data.frame(exp(coef(log_model_pedometer_all_after)))
summary(log_model_pedometer_all_after)

# FDR correction
p.adjust_pedometer_after <- as.data.frame(p.adjust(summary(log_model_pedometer_all_after)$coefficients[,4], method = "fdr"))




# Proximity Sensor
tab_log_reg_proximity <- subset(tab_log_reg, Sensor_Type %in% c("Proximity Sensor"))

# log regression
log_model_proximity_all <- glm(shared ~ phase + Age + race_recode + Hispanic +
                          income_level_recode + education_level_recode,
                        data=tab_log_reg_proximity, family=binomial())

OR_Proximity <- as.data.frame(exp(coef(log_model_proximity_all)))  
summary(log_model_proximity_all)

# FDR correction
p.adjust_Proximity <- as.data.frame(p.adjust(summary(log_model_proximity_all)$coefficients[,4], method = "fdr", n = 28))



# Rotation Vector Sensor
tab_log_reg_rotation <- subset(tab_log_reg, Sensor_Type %in% c("Rotation Vector Sensor"))


log_model_rotation_all <- glm(shared ~ phase + Age + race_recode + Hispanic +
                                 income_level_recode + education_level_recode,
                               data=tab_log_reg_rotation, family=binomial())

OR_Rotation <- as.data.frame(exp(coef(log_model_rotation_all)))  
summary(log_model_rotation_all)

# FDR correction
p.adjust_Rotation <- as.data.frame(p.adjust(summary(log_model_rotation_all)$coefficients[,4], method = "fdr", n = 28))



# SMS
tab_log_reg_sms <- subset(tab_log_reg, Sensor_Type %in% c("SMS"))


log_model_sms_all <- glm(shared ~ phase + Age + race_recode + Hispanic +
                                income_level_recode + education_level_recode,
                              data=tab_log_reg_sms, family=binomial())

OR_sms <- as.data.frame(exp(coef(log_model_sms_all)))  
summary(log_model_sms_all)

# FDR correction
p.adjust_sms <- as.data.frame(p.adjust(summary(log_model_sms_all)$coefficients[,4], method = "fdr", n = 28))



# Step Detector 
tab_log_reg_sd <- subset(tab_log_reg, Sensor_Type %in% c("Step Detector"))


log_model_sd_all <- glm(shared ~ phase + Age + race_recode + Hispanic +
                           income_level_recode + education_level_recode,
                         data=tab_log_reg_sd, family=binomial())

OR_sd <- as.data.frame(exp(coef(log_model_sd_all)))  
summary(log_model_sd_all)

# FDR correction
p.adjust_sd <- as.data.frame(p.adjust(summary(log_model_sd_all)$coefficients[,4], method = "fdr", n = 28))



# Tilt Sensor
tab_log_reg_tilt <- subset(tab_log_reg, Sensor_Type %in% c("Tilt Sensor"))


log_model_tilt_all <- glm(shared ~ phase + Age + race_recode + Hispanic +
                          income_level_recode + education_level_recode,
                        data=tab_log_reg_tilt, family=binomial())

OR_tilt <- as.data.frame(exp(coef(log_model_tilt_all)))  
summary(log_model_tilt_all)


# FDR correction
p.adjust_tilt <- as.data.frame(p.adjust(summary(log_model_tilt_all)$coefficients[,4], method = "fdr", n = 28))


# WiFi
tab_log_reg_wifi <- subset(tab_log_reg, Sensor_Type %in% c("WiFi"))


log_model_wifi_all <- glm(shared ~ phase + Age + race_recode + Hispanic +
                            income_level_recode + education_level_recode,
                          data=tab_log_reg_wifi, family=binomial())

OR_wifi <- as.data.frame(exp(coef(log_model_wifi_all)))  
summary(log_model_wifi_all)

# FDR correction
p.adjust_wifi <- as.data.frame(p.adjust(summary(log_model_wifi_all)$coefficients[,4], method = "fdr", n = 28))

                                     
############################# Cohort analysis Android vs iPhone ##################
# subset a dataframe with participant-sensor type pair
tab_all_metadata <- as.data.frame(table(all_metadata$`User ID`, all_metadata$Sensor_Type))
all_metadata_cohort <- tab_all_metadata[! (tab_all_metadata$Freq == 0),]

split <- strsplit(as.vector(all_metadata_cohort$Var2), split = "-")
all_metadata_cohort$Device_Type <- sapply(split, function(x) x[1])
all_metadata_cohort$Sensor_Type <- sapply(split, function(x) x[4])

id_sensor_pair <- as.data.frame(table(all_metadata_cohort$Var1, all_metadata_cohort$Device_Type))
id_sensor_pair <- id_sensor_pair[! (id_sensor_pair$Freq == 0),]
id_sensor_pair <- subset(id_sensor_pair, select = - c(Freq))
colnames(id_sensor_pair)[1] <- "User ID"
colnames(id_sensor_pair)[2] <- "Device_Type"

# ppl who submitted sensor data only: 10583 
# Android:4700 (44.4%)
# iPhone: 5883 (55.6%)
# did not submit any survey data: 830
id_sensor_pair_sensor <- merge(id_sensor_pair, all_survey_early_date, by = "User ID", all.x = T)
android_iphone_sensor <- as.data.frame(table(id_sensor_pair_sensor$Device_Type))
sum(is.na(id_sensor_pair_sensor$Start_date))

# ppl who submitted survey data only: 9938 
# Android:4069 (40.9%)
# iPhone: 5684 (57.2%)
# did not submit any sensor data: 185 (1.9%)
id_sensor_pair_survey <- merge(id_sensor_pair, all_survey_early_date, by = "User ID", all.y = T)
android_iphone_survey <- as.data.frame(table(id_sensor_pair_survey$Device_Type))
sum(is.na(id_sensor_pair_survey$Device_Type))

# ppl who submitted both survey and sensor data : 9753 
# Android: 4069 (41.7%)
# iPhone: 5684 (58.3%)
id_sensor_pair_both <- merge(id_sensor_pair, all_survey_early_date, by = "User ID")
android_iphone_both <- as.data.frame(table(id_sensor_pair_both$Device_Type))

# full cohort : 10768
# Android:4700 (43.6%)
# iPhone: 5883 (54.6%)
# did not submit any sensor data: 185 (1.7%)
id_sensor_pair_full <- merge(id_sensor_pair, all_survey_early_date, by = "User ID", all = T)
android_iphone_full <- as.data.frame(table(id_sensor_pair_full$Device_Type))

# test Android vs iPhone distribution in baseline missingness: 4053
# Android: 1405 (34.7%)
# iPhone:2584 (63.8%)

missingness_device <- id_sensor_pair_full[which(!(id_sensor_pair_full$`User ID` %in% wash_survey_baseline_cleaned_duprm$`User ID`)),]
android_iphone_missing <- as.data.frame(table(missingness_device$Device_Type))

                                          
# Plot: Is the data coming into the study on a consistent basis 
# Active data stream
# first split cohort by recruited in phase 1 vs 2
all_survey_before <- subset(all_survey, `User ID` %in% day_in_study_all_before$`User ID`)
all_survey_after <- subset(all_survey, `User ID` %in% day_in_study_all_after$`User ID`)

all_survey_last_date_before <- subset(all_survey_last_date, `User ID` %in% day_in_study_all_before$`User ID`)
all_survey_last_date_after <- subset(all_survey_last_date, `User ID` %in% day_in_study_all_after$`User ID`)

# phase 1
# obtain info on participant recruitment
incoming_data_before <- as.data.frame(table(all_survey_early_date$Start_date))
colnames(incoming_data_before)[1] <- "Date"
colnames(incoming_data_before)[2] <- "Num_incoming"
incoming_data_before$Date <- as.Date(incoming_data_before$Date)

# integrate info on participant attrition
outgoing_data_before <- as.data.frame(table(all_survey_last_date_before$Y_M_D))
colnames(outgoing_data_before)[1] <- "Date"
colnames(outgoing_data_before)[2] <- "Num_outgoing"
outgoing_data_before$Date <- as.Date(outgoing_data_before$Date)
# data was still submitted on the last day, so the outgoing date = last date + 1 day
outgoing_data_before$Date <- outgoing_data_before$Date + 1

incoming_data_before <- merge(incoming_data_before,outgoing_data_before, all = T)
incoming_data_before[is.na(incoming_data_before)] <- 0

# obtain num of actual active participant each day
incoming_data_before <-
  incoming_data_before %>%
  mutate(Num_active = cumsum(Num_incoming - Num_outgoing))

# integrate info on participants contributing to data
contributing_data_before <- as.data.frame(table(all_survey_before$`date taken`, all_survey_before$`User ID`))
contributing_data_before$Freq[contributing_data_before$Freq > 1] <- 1
contributing_data_before <- subset(contributing_data_before, Freq == 1)
contributing_data_before <- as.data.frame(table(contributing_data_before$Var1))

colnames(contributing_data_before)[1] <- "Date"
colnames(contributing_data_before)[2] <- "Num_contributed_data"
contributing_data_before$Date <- as.Date(contributing_data_before$Date)

incoming_data_before <- merge(incoming_data_before,contributing_data_before, all = T)
incoming_data_before[is.na(incoming_data_before)] <- 0
                                          
incoming_data_before <- subset(incoming_data_before, Date < as.Date("2020-08-30"))

# obtain proportion of active participants
incoming_data_before$Prop <- round(incoming_data_before$Num_contributed_data / incoming_data_before$Num_active * 100,digits = 1)

# plot active data stream - phase 1
p_active_before <- 
  ggplot(incoming_data_before, aes(x=Date, y=Prop)) +
  geom_line(group = 1, size=0.3) + 
  geom_point(size=0.8) +
  theme_bw() +
  ylab("Proportion of Active Participants Submitting Active Data") +
  xlab("Date") +
  scale_x_date(date_breaks = "month", labels = label_date_short())  +
  geom_ma(ma_fun = SMA, n = 7,size = 1)

# phase 2
# obtain info on participant recruitment
incoming_data_after <- as.data.frame(table(all_survey_early_date$Start_date))
colnames(incoming_data_after)[1] <- "Date"
colnames(incoming_data_after)[2] <- "Num_incoming"
incoming_data_after$Date <- as.Date(incoming_data_after$Date)

# integrate info on participant attrition
outgoing_data_after <- as.data.frame(table(all_survey_last_date_after$Y_M_D))
colnames(outgoing_data_after)[1] <- "Date"
colnames(outgoing_data_after)[2] <- "Num_outgoing"
outgoing_data_after$Date <- as.Date(outgoing_data_after$Date)
# data was still submitted on the last day, so the outgoing date = last date + 1 day
outgoing_data_after$Date <- outgoing_data_after$Date + 1

incoming_data_after <- merge(incoming_data_after,outgoing_data_after, all = T)
incoming_data_after[is.na(incoming_data_after)] <- 0

incoming_data_after <- subset(incoming_data_after, Date >= as.Date("2020-08-30"))


# obtain num of actual active participant each day
incoming_data_after <-
  incoming_data_after %>%
  mutate(Num_active = cumsum(Num_incoming - Num_outgoing))

# integrate info on participants contributing to data
contributing_data_after <- as.data.frame(table(all_survey_after$`date taken`, all_survey_after$`User ID`))
contributing_data_after$Freq[contributing_data_after$Freq > 1] <- 1
contributing_data_after <- subset(contributing_data_after, Freq == 1)
contributing_data_after <- as.data.frame(table(contributing_data_after$Var1))

colnames(contributing_data_after)[1] <- "Date"
colnames(contributing_data_after)[2] <- "Num_contributed_data"
contributing_data_after$Date <- as.Date(contributing_data_after$Date)

incoming_data_after <- merge(incoming_data_after,contributing_data_after, all = T)
incoming_data_after[is.na(incoming_data_after)] <- 0


# obtain proportion of active participants
incoming_data_after$Prop <- round(incoming_data_after$Num_contributed_data / incoming_data_after$Num_active * 100,digits = 1)
incoming_data_after$Prop[incoming_data_after$Date == as.Date("2020-08-31")] <- 100.0
incoming_data_after$Prop[incoming_data_after$Date == as.Date("2021-07-10")] <- 48.3
incoming_data_after$Prop[incoming_data_after$Date == as.Date("2021-08-03")] <- 36.8 
incoming_data_after$Prop[incoming_data_after$Date == as.Date("2021-12-15")] <- 46.3


# plot active data stream - phase 2
p_active_after <- 
  ggplot(incoming_data_after, aes(x=Date, y=Prop)) +
  geom_line(group = 1, size=0.3) + 
  geom_point(size=0.8) +
  theme_bw() +
  ylab("Proportion of Active Participants Submitting Active Data") +
  xlab("Date") +
  scale_x_date(date_breaks = "month", labels = label_date_short()) +
  geom_ma(ma_fun = SMA, n = 7,size = 1)


# Passive data stream
# first split cohort by recruited in phase 1 vs 2
all_metadata_before <- subset(all_metadata, `User ID` %in% day_in_study_all_before$`User ID`)
all_metadata_after <- subset(all_metadata, `User ID` %in% day_in_study_all_after$`User ID`)

all_metadata_last_date_before <- subset(all_metadata_last_date, `User ID` %in% day_in_study_all_before$`User ID`)
all_metadata_last_date_after <- subset(all_metadata_last_date, `User ID` %in% day_in_study_all_after$`User ID`)

# phase 1
# obtain info on participant recruitment
incoming_data_before_sensor <- as.data.frame(table(all_metadata_first_date$`Sensor first date`))
colnames(incoming_data_before_sensor)[1] <- "Date"
colnames(incoming_data_before_sensor)[2] <- "Num_incoming"
incoming_data_before_sensor$Date <- as.Date(incoming_data_before_sensor$Date)

# integrate info on participant attrition
outgoing_data_before_sensor <- as.data.frame(table(all_metadata_last_date_before$`Sensor last date`))
colnames(outgoing_data_before_sensor)[1] <- "Date"
colnames(outgoing_data_before_sensor)[2] <- "Num_outgoing"
outgoing_data_before_sensor$Date <- as.Date(outgoing_data_before_sensor$Date)
# data was still submitted on the last day, so the outgoing date = last date + 1 day
outgoing_data_before_sensor$Date <- outgoing_data_before_sensor$Date + 1

incoming_data_before_sensor <- merge(incoming_data_before_sensor,outgoing_data_before_sensor, all = T)
incoming_data_before_sensor[is.na(incoming_data_before_sensor)] <- 0

# obtain num of actual active participant each day
incoming_data_before_sensor <-
  incoming_data_before_sensor %>%
  mutate(Num_active = cumsum(Num_incoming - Num_outgoing))

# integrate info on participants contributing to data
contributing_data_before_sensor <- as.data.frame(table(all_metadata_before$Record_creation_ts, all_metadata_before$`User ID`))
contributing_data_before_sensor$Freq[contributing_data_before_sensor$Freq > 1] <- 1
contributing_data_before_sensor <- subset(contributing_data_before_sensor, Freq == 1)
contributing_data_before_sensor <- as.data.frame(table(contributing_data_before_sensor$Var1))

colnames(contributing_data_before_sensor)[1] <- "Date"
colnames(contributing_data_before_sensor)[2] <- "Num_contributed_data"
contributing_data_before_sensor$Date <- as.Date(contributing_data_before_sensor$Date)

incoming_data_before_sensor <- merge(incoming_data_before_sensor,contributing_data_before_sensor, all = T)
incoming_data_before_sensor[is.na(incoming_data_before_sensor)] <- 0

incoming_data_before_sensor <- subset(incoming_data_before_sensor, Date < as.Date("2020-08-30"))

# obtain proportion of active participants
incoming_data_before_sensor$Prop <- round(incoming_data_before_sensor$Num_contributed_data / incoming_data_before_sensor$Num_active * 100,digits = 1)

# # plot passive data stream - phase 1
p_active_before_sensor <- 
  ggplot(incoming_data_before_sensor, aes(x=Date, y=Prop)) +
  geom_line(group = 1, size=0.3) + 
  geom_point(size=0.8) +
  theme_bw() +
  ylab("Proportion of Active Participants Submitting Passive Data") +
  xlab("Date") +
  scale_x_date(date_breaks = "month", labels = label_date_short())  +
  geom_ma(ma_fun = SMA, n = 7,size = 1)

# phase 2
# obtain info on participant recruitment
incoming_data_after_sensor <- as.data.frame(table(all_metadata_first_date$`Sensor first date`))
colnames(incoming_data_after_sensor)[1] <- "Date"
colnames(incoming_data_after_sensor)[2] <- "Num_incoming"
incoming_data_after_sensor$Date <- as.Date(incoming_data_after_sensor$Date)

# integrate info on participant attrition
outgoing_data_after_sensor <- as.data.frame(table(all_metadata_last_date_after$`Sensor last date`))
colnames(outgoing_data_after_sensor)[1] <- "Date"
colnames(outgoing_data_after_sensor)[2] <- "Num_outgoing"
outgoing_data_after_sensor$Date <- as.Date(outgoing_data_after_sensor$Date)
# data was still submitted on the last day, so the outgoing date = last date + 1 day
outgoing_data_after_sensor$Date <- outgoing_data_after_sensor$Date + 1

incoming_data_after_sensor <- merge(incoming_data_after_sensor,outgoing_data_after_sensor, all = T)
incoming_data_after_sensor[is.na(incoming_data_after_sensor)] <- 0

incoming_data_after_sensor <- subset(incoming_data_after_sensor, Date >= as.Date("2020-08-30"))

# obtain num of actual active participant each day
incoming_data_after_sensor <-
  incoming_data_after_sensor %>%
  mutate(Num_active = cumsum(Num_incoming - Num_outgoing))

# integrate info on participants contributing to data
contributing_data_after_sensor <- as.data.frame(table(all_metadata_after$Record_creation_ts, all_metadata_after$`User ID`))
contributing_data_after_sensor$Freq[contributing_data_after_sensor$Freq > 1] <- 1
contributing_data_after_sensor <- subset(contributing_data_after_sensor, Freq == 1)
contributing_data_after_sensor <- as.data.frame(table(contributing_data_after_sensor$Var1))

colnames(contributing_data_after_sensor)[1] <- "Date"
colnames(contributing_data_after_sensor)[2] <- "Num_contributed_data"
contributing_data_after_sensor$Date <- as.Date(contributing_data_after_sensor$Date)

incoming_data_after_sensor <- merge(incoming_data_after_sensor,contributing_data_after_sensor, all = T)
incoming_data_after_sensor[is.na(incoming_data_after_sensor)] <- 0

# obtain proportion of active participants
incoming_data_after_sensor$Prop <- round(incoming_data_after_sensor$Num_contributed_data / incoming_data_after_sensor$Num_active * 100,digits = 1)
incoming_data_after_sensor$Prop[incoming_data_after_sensor$Date == as.Date("2020-08-31")] <- 100.0
incoming_data_after_sensor$Prop[incoming_data_after_sensor$Date == as.Date("2021-08-06")] <- 70.3
incoming_data_after_sensor$Prop[incoming_data_after_sensor$Date == as.Date("2022-01-09")] <- 65.6

# plot passive data stream - phase 2
p_active_after_sensor <- 
  ggplot(incoming_data_after_sensor, aes(x=Date, y=Prop)) +
  geom_line(group = 1, size=0.3) + 
  geom_point(size=0.8) +
  theme_bw() +
  ylab("Proportion of Active Participants Submitting Passive Data") +
  xlab("Date") +
  scale_x_date(date_breaks = "month", labels = label_date_short())  +
  geom_ma(ma_fun = SMA, n = 7,size = 1)


# the 4 plots produced previously were individual plots
# now produce combined plots (Phase 1 active+passive, Phase 2 active+passive)
# phase 1: active+passive
p.before <- 
  ggplot(incoming_data_before, aes(x=Date, y=Prop))+
  geom_line(group = 1) +
  geom_ma(data = incoming_data_before, ma_fun = SMA, n = 7, color = "black", size = 1.5) +
  geom_line(data = incoming_data_before_sensor,
            aes(x=Date, y=Prop),
            group=1, color="red")+
  geom_ma(data = incoming_data_before_sensor, ma_fun = SMA, n = 7, color = "red", size = 1.5) +
  geom_point(size=0.8)+
  theme_bw()+
  ylab("Percentage")+
  xlab("Date")+
  ylim(0,100)+
  scale_x_date(date_breaks = "month", labels = label_date_short()) +
  theme(text = element_text(size = 20)) 

# phase 2: active+passive
p.after <- 
  ggplot(incoming_data_after, aes(x=Date, y=Prop))+
  geom_line(group = 1) +
  geom_ma(data = incoming_data_after, ma_fun = SMA, n = 7, color = "black", size = 1.5) +
  geom_line(data = incoming_data_after_sensor,
            aes(x=Date, y=Prop),
            group=1, color="red")+
  geom_ma(data = incoming_data_after_sensor, ma_fun = SMA, n = 7, color = "red", size = 1.5) +
  geom_point(size=0.8)+
  theme_bw()+
  ylab("Percentage")+
  xlab("Date")+
  ylim(0,100)+
  scale_x_date(date_breaks = "3 months", labels = label_date_short()) +
  theme(text = element_text(size = 20)) +
   annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = as.Date("2021-03-06"), xmax = as.Date("2021-04-14"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = as.Date("2021-11-02"), xmax = as.Date("2021-12-06"),
           ymin = -Inf, ymax = Inf) 
  
# obtain median and IQR of % participants shared any data 
summary(incoming_data_before$Prop)
summary(incoming_data_before_sensor$Prop)
summary(incoming_data_after$Prop)
summary(incoming_data_after_sensor$Prop)



# plots with just the moving average
# phase 1
p.before.ma <- 
ggplot(incoming_data_before, aes(x=Date, y=Prop))+
  #geom_line(group = 1) +
  geom_ma(data = incoming_data_before, ma_fun = SMA, n = 7, size = 1.5) +
  # geom_line(data = incoming_data_before_sensor,
  #           aes(x=Date, y=Prop),
  #           group=1, color="red")+
  geom_ma(data = incoming_data_before_sensor, ma_fun = SMA, n = 7, color = "red", size = 1.5) +
  #geom_point(size=0.8)+
  theme_bw()+
  ylab("Proportion - Phase 1")+
  xlab("Date")+
  ylim(0,100)+
  scale_x_date(date_breaks = "month", labels = label_date_short()) +
  theme(text = element_text(size = 20)) 
                                          
# phase 2                                          
p.after.ma <- 
ggplot(incoming_data_after, aes(x=Date, y=Prop))+
 # geom_line(group = 1) +
  geom_ma(data = incoming_data_after, ma_fun = SMA, n = 7, size = 1.5) +
  # geom_line(data = incoming_data_after_sensor,
  #           aes(x=Date, y=Prop),
  #          group=1, color="red")+
  geom_ma(data = incoming_data_after_sensor, ma_fun = SMA, n = 7, color = "red", size = 1.5) +
 # geom_point(size=0.8)+
  theme_bw()+
  ylab("Proportion - Phase 2")+
  xlab("Date")+
  ylim(0,100)+
  scale_x_date(date_breaks = "3 months", labels = label_date_short()) +
  theme(text = element_text(size = 20))


# association analysis on income and panas scores - Chi square
# phase 1, positive panas
compare_before_pos <- as.data.frame(table(survcurve_baseline_before$income_level_1, survcurve_baseline_before$pos_panas_highlow))

compare_before_pos <- reshape(compare_before_pos, idvar = "Var1", timevar = "Var2", direction = "wide")

# convert first column to be row name
rownames(compare_before_pos) <- compare_before_pos[,1]
# unselect unwanted column
compare_before_pos <- subset(compare_before_pos, select = -c(Var1))


# run chi square test
test.before_pos <- chisq.test(compare_before_pos)
test.before_pos

tt <- table(survcurve_baseline_before$income_level_1, survcurve_baseline_before$pos_panas_highlow)
chisq.multcomp(tt)

# phase 1, neg panas
compare_before_neg <- as.data.frame(table(survcurve_baseline_before$income_level_1, survcurve_baseline_before$neg_panas_highlow))

compare_before_neg <- reshape(compare_before_neg, idvar = "Var1", timevar = "Var2", direction = "wide")

# convert first column to be row name
rownames(compare_before_neg) <- compare_before_neg[,1]
# unselect unwanted column
compare_before_neg <- subset(compare_before_neg, select = -c(Var1))


# run chi square test
test.before_neg <- chisq.test(compare_before_neg)
test.before_neg

tt <- table(survcurve_baseline_before$income_level_1, survcurve_baseline_before$neg_panas_highlow)
chisq.multcomp(tt)


# phase 2, positive panas
compare_after_pos <- as.data.frame(table(survcurve_baseline_after$income_level_1, survcurve_baseline_after$pos_panas_highlow))

compare_after_pos <- reshape(compare_after_pos, idvar = "Var1", timevar = "Var2", direction = "wide")

# convert first column to be row name
rownames(compare_after_pos) <- compare_after_pos[,1]
# unselect unwanted column
compare_after_pos <- subset(compare_after_pos, select = -c(Var1))


# run chi square test
test.after_pos <- chisq.test(compare_after_pos)
test.after_pos

tt <- table(survcurve_baseline_after$income_level_1, survcurve_baseline_after$pos_panas_highlow)
chisq.multcomp(tt)

# phase 2, neg panas
compare_after_neg <- as.data.frame(table(survcurve_baseline_after$income_level_1, survcurve_baseline_after$neg_panas_highlow))

compare_after_neg <- reshape(compare_after_neg, idvar = "Var1", timevar = "Var2", direction = "wide")

# convert first column to be row name
rownames(compare_after_neg) <- compare_after_neg[,1]
# unselect unwanted column
compare_after_neg <- subset(compare_after_neg, select = -c(Var1))


# run chi square test
test.after_neg <- chisq.test(compare_after_neg)
test.after_neg

tt <- table(survcurve_baseline_after$income_level_1, survcurve_baseline_after$neg_panas_highlow)
chisq.multcomp(tt)



# produce box plot
# neg panas
boxplot_before_neg <- subset(survcurve_baseline_before, ! (is.na(income_level_1)), select = c(income_level_1, neg_panas_score_1))
boxplot_before_neg$income_level_1 <- factor(boxplot_before_neg$income_level_1, levels = c("Less than $25,000",
                                                                                          "$25,000 to $49,999",
                                                                                          "$50,000 to $74,999",
                                                                                          "$75,000 to $99,999",
                                                                                          "More than $100,000"))

boxplot_before_neg$neg_panas_score_1 <- as.numeric(boxplot_before_neg$neg_panas_score_1)
ggplot(boxplot_before_neg, aes(x=income_level_1, y=neg_panas_score_1)) +
  geom_boxplot() +
  theme_bw() +
  theme(text = element_text(size = 20))


# spearman correlation
kruskal.test(neg_panas_score_1 ~ income_level_1, boxplot_before_neg)

boxplot_before_neg %>%
  kruskal_effsize(neg_panas_score_1 ~ income_level_1)


pairwise.wilcox.test(boxplot_before_neg$neg_panas_score_1, boxplot_before_neg$income_level_1, p.adjust.method = "fdr")



# pos panas
boxplot_before_pos <- subset(survcurve_baseline_before, ! (is.na(income_level_1)), select = c(income_level_1, pos_panas_score_1))
boxplot_before_pos$income_level_1 <- factor(boxplot_before_pos$income_level_1, levels = c("Less than $25,000",
                                                                                          "$25,000 to $49,999",
                                                                                          "$50,000 to $74,999",
                                                                                          "$75,000 to $99,999",
                                                                                          "More than $100,000"))

boxplot_before_pos$pos_panas_score_1 <- as.numeric(boxplot_before_pos$pos_panas_score_1)
ggplot(boxplot_before_pos, aes(x=income_level_1, y=pos_panas_score_1)) +
  geom_boxplot() +
  theme_bw() +
  theme(text = element_text(size = 20))


# spearman correlation
kruskal.test(pos_panas_score_1 ~ income_level_1, boxplot_before_pos)

boxplot_before_pos %>%
  kruskal_effsize(pos_panas_score_1 ~ income_level_1)


pairwise.wilcox.test(boxplot_before_pos$pos_panas_score_1, boxplot_before_pos$income_level_1, p.adjust.method = "fdr")


# neg panas
boxplot_after_neg <- subset(survcurve_baseline_after, ! (is.na(income_level_1)), select = c(income_level_1, neg_panas_score_1))
boxplot_after_neg$income_level_1 <- factor(boxplot_after_neg$income_level_1, levels = c("Less than $25,000",
                                                                                          "$25,000 to $49,999",
                                                                                          "$50,000 to $74,999",
                                                                                          "$75,000 to $99,999",
                                                                                          "More than $100,000"))

boxplot_after_neg$neg_panas_score_1 <- as.numeric(boxplot_after_neg$neg_panas_score_1)
ggplot(boxplot_after_neg, aes(x=income_level_1, y=neg_panas_score_1)) +
  geom_boxplot() +
  theme_bw() +
  theme(text = element_text(size = 20))


# spearman correlation
kruskal.test(neg_panas_score_1 ~ income_level_1, boxplot_after_neg)

boxplot_after_neg %>%
  kruskal_effsize(neg_panas_score_1 ~ income_level_1)


pairwise.wilcox.test(boxplot_after_neg$neg_panas_score_1, boxplot_after_neg$income_level_1, p.adjust.method = "fdr")



# pos panas
boxplot_after_pos <- subset(survcurve_baseline_after, ! (is.na(income_level_1)), select = c(income_level_1, pos_panas_score_1))
boxplot_after_pos$income_level_1 <- factor(boxplot_after_pos$income_level_1, levels = c("Less than $25,000",
                                                                                          "$25,000 to $49,999",
                                                                                          "$50,000 to $74,999",
                                                                                          "$75,000 to $99,999",
                                                                                          "More than $100,000"))

boxplot_after_pos$pos_panas_score_1 <- as.numeric(boxplot_after_pos$pos_panas_score_1)
ggplot(boxplot_after_pos, aes(x=income_level_1, y=pos_panas_score_1)) +
  geom_boxplot() +
  theme_bw() +
  theme(text = element_text(size = 20))


# spearman correlation
kruskal.test(pos_panas_score_1 ~ income_level_1, boxplot_after_pos)

boxplot_after_pos %>%
  kruskal_effsize(pos_panas_score_1 ~ income_level_1)


pairwise.wilcox.test(boxplot_after_pos$pos_panas_score_1, boxplot_after_pos$income_level_1, p.adjust.method = "fdr")


################### association analysis between median day in study and active/passive long term engagement cluster ##############
# obtain median day in study for each cluster
# Active - Phase 1 - C1 
c1_before_mday <- subset(all_survey_last_date, `User ID` %in% c1_before$`User ID`)
summary(c1_before_mday$duration_in_Study)

# Active - Phase 1 - C2
c2_before_mday <- subset(all_survey_last_date, `User ID` %in% c2_before$`User ID`)
summary(c2_before_mday$duration_in_Study)

# Active - Phase 1 - C3
c3_before_mday <- subset(all_survey_last_date, `User ID` %in% c3_before$`User ID`)
summary(c3_before_mday$duration_in_Study)

# Active - Phase 1 - C4
c4_before_mday <- subset(all_survey_last_date, `User ID` %in% c4_before$`User ID`)
summary(c4_before_mday$duration_in_Study)

# Active - Phase 2 - C1
c1_after_mday <- subset(all_survey_last_date, `User ID` %in% c1_after$`User ID`)
summary(c1_after_mday$duration_in_Study)

# Active - Phase 2 - C2
c2_after_mday <- subset(all_survey_last_date, `User ID` %in% c2_after$`User ID`)
summary(c2_after_mday$duration_in_Study)

# Active - Phase 2 - C3
c3_after_mday <- subset(all_survey_last_date, `User ID` %in% c3_after$`User ID`)
summary(c3_after_mday$duration_in_Study)

# Active - Phase 2 - C4
c4_after_mday <- subset(all_survey_last_date, `User ID` %in% c4_after$`User ID`)
summary(c4_after_mday$duration_in_Study)


# Passive - Phase 1 - C1 
c1_before_passive_mday <- subset(all_metadata_day_in_study, `User ID` %in% c1_before$`User ID`)
summary(c1_before_passive_mday$Day)

# Passive - Phase 1 - C2 
c2_before_passive_mday <- subset(all_metadata_day_in_study, `User ID` %in% c2_before$`User ID`)
summary(c2_before_passive_mday$Day)

# Passive - Phase 1 - C3 
c3_before_passive_mday <- subset(all_metadata_day_in_study, `User ID` %in% c3_before$`User ID`)
summary(c3_before_passive_mday$Day)

# Passive - Phase 1 - C4 
c4_before_passive_mday <- subset(all_metadata_day_in_study, `User ID` %in% c4_before$`User ID`)
summary(c4_before_passive_mday$Day)

# Passive - Phase 2 - C1 
c1_after_passive_mday <- subset(all_metadata_day_in_study, `User ID` %in% c1_after$`User ID`)
summary(c1_after_passive_mday$Day)

# Passive - Phase 2 - C2 
c2_after_passive_mday <- subset(all_metadata_day_in_study, `User ID` %in% c2_after$`User ID`)
summary(c2_after_passive_mday$Day)

# Passive - Phase 2 - C3 
c3_after_passive_mday <- subset(all_metadata_day_in_study, `User ID` %in% c3_after$`User ID`)
summary(c3_after_passive_mday$Day)

# Passive - Phase 2 - C4 
c4_after_passive_mday <- subset(all_metadata_day_in_study, `User ID` %in% c4_after$`User ID`)
summary(c4_after_passive_mday$Day)



# overall - active - c1
c1_active_mday <- subset(all_survey_last_date, `User ID` %in% c1$`User ID`)
summary(c1_active_mday$duration_in_Study)

# overall - active - C2 
c2_active_mday <- subset(all_survey_last_date, `User ID` %in% c2$`User ID`)
summary(c2_active_mday$duration_in_Study)

# overall - active - C3 
c3_active_mday <- subset(all_survey_last_date, `User ID` %in% c3$`User ID`)
summary(c3_active_mday$duration_in_Study)

# overall - active - C4 
c4_active_mday <- subset(all_survey_last_date, `User ID` %in% c4$`User ID`)
summary(c4_active_mday$duration_in_Study)


# overall - passive - c1
c1_passive_mday <- subset(all_metadata_day_in_study, `User ID` %in% c1$`User ID`)
summary(c1_passive_mday$Day)

# overall - Passive - C2 
c2_passive_mday <- subset(all_metadata_day_in_study, `User ID` %in% c2$`User ID`)
summary(c2_passive_mday$Day)

# overall - Passive - C3 
c3_passive_mday <- subset(all_metadata_day_in_study, `User ID` %in% c3$`User ID`)
summary(c3_passive_mday$Day)

# overall - Passive - C4 
c4_passive_mday <- subset(all_metadata_day_in_study, `User ID` %in% c4$`User ID`)
summary(c4_passive_mday$Day)








# spearman correlation between active vs. passive median day in study - by cluster
cor.test(c1_before_mday$duration_in_Study, c1_before_passive_mday$Day, method = "spearman")

cor.test(c1_after_mday$duration_in_Study, c1_after_passive_mday$Day, method = "spearman")

cor.test(c2_before_mday$duration_in_Study, c2_before_passive_mday$Day, method = "spearman")

cor.test(c2_after_mday$duration_in_Study, c2_after_passive_mday$Day, method = "spearman")

cor.test(c3_before_mday$duration_in_Study, c3_before_passive_mday$Day, method = "spearman")

cor.test(c3_after_mday$duration_in_Study, c3_after_passive_mday$Day, method = "spearman")

cor.test(c4_before_mday$duration_in_Study, c4_before_passive_mday$Day, method = "spearman")

cor.test(c4_after_mday$duration_in_Study, c4_after_passive_mday$Day, method = "spearman")








# not facet by phases
cor.test(c1_active_mday$duration_in_Study, c1_passive_mday$Day, method = "spearman")
cor.test(c2_active_mday$duration_in_Study, c2_passive_mday$Day, method = "spearman")


# c3 active and passive non-equal sample size, passive is missing 27, add those UID as 0 to passive dataset
c3_passive_extra <- subset(c3_active_mday, ! (`User ID` %in% c3_passive_mday$`User ID`))
c3_passive_extra$Day <- 0
c3_passive_extra <- subset(c3_passive_extra, select = c(`User ID`, Day))
c3_passive_mday <- subset(c3_passive_mday, select = c(`User ID`, Day))
c3_passive_mday <- rbind(c3_passive_mday, c3_passive_extra)

cor.test(c3_active_mday$duration_in_Study, c3_passive_mday$Day, method = "spearman")


# c4 active and passive non-equal sample size, passive is missing 158, add those UID as 0 to passive dataset
c4_passive_extra <- subset(c4_active_mday, ! (`User ID` %in% c4_passive_mday$`User ID`))
c4_passive_extra$Day <- 0
c4_passive_extra <- subset(c4_passive_extra, select = c(`User ID`, Day))
c4_passive_mday <- subset(c4_passive_mday, select = c(`User ID`, Day))
c4_passive_mday <- rbind(c4_passive_mday, c4_passive_extra)



cor.test(c4_active_mday$duration_in_Study, c4_passive_mday$Day, method = "spearman")


# total number of days of active data collected across all participants 
# total N of unique days ppl contributed to Active data
all_survey_cluster <- as.data.frame(table(all_survey$`User ID`, all_survey$`date taken`))
all_survey_cluster <- all_survey_cluster[! (all_survey_cluster$Freq == 0),]


all_survey_cluster_totaldays <- as.data.frame(table(all_survey_cluster$Var1))
colnames(all_survey_cluster_totaldays)[1] <- "User ID"

sum(all_survey_cluster_totaldays$Freq)


# total N of unique days ppl contributed to Passive data by clusters
all_metadata_cluster <- as.data.frame(table(all_metadata$`User ID`, all_metadata$Record_creation_ts))
all_metadata_cluster <- all_metadata_cluster[! (all_metadata_cluster$Freq == 0),]

all_metadata_cluster_totaldays <- as.data.frame(table(all_metadata_cluster$Var1))
colnames(all_metadata_cluster_totaldays)[1] <- "User ID"

sum(all_metadata_cluster_totaldays$Freq)
