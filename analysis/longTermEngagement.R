library(factoextra)
library(cluster)
library(NbClust)
library(animation)
library(devtools)
library(ComplexHeatmap)
library(circlize)


############ Survey data ############
# subset Day to 84 --> study end day
all_survey_lte <- subset(all_survey, Day <= 84)

# binary coding - on one day, ppl submitted any survey: 1, did not submit any survey: 0
tbl_all_survey_lte <- as.data.frame(table(all_survey_lte$`User ID`,all_survey_lte$Day))
colnames(tbl_all_survey_lte)[1] <- "User ID"
colnames(tbl_all_survey_lte)[2] <- "Day"
tbl_all_survey_lte$Freq[tbl_all_survey_lte$Freq > 0] <- 1

# reformat into a numeric matrix
tbl_all_survey_lte_temp <- tbl_all_survey_lte
tbl_all_survey_lte_temp <- reshape(tbl_all_survey_lte_temp, idvar = "User ID", timevar = "Day", direction = 'wide')

# facet by phases
tbl_all_survey_lte_temp_before <- subset(tbl_all_survey_lte_temp, `User ID` %in% day_in_study_all_before$`User ID`)
tbl_all_survey_lte_temp_after <- subset(tbl_all_survey_lte_temp, `User ID` %in% day_in_study_all_after$`User ID`)

# Phase 1
rownames(tbl_all_survey_lte_temp_before) <- tbl_all_survey_lte_temp_before[,1]
tbl_all_survey_lte_temp_before <- subset(tbl_all_survey_lte_temp_before, select = -c(`User ID`))
survey_matrix_before <- as.matrix(tbl_all_survey_lte_temp_before)

# Phase 2
rownames(tbl_all_survey_lte_temp_after) <- tbl_all_survey_lte_temp_after[,1]
tbl_all_survey_lte_temp_after <- subset(tbl_all_survey_lte_temp_after, select = -c(`User ID`))
survey_matrix_after <- as.matrix(tbl_all_survey_lte_temp_after)


# determine num of cluster
fviz_nbclust(survey_matrix, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Elbow Method")

fviz_nbclust(tbl_all_survey_lte, kmeans, method = "silhouette") +
  labs(subtitle = "silhouette Method")

fviz_nbclust(tbl_all_survey_lte, kmeans, nstart = 25, method = "gap_stat", nboot = 50) +
  labs(subtitle = "Gap Stats Method")


# phase 1
# set seed for reproducibility
set.seed(12345)

# produce heatmap
kclus_before <- kmeans(survey_matrix_before, 4)
split_before = factor(kclus_before$cluster, levels = c(4, 3, 2, 1))

col_fun_active <- colorRamp2(c(-2, 0, 2), c("blue", "#f5f5f5", "#5e3c99"))

hm_survey_before <- Heatmap(survey_matrix_before, 
                            #k = 4, 
                            col = col_fun_active, 
                            column_title = "Active Data - Phase 1", 
                            split = split_before,
                            cluster_row_slices = FALSE,
                            show_column_dend = FALSE,
                            show_heatmap_legend = F,
                            show_row_dend = F, cluster_columns = F,
                            show_column_names = F, show_row_names = T)

print(hm_survey_before)

# obtain row names
row_order_before <- row_order(hm_survey_before)

id_ordered_before <- row.names(survey_matrix_before)
id_by_clusters_before <- sapply(row_order_before, function(x) id_ordered_before[x])
names(id_by_clusters_before) <- paste0('C', 1:length(id_by_clusters_before))

id_by_clusters_before <- melt(id_by_clusters_before)
colnames(id_by_clusters_before) = c('User ID', 'cluster')



# phase 2
# set seed for reproducibility
set.seed(12345)

# produce heatmap
kclus_after <- kmeans(survey_matrix_after, 4)
split_after = factor(kclus_after$cluster, levels = c(1, 4, 2, 3))

col_fun_active <- colorRamp2(c(-2, 0, 2), c("blue", "#f5f5f5", "#5e3c99"))

hm_survey_after <-  Heatmap(survey_matrix_after, 
                            #k = 4, 
                            col = col_fun_active, 
                            column_title = "Active Data - Phase 2", 
                            split = split_after,
                            cluster_row_slices = FALSE,
                            show_column_dend = FALSE,
                            show_heatmap_legend = F,
                            show_row_dend = F, cluster_columns = F,
                            show_column_names = F, show_row_names = T)

print(hm_survey_after)


# obtain row names
row_order_after <- row_order(hm_survey_after)

id_ordered_after <- row.names(survey_matrix_after)
id_by_clusters_after <- sapply(row_order_after, function(x) id_ordered_after[x])
names(id_by_clusters_after) <- paste0('C', 1:length(id_by_clusters_after))

id_by_clusters_after <- melt(id_by_clusters_after)
colnames(id_by_clusters_after) = c('User ID', 'cluster')

##############3 combine p1 and 2
# Active - combine P1 and 2
############ Survey data ############
# subset Day to 84 --> study end day
all_survey_lte <- subset(all_survey, Day <= 84)

# binary coding - on one day, ppl submitted any survey: 1, did not submit any survey: 0
tbl_all_survey_lte <- as.data.frame(table(all_survey_lte$`User ID`,all_survey_lte$Day))
colnames(tbl_all_survey_lte)[1] <- "User ID"
colnames(tbl_all_survey_lte)[2] <- "Day"
tbl_all_survey_lte$Freq[tbl_all_survey_lte$Freq > 0] <- 1

# reformat into a numeric matrix
tbl_all_survey_lte_temp <- tbl_all_survey_lte
tbl_all_survey_lte_temp <- reshape(tbl_all_survey_lte_temp, idvar = "User ID", timevar = "Day", direction = 'wide')

rownames(tbl_all_survey_lte_temp) <- tbl_all_survey_lte_temp[,1]
tbl_all_survey_lte_temp <- subset(tbl_all_survey_lte_temp, select = -c(`User ID`))
survey_matrix <- as.matrix(tbl_all_survey_lte_temp)

# determine num of cluster
fviz_nbclust(survey_matrix, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Elbow Method")
# set seed for reproducibility
set.seed(12345)

# produce heatmap
kclus <- kmeans(survey_matrix, 4)
split = factor(kclus$cluster, levels = c(1, 2, 4, 3))

col_fun_active <- colorRamp2(c(-2, 0, 2), c("blue", "#f5f5f5", "#5e3c99"))

hm_survey <- Heatmap(survey_matrix, 
                            #k = 4, 
                            col = col_fun_active, 
                            column_title = "Active Data", 
                            split = split,
                            cluster_row_slices = FALSE,
                            show_column_dend = FALSE,
                            show_heatmap_legend = F,
                            show_row_dend = F, cluster_columns = F,
                            show_column_names = F, show_row_names = T)

print(hm_survey)

# obtain row names
row_order <- row_order(hm_survey)

id_ordered <- row.names(survey_matrix)
id_by_clusters <- sapply(row_order, function(x) id_ordered[x])
names(id_by_clusters) <- paste0('C', 1:length(id_by_clusters))

id_by_clusters <- melt(id_by_clusters)
colnames(id_by_clusters) = c('User ID', 'cluster')

#https://davetang.org/muse/2018/05/15/making-a-heatmap-in-r-with-the-pheatmap-package/ 
#pid_col <- cutree(tree = as.dendrogram(my_hclust_gene), k = 2)




# total N of unique days ppl contributed to Active data by clusters
all_survey_cluster <- as.data.frame(table(all_survey$`User ID`, all_survey$`date taken`))
all_survey_cluster <- all_survey_cluster[! (all_survey_cluster$Freq == 0),]

all_survey_cluster_days <- as.data.frame(table(all_survey_cluster$Var1))
colnames(all_survey_cluster_days)[1] <- "User ID"

# incorporate cluster infor
all_survey_cluster_days <- merge(all_survey_cluster_days, id_by_clusters)



# total N of unique days ppl contributed to Passive data by clusters
all_metadata_cluster <- as.data.frame(table(all_metadata$`User ID`, all_metadata$Record_creation_ts))
all_metadata_cluster <- all_metadata_cluster[! (all_metadata_cluster$Freq == 0),]

all_metadata_cluster_days <- as.data.frame(table(all_metadata_cluster$Var1))
colnames(all_metadata_cluster_days)[1] <- "User ID"

# incorporate cluster infor
all_metadata_cluster_days <- merge(all_metadata_cluster_days, id_by_clusters)
all_metadata_cluster_days$Data <- "Passive"

all_survey_cluster_days <- subset(all_survey_cluster_days, `User ID` %in% all_metadata_cluster_days$`User ID`)
all_survey_cluster_days$Data <- "Active"


cluster_all <- rbind(all_survey_cluster_days, all_metadata_cluster_days)

# box plot
ggplot(cluster_all, aes(x=cluster, y=Freq, fill=Data)) + 
  geom_boxplot()+
  theme_bw() +
  theme(text = element_text(size = 20))+
  xlab("Cluster")+
  ylab("Total Days Contributed") +
  theme(
        legend.title=element_blank())+
  scale_fill_manual(values = c("#5e3c99", "#e66101"))


# subset by cluster
cluster_all_c1 <- subset(cluster_all, cluster == "C1")
cluster_all_c2 <- subset(cluster_all, cluster == "C2")
cluster_all_c3 <- subset(cluster_all, cluster == "C3")
cluster_all_c4 <- subset(cluster_all, cluster == "C4")


# Kruskal test - C1
kruskal.test(Freq ~ Data, data = cluster_all_c1)
kruskal.test(Freq ~ Data, data = cluster_all_c2)
kruskal.test(Freq ~ Data, data = cluster_all_c3)
kruskal.test(Freq ~ Data, data = cluster_all_c4)



# spearman r
# c1
cluster_all_c1_active <- subset(cluster_all_c1, Data == "Active")
cluster_all_c1_passive <- subset(cluster_all_c1, Data == "Passive")

cor.test(x=cluster_all_c1_active$Freq, y=cluster_all_c1_passive$Freq, method = 'spearman')

# c2
cluster_all_c2_active <- subset(cluster_all_c2, Data == "Active")
cluster_all_c2_passive <- subset(cluster_all_c2, Data == "Passive")

cor.test(x=cluster_all_c2_active$Freq, y=cluster_all_c2_passive$Freq, method = 'spearman')

# c3
cluster_all_c3_active <- subset(cluster_all_c3, Data == "Active")
cluster_all_c3_passive <- subset(cluster_all_c3, Data == "Passive")

cor.test(x=cluster_all_c3_active$Freq, y=cluster_all_c3_passive$Freq, method = 'spearman')

# c4
cluster_all_c4_active <- subset(cluster_all_c4, Data == "Active")
cluster_all_c4_passive <- subset(cluster_all_c4, Data == "Passive")

cor.test(x=cluster_all_c1_active$Freq, y=cluster_all_c1_passive$Freq, method = 'spearman')



############ Sensor data - difference row order than Active pattern ############
# calculate Day-in-Study
all_metadata_day <- merge(all_metadata,all_metadata_first_date, all = T)
all_metadata_day$Y_M_D <- format(as.Date(all_metadata_day$Record_creation_ts), "%Y-%m-%d")
all_metadata_day$Day <- as.numeric(as.Date(all_metadata_day$Y_M_D) - as.Date(all_metadata_day$`Sensor first date`) + 1)

# # subset Day to 84 --> study end day
all_metadata_day_lte <- subset(all_metadata_day, Day <= 84)
# 
# # binary coding - on one day, ppl submitted any survey: 1, did not submit any survey: 0
tbl_all_metadata_day_lte <- as.data.frame(table(all_metadata_day_lte$`User ID`, all_metadata_day_lte$Day))
colnames(tbl_all_metadata_day_lte)[1] <- "User ID"
colnames(tbl_all_metadata_day_lte)[2] <- "Day"
tbl_all_metadata_day_lte$Freq[tbl_all_metadata_day_lte$Freq > 0] <- 1

tbl_all_metadata_day_lte_temp <- tbl_all_metadata_day_lte
tbl_all_metadata_day_lte_temp <- reshape(tbl_all_metadata_day_lte_temp, idvar = "User ID", timevar = "Day", direction = 'wide')


# # facet by phases
# tbl_all_metadata_day_lte_temp_before <- subset(tbl_all_metadata_day_lte_temp, `User ID` %in% day_in_study_all_before$`User ID`)
# tbl_all_metadata_day_lte_temp_after <- subset(tbl_all_metadata_day_lte_temp, `User ID` %in% day_in_study_all_after$`User ID`)
# 
# # Phase 1
# rownames(tbl_all_metadata_day_lte_temp_before) <- tbl_all_metadata_day_lte_temp_before[,1]
# tbl_all_metadata_day_lte_temp_before <- subset(tbl_all_metadata_day_lte_temp_before, select = -c(`User ID`))
# metadata_matrix_before <- as.matrix(tbl_all_metadata_day_lte_temp_before)
# 
# # Phase 2
# rownames(tbl_all_metadata_day_lte_temp_after) <- tbl_all_metadata_day_lte_temp_after[,1]
# tbl_all_metadata_day_lte_temp_after <- subset(tbl_all_metadata_day_lte_temp_after, select = -c(`User ID`))
# metadata_matrix_after <- as.matrix(tbl_all_metadata_day_lte_temp_after)
# 
# 
# # determine num of cluster
# fviz_nbclust(metadata_matrix, kmeans, method = "wss") +
#   geom_vline(xintercept = 4, linetype = 2) +
#   labs(subtitle = "Elbow Method")
# 
# fviz_nbclust(tbl_all_survey_lte, kmeans, method = "silhouette") +
#   labs(subtitle = "silhouette Method")
# 
# fviz_nbclust(tbl_all_survey_lte, kmeans, nstart = 25, method = "gap_stat", nboot = 50) +
#   labs(subtitle = "Gap Stats Method")
# 
# 
# # phase 1
# # set seed for reproducibility
# set.seed(12345)
# 
# # produce heatmap
# col_fun <- colorRamp2(c(-2, 0, 2), c("blue", "white", "purple"))
# hm_metadata_before <- Heatmap(metadata_matrix_before, 
#                               k = 4, 
#                               col = col_fun, 
#                               column_title = "Passive Data - Phase 1", 
#                               show_column_dend = FALSE,
#                               show_heatmap_legend = F,
#                               show_row_dend = F, cluster_columns = F,
#                               show_column_names = F, show_row_names = T)
# 
# print(hm_metadata_before)
# 
# 
# 
# 
# # phase 2
# # set seed for reproducibility
# set.seed(12345)
# 
# # produce heatmap
# col_fun <- colorRamp2(c(-2, 0, 2), c("blue", "white", "purple"))
# hm_metadata_after <- Heatmap(metadata_matrix_after, 
#                               k = 4, 
#                               col = col_fun, 
#                               column_title = "Passive Data - Phase 2", 
#                               show_column_dend = FALSE,
#                               show_heatmap_legend = F,
#                               show_row_dend = F, cluster_columns = F,
#                               show_column_names = F, show_row_names = T)
# 
# print(hm_metadata_after)
# 
# 
# 
# 
# 


############ Sensor data - same row order than Active pattern ############
# Phase 1
tbl_all_metadata_day_lte_temp_before_row.reorder <- subset(tbl_all_metadata_day_lte_temp_before, 
                                                           rownames(tbl_all_metadata_day_lte_temp_before) %in% rownames(tbl_all_survey_lte_temp_before))
metadata_matrix_beforerow.reorder <- as.matrix(tbl_all_metadata_day_lte_temp_before_row.reorder)


# Phase 2
tbl_all_metadata_day_lte_temp_after_row.reorder <- subset(tbl_all_metadata_day_lte_temp_after, 
                                                           rownames(tbl_all_metadata_day_lte_temp_after) %in% rownames(tbl_all_survey_lte_temp_after))

temp <- subset(tbl_all_survey_lte_temp_after, 
               ! (rownames(tbl_all_survey_lte_temp_after) %in% rownames(tbl_all_metadata_day_lte_temp_after_row.reorder)))
temp[temp > 0] <- 0

tbl_all_metadata_day_lte_temp_after_row.reorder <- rbind(tbl_all_metadata_day_lte_temp_after_row.reorder, temp)
tbl_all_metadata_day_lte_temp_after_row.reorder <- tbl_all_metadata_day_lte_temp_after_row.reorder[ order(row.names(tbl_all_metadata_day_lte_temp_after_row.reorder)), ]
metadata_matrix_afterrow.reorder <- as.matrix(tbl_all_metadata_day_lte_temp_after_row.reorder)


# phase 1
# set seed for reproducibility
set.seed(12345)

# produce heatmap
col_fun_passive <- colorRamp2(c(-2, 0, 2), c("blue", "#f5f5f5", "#e66101"))
hm_metadata_before_row.reorder <- Heatmap(metadata_matrix_beforerow.reorder, 
                              #k = 4, 
                              col = col_fun_passive, 
                              column_title = "Passive Data - Phase 1", 
                              #row_order = id_by_clusters_before$`User ID`,
                              split = split_before,
                              cluster_row_slices = FALSE,
                              show_column_dend = FALSE,
                              show_heatmap_legend = F,
                              show_row_dend = F, cluster_columns = F,
                              show_column_names = F, show_row_names = T)

print(hm_metadata_before_row.reorder)




# phase 2
# set seed for reproducibility
set.seed(12345)

# produce heatmap
col_fun_passive <- colorRamp2(c(-2, 0, 2), c("blue", "#f5f5f5", "#e66101"))
hm_metadata_after_row.reorder <- Heatmap(metadata_matrix_afterrow.reorder, 
                                         # k = 4, 
                                          col = col_fun_passive, 
                                          column_title = "Passive Data - Phase 2", 
                                         # row_order = id_by_clusters_after,
                                         split = split_after,
                                         cluster_row_slices = FALSE,
                                          show_column_dend = FALSE,
                                          show_heatmap_legend = F,
                                          show_row_dend = F, cluster_columns = F,
                                          show_column_names = F, show_row_names = T)

print(hm_metadata_after_row.reorder)





# combine P1 and P2
# calculate Day-in-Study
all_metadata_day <- merge(all_metadata,all_metadata_first_date, all = T)
all_metadata_day$Y_M_D <- format(as.Date(all_metadata_day$Record_creation_ts), "%Y-%m-%d")
all_metadata_day$Day <- as.numeric(as.Date(all_metadata_day$Y_M_D) - as.Date(all_metadata_day$`Sensor first date`) + 1)

# # subset Day to 84 --> study end day
all_metadata_day_lte <- subset(all_metadata_day, Day <= 84)
# 
# # binary coding - on one day, ppl submitted any survey: 1, did not submit any survey: 0
tbl_all_metadata_day_lte <- as.data.frame(table(all_metadata_day_lte$`User ID`, all_metadata_day_lte$Day))
colnames(tbl_all_metadata_day_lte)[1] <- "User ID"
colnames(tbl_all_metadata_day_lte)[2] <- "Day"
tbl_all_metadata_day_lte$Freq[tbl_all_metadata_day_lte$Freq > 0] <- 1

tbl_all_metadata_day_lte_temp <- tbl_all_metadata_day_lte
tbl_all_metadata_day_lte_temp <- reshape(tbl_all_metadata_day_lte_temp, idvar = "User ID", timevar = "Day", direction = 'wide')

rownames(tbl_all_metadata_day_lte_temp) <- tbl_all_metadata_day_lte_temp[,1]
tbl_all_metadata_day_lte_temp <- subset(tbl_all_metadata_day_lte_temp, select = -c(`User ID`))
metadata_matrix <- as.matrix(tbl_all_metadata_day_lte_temp)




tbl_all_metadata_day_lte_temp_row.reorder <- subset(tbl_all_metadata_day_lte_temp, 
                                                           rownames(tbl_all_metadata_day_lte_temp) %in% rownames(tbl_all_survey_lte_temp))

temp <- subset(tbl_all_survey_lte_temp, 
               ! (rownames(tbl_all_survey_lte_temp) %in% rownames(tbl_all_metadata_day_lte_temp_row.reorder)))
temp[temp > 0] <- 0

tbl_all_metadata_day_lte_temp_row.reorder <- rbind(tbl_all_metadata_day_lte_temp_row.reorder, temp)
tbl_all_metadata_day_lte_temp_row.reorder <- tbl_all_metadata_day_lte_temp_row.reorder[ order(row.names(tbl_all_metadata_day_lte_temp_row.reorder)), ]
metadata_matrix_row.reorder <- as.matrix(tbl_all_metadata_day_lte_temp_row.reorder)

# produce heatmap
col_fun_passive <- colorRamp2(c(-2, 0, 2), c("blue", "#f5f5f5", "#e66101"))

hm_sensor <- Heatmap(metadata_matrix_row.reorder, 
                     #k = 4, 
                     col = col_fun_passive, 
                     column_title = "Passive Data", 
                     split = split,
                     cluster_row_slices = FALSE,
                     show_column_dend = FALSE,
                     show_heatmap_legend = F,
                     show_row_dend = F, cluster_columns = F,
                     show_column_names = F, show_row_names = T)

print(hm_sensor)


