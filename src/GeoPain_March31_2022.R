setwd("C:/Users/11191/Desktop/2022 Winter/MDP/Migraine_Predictive_Analytics/Migraine_Predictive_Analytics_final_version")
#setwd("C:/Users/simeonem/Documents/Papers/GeoPain/")
#load("GeoPain_Metadata_CBDA_Feb2021.RData")
## AFTRER LOADING THE RDATA WORKSPACE BELOW, SKIP TO LINE 92
#load("GeoPain_Feb9_2022.RData") 
load("./Manuscript/YangChen/GeoPain_3_23.RData")
save(list = ls(all.names = TRUE), file = "./Manuscript/YangChen/GeoPain_3_23_SM.RData")
dim(GeoPain_Metadata_CBDA) # 2050 x 112
filename <- c("C:/Users/simeonem/Documents/GeoPain/GeoPainDataSignsSymptomsHead.json")
GeoPain_Data_R <- ndjson::stream_in(filename)
dim(GeoPain_Data_R) # 2050 x 129
patient_id <- unique(GeoPain_Data_R$guid)
patient_outcome <- NULL
Patient_history <- NULL
for (i in 1:length(patient_id))
{
  All_events <- NULL
  for (j in 1:length(which(GeoPain_Data_R$guid==patient_id[i])))
  {
    All_events <- 
      c(All_events,GeoPain_Data_R$diagnosis.0[j] ,
      GeoPain_Data_R$diagnosis.1[j] ,
      GeoPain_Data_R$diagnosis.2[j] ,
      GeoPain_Data_R$diagnosis.3[j] ,
      GeoPain_Data_R$diagnosis.4[j] ,
      GeoPain_Data_R$diagnosis.5[j] ,
      GeoPain_Data_R$diagnosis.6[j] ,
      GeoPain_Data_R$diagnosis.7[j] ,
      GeoPain_Data_R$diagnosis.8[j] ,
      GeoPain_Data_R$diagnosis.9[j] ,
      GeoPain_Data_R$diagnosis.10[j],
      GeoPain_Data_R$diagnosis.11[j],
      GeoPain_Data_R$diagnosis.12[j])
  }
  Migraine_events <- TMD_events <- NULL
  Migraine_events <- which(All_events == "migraine/headache")
  TMD_events<- which(All_events == "temporomandibular joint dysfunction")
  if (length(Migraine_events) > 0 & length(TMD_events) == 0){patient_outcome[i]=0}
  if (length(TMD_events) > 0 & length(Migraine_events) == 0){patient_outcome[i]=1}
  if (length(Migraine_events) > 0 & length(TMD_events) > 0){patient_outcome[i]=3}
  Patient_history <- 
    rbind(Patient_history,c(i,length(Migraine_events),length(TMD_events)))
}
patient_outcome
Patient_history <- as.data.frame(Patient_history)
names(Patient_history) <- c("Patient","MigraineCount","TMDCount")

## remove ONLY the comorbidity records
ALL_patients_occurences <- NULL
for (i in patient_id)
{
  ALL_patients_occurences <- 
    c(ALL_patients_occurences,length(which(GeoPain_Data_R$guid == i)))
}
Patient_frequency <- cbind(1:length(patient_id),ALL_patients_occurences)
patients_occurences <- NULL
records_comorbidity <- NULL
for (i in which(patient_outcome==3))
{
  records_comorbidity <- 
    c(records_comorbidity,which(GeoPain_Data_R$guid == patient_id[i]))
  patients_occurences <- c(patients_occurences,length(which(GeoPain_Data_R$guid == patient_id[i])))
}
Comorbidity_matrix <- cbind(which(patient_outcome==3),patients_occurences)

length(records_comorbidity) # 1271
GeoPain_Data_R_nocomorb <- GeoPain_Data_R[-records_comorbidity,]
dim(GeoPain_Data_R_nocomorb) # 779
dim(GeoPain_Metadata_CBDA) # 2050 x 112
GeoPain_Metadata_CBDA_noCoMorb <- GeoPain_Metadata_CBDA[-records_comorbidity,]
dim(GeoPain_Metadata_CBDA_noCoMorb) # 779 x 112
table(GeoPain_Metadata_CBDA_noCoMorb$Outcome)
a3 <- which(GeoPain_Metadata_CBDA_noCoMorb$Outcome==2)


All_IDs <- cbind(c(1:dim(GeoPain_Data_R)[1]),GeoPain_Data_R$guid)
dim(All_IDs) # 2050 x 2
NO_comorbidity_IDs_temp <- All_IDs[-records_comorbidity,]
dim(NO_comorbidity_IDs_temp) # 779
length(a3) # 28
NO_comorbidity_IDs_map <- NO_comorbidity_IDs_temp[-a3,]
dim(NO_comorbidity_IDs) # 751 x 2

GeoPain_Metadata_CBDA_noCoMorb_final <- GeoPain_Metadata_CBDA_noCoMorb[-a3,]
dim(GeoPain_Metadata_CBDA_noCoMorb_final) # 751 x 112
table(GeoPain_Metadata_CBDA_noCoMorb_final$Outcome) # 668 vs 63
GeoPain_Metadata_CBDA_noCoMorb_Validation <- 
  GeoPain_Metadata_CBDA[records_comorbidity,]
dim(GeoPain_Metadata_CBDA_noCoMorb_Validation) # 1271  x 112
table(GeoPain_Metadata_CBDA_noCoMorb_Validation$Outcome)
a4 <- NULL
a4 <- which(GeoPain_Metadata_CBDA_noCoMorb_Validation$Outcome==2)
GeoPain_Metadata_CBDA_noCoMorb_Validation_final <-
  GeoPain_Metadata_CBDA_noCoMorb_Validation[-a4,]
table(GeoPain_Metadata_CBDA_noCoMorb_Validation_final$Outcome)

Comorbidity_IDs_map_temp <- All_IDs[records_comorbidity,]
length(records_comorbidity) # 1271
dim(Comorbidity_IDs_map_temp) # 1271 x 2
Comorbidity_IDs_map <- Comorbidity_IDs_map_temp[-a4,]
dim(Comorbidity_IDs_map) # 1240 x 2

GeoPain_Metadata_CBDA_noCoMorb_final$age <- NULL
GeoPain_Metadata_CBDA_noCoMorb_Validation_final$age <- NULL
headneck_labels <- c(3:20)
GeoPain_Metadata_CBDA_noCoMorb_final_noHN <- 
  GeoPain_Metadata_CBDA_noCoMorb_final[,-headneck_labels]
GeoPain_Metadata_CBDA_noCoMorb_Validation_final_noHN <- 
  GeoPain_Metadata_CBDA_noCoMorb_Validation_final[,-headneck_labels]
dim(GeoPain_Metadata_CBDA_noCoMorb_final_noHN) # 751  93
dim(GeoPain_Metadata_CBDA_noCoMorb_Validation_final_noHN) # 1240   93

## Dataset for Unsupervised classification
GeoPain_Metadata_Unsupervised_noCoMorb_final_noHN <- 
  rbind(GeoPain_Metadata_CBDA_noCoMorb_final_noHN,
        GeoPain_Metadata_CBDA_noCoMorb_Validation_final_noHN)
dim(GeoPain_Metadata_Unsupervised_noCoMorb_final_noHN)
table(GeoPain_Metadata_Unsupervised_noCoMorb_final_noHN$Outcome)
# Eliminate the $Outcome column before analysis
# Run Unsupervised before and after rebalancing the dataset
## Unbalanced dataset: X_unbalanced OR GeoPain_Metadata_Unsupervised_noCoMorb_final_noHN
## Balanced dataset: X_balanced
## Balancing the Geopain Data without comorbidities
## TRAINING SET
dim(GeoPain_Metadata_Unsupervised_noCoMorb_final_noHN) # 1991  93
table(GeoPain_Metadata_Unsupervised_noCoMorb_final_noHN$Outcome)
data_temp <- GeoPain_Metadata_Unsupervised_noCoMorb_final_noHN
X <- data_temp[,-c(1,2)]
dim(X)
X_unbalanced <- as.data.frame(X)
Y_unbalanced <-  data_temp$Outcome
table(Y_unbalanced)
Data_unbalanced <- cbind(X_unbalanced,Y_unbalanced)
Data_balanced <- smotefamily::SMOTE(Data_unbalanced[,-dim(Data_unbalanced)[2]],
                                    Data_unbalanced[,dim(Data_unbalanced)[2]])
X_balanced <- Data_balanced$data[,-dim(Data_unbalanced)[2]]
Y_balanced <- as.numeric(Data_balanced$data[,dim(Data_unbalanced)[2]])
table(Y_balanced)
dim(X_balanced)
Data_balanced <- cbind(X_balanced,Y_balanced)



## Retrieving the CBDA top predictive features
#setwd("C:/Users/simeonem/Documents/Papers/GeoPain/")
setwd("C:/Users/11191/Desktop/2022 Winter/MDP/Migraine_PredictiveAnalytics/Code_Data_3methods")
#load("./CBDA/CBDA_M5000_miss0_GeoPain_noCoMorb_noAge_noHN_balanced_VALIDATION.RData")
load("./Mar_9_2022.RData")


CBDA_object_Validation_GeoPain_noCoMorb_noAge_noHN_balanced <- 
  CBDA_object_Validation
a4 <- which.max(CBDA_object_Validation$ValidationTable[,2])
CBDA_object_Validation$ValidationTable[a4,2]
names(GeoPain_Metadata_CBDA_noCoMorb_final_noHN)[CBDA_object_Validation_GeoPain_noCoMorb_noAge_noHN_balanced$TopFeaturesAccuracy[1:a4]+2]
CBDA_object_Validation$ConfusionMatrices[a4+2]
feature_set <- NULL
feature_set_temp <- NULL
feature_set_temp <- CBDA_object_Validation$TopFeaturesAccuracy[1:(a4+2)]
feature_set <- names(GeoPain_Metadata_CBDA_noCoMorb_final_noHN)[feature_set_temp+2]
CBDA_GeoPain_Top <- data.frame(GeoPain_Feature=feature_set,
                               Likelihood=c(0,0,CBDA_object_Validation$ValidationTable[1:a4,2]))
#write.csv(CBDA_GeoPain_Top,file = "./Manuscript/Tables/CBDA_noCoMorb_noAge_noHN_balanced_Aug5_2021.csv")


Top50_CBDA_index <- CBDA_object_Validation$TopFeaturesAccuracy
# Retrieve the labels from the list of all the labels
feature_set_all[Top50_CBDA_index+2]

GeoPain_unbalanced_top50 <- 
  X_unbalanced[,Top50_CBDA_index]
dim(GeoPain_unbalanced_top50)

GeoPain_balanced_top50 <- 
  X_balanced[,Top50_CBDA_index]
dim(GeoPain_balanced_top50)

## PCA on the Top50 CBDA features
##Unbalanced data
pca_unbalanced_top50CBDA <- prcomp(GeoPain_unbalanced_top50, center = T, scale. = TRUE)

library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
grid.arrange(p1, p2, ncol = 2)
grid.arrange(t, p, p2, r, ncol=2)

par(mfrow = c(1, 2))
p1 <- autoplot(pca_unbalanced_top50CBDA, data = Data_unbalanced, colour = 'Y_unbalanced')
p2 <- fviz_eig(pca_unbalanced_top50CBDA)
p4 = grid.arrange(p1, p1, ncol = 2)
ggsave("PCA_Top50_CBDA_features.png", plot = p4, width = 18, height = 10)

# autoplot(pca_unbalanced_top50CBDA)
Data_unbalanced$Y_unbalanced <- as.factor(Data_unbalanced$Y_unbalanced)
autoplot(pca_unbalanced_top50CBDA, data = Data_unbalanced, colour = 'Y_unbalanced')
p3 <- pca3d(pca_unbalanced_top50CBDA, group = Data_unbalanced$Y_unbalanced, title = "3D PCA
plots of unbalanced data PAIN ONLY", show.ellipses=FALSE)
summary(pca_unbalanced_top50CBDA)
fviz_eig(pca_unbalanced_top50CBDA)
sort(pca_unbalanced_top50CBDA$scale, decreasing = T)[1:10]
snapshotPCA3d("pca_unbalanced_top50CBDA.png")
#hist(pca_unbalanced$scale)


library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
p <- qplot(1,1)
p2 <- xyplot(1~1)
r <- rectGrob(gp=gpar(fill="grey90"))
t <- textGrob("text")
grid.arrange(t, p, p2, r, ncol=2)



##Balanced data
pca_balanced_top50CBDA <- prcomp(GeoPain_balanced_top50, center = T, scale. = TRUE)
autoplot(pca_balanced_top50CBDA)
names(Data_balanced$data)[92] <- 'Y_balanced'
Data_balanced$data[,92] <- as.factor(Data_balanced$data[,92])
autoplot(pca_balanced_top50CBDA, data = Data_balanced$data, colour = 'Y_balanced')
pca3d(pca_balanced_top50CBDA, group = Data_balanced$data$Y_balanced, title = "3D PCA plots of
balanced data (TOP 50 CBDA)")
summary(pca_balanced_top50CBDA)
fviz_eig(pca_balanced_top50CBDA)
sort(pca_balanced_top50CBDA$scale, decreasing = T)[1:10]
snapshotPCA3d("pca_balanced_top50CBDA.png")
#hist(pca_unbalanced$scale)


list_function <- grep(pattern="function",feature_set_all)
list_triggers <- grep(pattern="triggers",feature_set_all)
list_symptoms <- grep(pattern="symptoms",feature_set_all)
list_pain <- grep(pattern="pain",feature_set_all)
length(feature_set_all)
length(c(list_function,list_triggers,list_symptoms,list_pain))

# This is the subset of the GeoPain data with ONLY pain-related features
# Must subract 2 from list_*** due to indexing in perl/shell
X_unbalanced_pain <- X_unbalanced[,list_pain-2]
X_balanced_pain <- X_balanced[,list_pain-2]

# This is the subset of the GeoPain data WITHOUT pain-related features
list_other <- sort(c(list_function,list_triggers,list_symptoms))
X_unbalanced_other <- X_unbalanced[,list_other-2]
X_balanced_other <- X_balanced[,list_other-2]

#save(list = ls(all.names = TRUE), file = "Feb28.RData")

# ## PCA on the pain-related features only
# ##Unbalanced data
# pca_unbalanced_pain <- prcomp(X_unbalanced_pain, center = T, scale. = TRUE)
# autoplot(pca_unbalanced_pain)
# autoplot(pca_unbalanced_pain, data = Data_unbalanced, colour = 'Y_unbalanced')
# pca3d(pca_unbalanced_pain, group = Data_unbalanced$Y_unbalanced, title = "3D PCA
# plots of unbalanced data (PAIN ONLY")
# summary(pca_unbalanced_pain)
# fviz_eig(pca_unbalanced_pain)
# snapshotPCA3d("pca_unbalanced_pain.png")
# 
# ##Balanced data
# pca_balanced_pain <- prcomp(X_balanced_pain, center = T, scale. = TRUE)
# autoplot(pca_balanced_pain)
# names(Data_balanced$data)[92] <- 'Y_balanced'
# Data_balanced$data[,92] <- as.factor(Data_balanced$data[,92])
# autoplot(pca_balanced_pain, data = Data_balanced$data, colour = 'Y_balanced')
# pca3d(pca_balanced_pain, group = Data_balanced$data$Y_balanced, title = "3D PCA plots of
# balanced data (PAIN ONLY)")
# summary(pca_balanced_pain)
# fviz_eig(pca_balanced_pain)
# snapshotPCA3d("pca_balanced_pain.png")

# ## PCA on the NON-pain-related features only (function/triggers/symptoms)
# ##Unbalanced data
# pca_unbalanced_other <- prcomp(X_unbalanced_other, center = T, scale. = TRUE)
# autoplot(pca_unbalanced_other)
# autoplot(pca_unbalanced_other, data = Data_unbalanced, colour = 'Y_unbalanced')
# pca3d(pca_unbalanced_other, group = Data_unbalanced$Y_unbalanced, title = "3D PCA
# plots of unbalanced data (WITHOUT PAIN FEATURES")
# summary(pca_unbalanced_other)
# fviz_eig(pca_unbalanced_other)
# snapshotPCA3d("pca_unbalanced_other.png")

# ##Balanced data
# pca_balanced_other <- prcomp(X_balanced_other, center = T, scale. = TRUE)
# autoplot(pca_balanced_other)
# names(Data_balanced$data)[92] <- 'Y_balanced'
# Data_balanced$data[,92] <- as.factor(Data_balanced$data[,92])
# autoplot(pca_balanced_other, data = Data_balanced$data, colour = 'Y_balanced')
# jpeg("pca2D_balanced_otherNEW.jpeg")
# pca3d(pca_balanced_other, group = Data_balanced$data$Y_balanced, title = "3D PCA plots of
# balanced data (WITHOUT PAIN FEATURES)")
# summary(pca_balanced_other)
# fviz_eig(pca_balanced_other)
# snapshotPCA3d("pca_balanced_other.png")
#save(list = ls(all.names = TRUE), file = "Feb28.RData")






# Yang Chen
#save(list = ls(all.names = TRUE), file = "GeoPain_Feb9_2022.RData")
## Suggestions for unsupervised classification
# 1) PCA
# 2) tSNE
# 3) UMAP
# ....
library(patchwork)
library(ggfortify)
library(pca3d)
library(factoextra)
library(ggplot2)
library(gridExtra)   
library(grid)
library(lattice)
library(Rtsne)
library(umap)
#devtools::install_github("AckerDWM/gg3D")
library("gg3D")

## Generate png/jpg for each plot/graph for each dataset for each method
# List of all the X datasets
# X_unbalanced , X_unbalanced_pain, X_unbalanced_other,  GeoPain_unbalanced_top50
# X_balanced , X_balanced_pain, X_balanced_other,  GeoPain_balanced_top50
# Do something similar to "snapshotPCA3d("pca_balanced_other.png")" for plots and
# graphs, 2D and 3D generated by PCA, UMAP and tSNE, with the appropriate label.


# Separate according to the data type
# load("cyy.RData")
# Group1: X_unbalanced and X_balanced
# (a) PCA
# unbalanced
Data_unbalanced$Y_unbalanced <- as.factor(Data_unbalanced$Y_unbalanced)
pca_unbalanced <- prcomp(Data_unbalanced[,0:91], center = T, scale. = TRUE)
pca3d(pca_unbalanced, group = Data_unbalanced$Y_unbalanced, title = "3D PCA plots of unbalanced data")
snapshotPCA3d("pca_X_unbalanced_3D.png")
p1a_1 <- autoplot(pca_unbalanced, data = Data_unbalanced, colour = 'Y_unbalanced', title = "pca_unbalanced_2D")
p1a_2 <- fviz_eig(pca_unbalanced)
# summary(pca_unbalanced)
# fviz_contrib(pca_unbalanced, choice = "var", axes = 1, top = 20)
# unbalanced_var <- get_pca_var(pca_unbalanced)
# sort(unbalanced_var$contrib[,1], decreasing = T)

# balanced
Data_balanced$Y_balanced <- as.factor(Data_balanced$Y_balanced)
pca_balanced <- prcomp(Data_balanced[,0:91], center = T, scale. = TRUE)
pca3d(pca_balanced, group = Data_balanced$Y_balanced, title = "3D PCA plots of balanced data")
snapshotPCA3d("pca_X_balanced_3D.png")
p1a_3 <- autoplot(pca_balanced, data = Data_balanced, colour = 'Y_balanced')
p1a_4 <- fviz_eig(pca_balanced)
# summary(pca_balanced)
# fviz_contrib(pca_balanced, choice = "var", axes = 1, top = 20)
# balanced_var <- get_pca_var(pca_balanced)
# sort(balanced_var$contrib[,1], decreasing = T)
# sink(file = "balanced.txt")
# var$contrib
# sink(file = NULL)

p1a = grid.arrange(p1a_1, p1a_2, p1a_3, p1a_4, ncol = 2)
ggsave("X_PCA.png", plot = p1a, width = 16, height = 8)


# (b) tSNE
# unbalanced
tsne_unbalanced_data = as.matrix(Data_unbalanced[0:91])
set.seed(100) # change random seed to get different results
tsne_unbalanced <- Rtsne(tsne_unbalanced_data, dims = 2, theta = 0.5, check_duplicates = FALSE)
tsne_plot_unbalanced <- data.frame(x = tsne_unbalanced$Y[,1], y = tsne_unbalanced$Y[,2], col = Data_unbalanced$Y_unbalanced)
p1b_1 <- ggplot(tsne_plot_unbalanced) + geom_point(aes(x=x, y=y, color=col))
X_tSNE_record1 <- which(tsne_unbalanced$Y[,1]>6 & 
                          tsne_unbalanced$Y[,2]>=30) # seed 100
X_tSNE_record2 <- which(tsne_unbalanced$Y[,1]>6 & 
                          tsne_unbalanced$Y[,2]>=-10 &
                          tsne_unbalanced$Y[,2]<30)
X_tSNE_record3 <- which(2.2*tsne_unbalanced$Y[,1] + tsne_unbalanced$Y[,2] < 25) 
X_tSNE_subject1 = X_unbalanced[X_tSNE_record1,]
X_tSNE_subject2 = X_unbalanced[X_tSNE_record2,]
X_tSNE_subject3 = X_unbalanced[X_tSNE_record3,]
# 3D
tsne_unbalanced_3D <- Rtsne(tsne_unbalanced_data, dims = 3, theta = 0.5, check_duplicates = FALSE) 
tsne_plot_unbalanced_3D <- data.frame(x = tsne_unbalanced_3D$Y[,1], y = tsne_unbalanced_3D$Y[,2], z = tsne_unbalanced_3D$Y[,3], col = Data_unbalanced$Y_unbalanced)
p1b_2 <- ggplot(tsne_plot_unbalanced_3D, aes(x=x, y=y, z=z, color=col)) + theme_void() + axes_3D() + stat_3D()

# balanced
tsne_balanced_data = as.matrix(Data_balanced[0:91])
set.seed(100)
tsne_balanced <- Rtsne(tsne_balanced_data, dims = 2, theta = 0.5, check_duplicates = FALSE) 
tsne_plot_balanced <- data.frame(x = tsne_balanced$Y[,1], y = tsne_balanced$Y[,2], col = Data_balanced$Y_balanced)
p1b_3 <- ggplot(tsne_plot_balanced) + geom_point(aes(x=x, y=y, color=col))
# 3D
tsne_balanced_3D <- Rtsne(tsne_balanced_data, dims = 3, theta = 0.5, check_duplicates = FALSE) 
tsne_plot_balanced_3D <- data.frame(x = tsne_balanced_3D$Y[,1], y = tsne_balanced_3D$Y[,2], z = tsne_balanced_3D$Y[,3], col = Data_balanced$Y_balanced)
p1b_4 <- ggplot(tsne_plot_balanced_3D, aes(x=x, y=y, z=z, color=col)) + theme_void() + axes_3D() + stat_3D()

p1b = grid.arrange(p1b_1, p1b_2, p1b_3, p1b_4, ncol = 2)
ggsave("X_tSNE.png", plot = p1b, width = 16, height = 12)


# (c) UMAP
# unbalanced
umap_unbalanced <- umap(Data_unbalanced[0:91], n_components = 2, random_state = 15)
umap_plot_unbalanced <- data.frame(x = umap_unbalanced$layout[,1], y = umap_unbalanced$layout[,2], col = Data_unbalanced$Y_unbalanced)
p1c_1 <- ggplot(umap_plot_unbalanced) + geom_point(aes(x=x, y=y, color=col)) + ggtitle("X_Unbalanced")
X_umap_record1 <- which(umap_unbalanced$layout[,1]>=15) # seed 100
X_umap_record2 <- which(umap_unbalanced$layout[,2]<=-1 &
                          umap_unbalanced$layout[,1]<15) 
X_umap_record3 <- which(umap_unbalanced$layout[,2]>-1 &
                          umap_unbalanced$layout[,1]<15) 
X_umap_subject1 = X_unbalanced[X_umap_record1,]
X_umap_subject2 = X_unbalanced[X_umap_record2,]
X_umap_subject3 = X_unbalanced[X_umap_record3,]


# 3D
umap_unbalanced_3D <- umap(Data_unbalanced[0:91], n_components = 3, random_state = 15)
umap_plot_unbalanced_3D <- data.frame(x = umap_unbalanced_3D$layout[,1], y = umap_unbalanced_3D$layout[,2], z = umap_unbalanced_3D$layout[,3], col = Data_unbalanced$Y_unbalanced)
p1c_2 <- ggplot(umap_plot_unbalanced_3D, aes(x=x, y=y, z=z, color=col)) + theme_void() + axes_3D() + stat_3D() + ggtitle("X_Unbalanced")

# balanced
umap_balanced <- umap(Data_balanced[0:91], n_components = 2, random_state = 15)
umap_plot_balanced <- data.frame(x = umap_balanced$layout[,1], y = umap_balanced$layout[,2], col = Data_balanced$Y_balanced)
p1c_3 <- ggplot(umap_plot_balanced) + geom_point(aes(x=x, y=y, color=col)) + ggtitle("X_Balanced")
# 3D
umap_balanced_3D <- umap(Data_balanced[0:91], n_components = 3, random_state = 15)
umap_plot_balanced_3D <- data.frame(x = umap_balanced_3D$layout[,1], y = umap_balanced_3D$layout[,2], z = umap_balanced_3D$layout[,3], col = Data_balanced$Y_balanced)
p1c_4 <- ggplot(umap_plot_balanced_3D, aes(x=x, y=y, z=z, color=col)) + theme_void() + axes_3D() + stat_3D() + ggtitle("X_Balanced")

p1c = grid.arrange(p1c_1, p1c_2, p1c_3, p1c_4, ncol = 2)
ggsave("X_UMAP.png", plot = p1c, width = 16, height = 12)


# Group2: X_unbalanced_pain and X_balanced_pain
# (a) PCA
# unbalanced
pca_unbalanced_pain <- prcomp(X_unbalanced_pain, center = T, scale. = TRUE)
pca3d(pca_unbalanced_pain, group = Data_unbalanced$Y_unbalanced, title = "3D PCA plots of unbalanced pain data")
snapshotPCA3d("pca_X_unbalanced_pain_3D.png")
p2a_1 <- autoplot(pca_unbalanced_pain, data = Data_unbalanced, colour = 'Y_unbalanced', title = "pca_unbalanced_pain_2D")
p2a_2 <- fviz_eig(pca_unbalanced_pain)

# balanced
pca_balanced_pain <- prcomp(X_balanced_pain, center = T, scale. = TRUE)
pca3d(pca_balanced_pain, group = Data_balanced$Y_balanced, title = "3D PCA plots of balanced pain data")
snapshotPCA3d("pca_X_balanced_pain_3D.png")
p2a_3 <- autoplot(pca_balanced_pain, data = Data_balanced, colour = 'Y_balanced')
p2a_4 <- fviz_eig(pca_balanced_pain)
# summary(pca_balanced)
# fviz_contrib(pca_balanced, choice = "var", axes = 1, top = 20)
# balanced_var <- get_pca_var(pca_balanced)
# sort(balanced_var$contrib[,1], decreasing = T)
# sink(file = "balanced.txt")
# var$contrib
# sink(file = NULL)

p2a = grid.arrange(p2a_1, p2a_2, p2a_3, p2a_4, ncol = 2)
ggsave("X_pain_PCA.png", plot = p2a, width = 16, height = 8)


# (b) tSNE
# unbalanced
tsne_unbalanced_pain_data = as.matrix(X_unbalanced_pain)
set.seed(100) # change random seed to get different results
tsne_unbalanced_pain <- Rtsne(tsne_unbalanced_pain_data, dims = 2, theta = 0.5, check_duplicates = FALSE)
tsne_plot_unbalanced_pain <- data.frame(x = tsne_unbalanced_pain$Y[,1], y = tsne_unbalanced_pain$Y[,2], col = Data_unbalanced$Y_unbalanced)
p2b_1 <- ggplot(tsne_plot_unbalanced_pain) + geom_point(aes(x=x, y=y, color=col)) + ggtitle("X_Unbalanced_pain (tSNE)")
X_pain_tSNE_record1 <- which(tsne_unbalanced_pain$Y[,1]>20 & tsne_unbalanced_pain$Y[,2]>-15) # seed 100
X_pain_tSNE_record2 <- which(tsne_unbalanced_pain$Y[,1]<=20 & tsne_unbalanced_pain$Y[,1]>=10 & tsne_unbalanced_pain$Y[,2]>=0)
X_pain_tSNE_record3 <- which(7*tsne_unbalanced_pain$Y[,1] + 3*tsne_unbalanced_pain$Y[,2] < 120) 
X_pain_tSNE_subject1 = X_unbalanced_pain[X_pain_tSNE_record1,]
X_pain_tSNE_subject2 = X_unbalanced_pain[X_pain_tSNE_record2,]
X_pain_tSNE_subject3 = X_unbalanced_pain[X_pain_tSNE_record3,]

# 3D
tsne_unbalanced_pain_3D <- Rtsne(tsne_unbalanced_pain_data, dims = 3, theta = 0.5, check_duplicates = FALSE) 
tsne_plot_unbalanced_pain_3D <- data.frame(x = tsne_unbalanced_pain_3D$Y[,1], y = tsne_unbalanced_pain_3D$Y[,2], z = tsne_unbalanced_pain_3D$Y[,3], col = Data_unbalanced$Y_unbalanced)
p2b_2 <- ggplot(tsne_plot_unbalanced_pain_3D, aes(x=x, y=y, z=z, color=col)) + theme_void() + axes_3D() + stat_3D() + ggtitle("X_Unbalanced_pain (tSNE)")

# balanced
tsne_balanced_pain_data = as.matrix(X_balanced_pain)
set.seed(100)
tsne_balanced_pain <- Rtsne(tsne_balanced_pain_data, dims = 2, theta = 0.5, check_duplicates = FALSE)
tsne_plot_balanced_pain <- data.frame(x = tsne_balanced_pain$Y[,1], y = tsne_balanced_pain$Y[,2], col = Data_balanced$Y_balanced)
p2b_3 <- ggplot(tsne_plot_balanced_pain) + geom_point(aes(x=x, y=y, color=col)) + ggtitle("X_Balanced_pain (tSNE)")
# 3D
tsne_balanced_pain_3D <- Rtsne(tsne_balanced_pain_data, dims = 3, theta = 0.5, check_duplicates = FALSE) 
tsne_plot_balanced_pain_3D <- data.frame(x = tsne_balanced_pain_3D$Y[,1], y = tsne_balanced_pain_3D$Y[,2], z = tsne_balanced_pain_3D$Y[,3], col = Data_balanced$Y_balanced)
p2b_4 <- ggplot(tsne_plot_balanced_pain_3D, aes(x=x, y=y, z=z, color=col)) + theme_void() + axes_3D() + stat_3D() + ggtitle("X_Balanced_pain (tSNE)")

p2b = grid.arrange(p2b_1, p2b_2, p2b_3, p2b_4, ncol = 2)
ggsave("X_pain_tSNE.png", plot = p2b, width = 16, height = 12)


# (c) UMAP
# unbalanced
umap_unbalanced_pain <- umap(X_unbalanced_pain, n_components = 2, random_state = 15)
umap_plot_unbalanced_pain <- data.frame(x = umap_unbalanced_pain$layout[,1], y = umap_unbalanced_pain$layout[,2], col = Data_unbalanced$Y_unbalanced)
p2c_1 <- ggplot(umap_plot_unbalanced_pain) + geom_point(aes(x=x, y=y, color=col)) + ggtitle("X_Unbalanced_pain (UMAP)")
X_pain_umap_record1 <- which(umap_unbalanced_pain$layout[,1]>=15) # seed 100
X_pain_umap_record2 <- which(umap_unbalanced_pain$layout[,2]>=0 & umap_unbalanced_pain$layout[,1]<15) 
X_pain_umap_record3 <- which(umap_unbalanced_pain$layout[,2]<0 & umap_unbalanced_pain$layout[,1]<15 & umap_unbalanced_pain$layout[,2]>-7.5) 
X_pain_umap_record4 <- which(umap_unbalanced_pain$layout[,2]<=-7.5) 
X_pain_umap_subject1 = X_unbalanced_pain[X_pain_umap_record1,]
X_pain_umap_subject2 = X_unbalanced_pain[X_pain_umap_record2,]
X_pain_umap_subject3 = X_unbalanced_pain[X_pain_umap_record3,]
X_pain_umap_subject4 = X_unbalanced_pain[X_pain_umap_record4,]

# 3D
umap_unbalanced_pain_3D <- umap(X_unbalanced_pain, n_components = 3, random_state = 15)
umap_plot_unbalanced_pain_3D <- data.frame(x = umap_unbalanced_pain_3D$layout[,1], y = umap_unbalanced_pain_3D$layout[,2], z = umap_unbalanced_pain_3D$layout[,3], col = Data_unbalanced$Y_unbalanced)
p2c_2 <- ggplot(umap_plot_unbalanced_pain_3D, aes(x=x, y=y, z=z, color=col)) + theme_void() + axes_3D() + stat_3D() + ggtitle("X_Unbalanced_pain (UMAP)")

# balanced
umap_balanced_pain <- umap(X_balanced_pain, n_components = 2, random_state = 15)
umap_plot_balanced_pain <- data.frame(x = umap_balanced_pain$layout[,1], y = umap_balanced_pain$layout[,2], col = Data_balanced$Y_balanced)
p2c_3 <- ggplot(umap_plot_balanced_pain) + geom_point(aes(x=x, y=y, color=col)) + ggtitle("X_balanced_pain (UMAP)")
# 3D
umap_balanced_pain_3D <- umap(X_balanced_pain, n_components = 3, random_state = 15)
umap_plot_balanced_pain_3D <- data.frame(x = umap_balanced_pain_3D$layout[,1], y = umap_balanced_pain_3D$layout[,2], z = umap_balanced_pain_3D$layout[,3], col = Data_balanced$Y_balanced)
p2c_4 <- ggplot(umap_plot_balanced_pain_3D, aes(x=x, y=y, z=z, color=col)) + theme_void() + axes_3D() + stat_3D() + ggtitle("X_balanced_pain (UMAP)")

p2c = grid.arrange(p2c_1, p2c_2, p2c_3, p2c_4, ncol = 2)
ggsave("X_pain_UMAP.png", plot = p2c, width = 16, height = 12)



# Group3: X_unbalanced_other and X_balanced_other
# (a) PCA
# unbalanced
pca_unbalanced_other <- prcomp(X_unbalanced_other, center = T, scale. = TRUE)
pca3d(pca_unbalanced_other, group = Data_unbalanced$Y_unbalanced, title = "3D PCA plots of unbalanced other data")
snapshotPCA3d("pca_X_unbalanced_other_3D.png")
p3a_1 <- autoplot(pca_unbalanced_other, data = Data_unbalanced, colour = 'Y_unbalanced', title = "pca_unbalanced_other_2D")
p3a_2 <- fviz_eig(pca_unbalanced_other)

# balanced
pca_balanced_other <- prcomp(X_balanced_other, center = T, scale. = TRUE)
pca3d(pca_balanced_other, group = Data_balanced$Y_balanced, title = "3D PCA plots of balanced other data")
snapshotPCA3d("pca_X_balanced_other_3D.png")
p3a_3 <- autoplot(pca_balanced_other, data = Data_balanced, colour = 'Y_balanced')
p3a_4 <- fviz_eig(pca_balanced_other)

p3a = grid.arrange(p3a_1, p3a_2, p3a_3, p3a_4, ncol = 2)
ggsave("X_other_PCA.png", plot = p3a, width = 16, height = 8)


# (b) tSNE
# unbalanced
tsne_unbalanced_other_data = as.matrix(X_unbalanced_other)
set.seed(100) # change random seed to get different results
tsne_unbalanced_other <- Rtsne(tsne_unbalanced_other_data, dims = 2, theta = 0.5, check_duplicates = FALSE)
tsne_plot_unbalanced_other <- data.frame(x = tsne_unbalanced_other$Y[,1], y = tsne_unbalanced_other$Y[,2], col = Data_unbalanced$Y_unbalanced)
p3b_1 <- ggplot(tsne_plot_unbalanced_other) + geom_point(aes(x=x, y=y, color=col)) + ggtitle("X_Unbalanced_other (tSNE)")
X_other_tSNE_record1 <- which(tsne_unbalanced_other$Y[,1]>9) # seed 100
X_other_tSNE_record2 <- which(tsne_unbalanced_other$Y[,1]<=9) 
X_other_tSNE_subject1 = X_unbalanced_other[X_other_tSNE_record1,]
X_other_tSNE_subject2 = X_unbalanced_other[X_other_tSNE_record2,]

# 3D
tsne_unbalanced_other_3D <- Rtsne(tsne_unbalanced_other_data, dims = 3, theta = 0.5, check_duplicates = FALSE) 
tsne_plot_unbalanced_other_3D <- data.frame(x = tsne_unbalanced_other_3D$Y[,1], y = tsne_unbalanced_other_3D$Y[,2], z = tsne_unbalanced_other_3D$Y[,3], col = Data_unbalanced$Y_unbalanced)
p3b_2 <- ggplot(tsne_plot_unbalanced_other_3D, aes(x=x, y=y, z=z, color=col)) + theme_void() + axes_3D() + stat_3D() + ggtitle("X_Unbalanced_other (tSNE)")

# balanced
tsne_balanced_other_data = as.matrix(X_balanced_other)
set.seed(100)
tsne_balanced_other <- Rtsne(tsne_balanced_other_data, dims = 2, theta = 0.5, check_duplicates = FALSE)
tsne_plot_balanced_other <- data.frame(x = tsne_balanced_other$Y[,1], y = tsne_balanced_other$Y[,2], col = Data_balanced$Y_balanced)
p3b_3 <- ggplot(tsne_plot_balanced_other) + geom_point(aes(x=x, y=y, color=col)) + ggtitle("X_Balanced_other (tSNE)")
# 3D
tsne_balanced_other_3D <- Rtsne(tsne_balanced_other_data, dims = 3, theta = 0.5, check_duplicates = FALSE) 
tsne_plot_balanced_other_3D <- data.frame(x = tsne_balanced_other_3D$Y[,1], y = tsne_balanced_other_3D$Y[,2], z = tsne_balanced_other_3D$Y[,3], col = Data_balanced$Y_balanced)
p3b_4 <- ggplot(tsne_plot_balanced_other_3D, aes(x=x, y=y, z=z, color=col)) + theme_void() + axes_3D() + stat_3D() + ggtitle("X_Balanced_other (tSNE)")

p3b = grid.arrange(p3b_1, p3b_2, p3b_3, p3b_4, ncol = 2)
ggsave("X_other_tSNE.png", plot = p3b, width = 16, height = 12)


# (c) UMAP
# unbalanced
set.seed(100)
umap_unbalanced_other <- umap(X_unbalanced_other, n_components = 2, random_state = 15)
umap_plot_unbalanced_other <- data.frame(x = umap_unbalanced_other$layout[,1], y = umap_unbalanced_other$layout[,2], col = Data_unbalanced$Y_unbalanced)
p3c_1 <- ggplot(umap_plot_unbalanced_other) + geom_point(aes(x=x, y=y, color=col)) + ggtitle("X_Unbalanced_other (UMAP)")
X_other_umap_record1 <- which(umap_unbalanced_other$layout[,1]>6) # seed 100
X_other_umap_record2 <- which(umap_unbalanced_other$layout[,2]>=7) 
X_other_umap_record3 <- which(umap_unbalanced_other$layout[,1]<=-7) 
X_other_umap_record4 <- which(umap_unbalanced_other$layout[,1]<=5 &
                                umap_unbalanced_other$layout[,1]>=-7 &
                                umap_unbalanced_other$layout[,2]>=-7 &
                                umap_unbalanced_other$layout[,2]<=7) 
X_other_umap_subject1 = X_unbalanced_other[X_other_umap_record1,]
X_other_umap_subject2 = X_unbalanced_other[X_other_umap_record2,]
X_other_umap_subject3 = X_unbalanced_other[X_other_umap_record3,]
X_other_umap_subject4 = X_unbalanced_other[X_other_umap_record4,]


# 3D
umap_unbalanced_other_3D <- umap(X_unbalanced_other, n_components = 3, random_state = 15)
umap_plot_unbalanced_other_3D <- data.frame(x = umap_unbalanced_other_3D$layout[,1], y = umap_unbalanced_other_3D$layout[,2], z = umap_unbalanced_other_3D$layout[,3], col = Data_unbalanced$Y_unbalanced)
p3c_2 <- ggplot(umap_plot_unbalanced_other_3D, aes(x=x, y=y, z=z, color=col)) + theme_void() + axes_3D() + stat_3D() + ggtitle("X_Unbalanced_other (UMAP)")

# balanced
umap_balanced_other <- umap(X_balanced_other, n_components = 2, random_state = 15)
umap_plot_balanced_other <- data.frame(x = umap_balanced_other$layout[,1], y = umap_balanced_other$layout[,2], col = Data_balanced$Y_balanced)
p3c_3 <- ggplot(umap_plot_balanced_other) + geom_point(aes(x=x, y=y, color=col)) + ggtitle("X_balanced_other (UMAP)")
# 3D
umap_balanced_pain_3D <- umap(X_balanced_pain, n_components = 3, random_state = 15)
umap_plot_balanced_pain_3D <- data.frame(x = umap_balanced_pain_3D$layout[,1], y = umap_balanced_pain_3D$layout[,2], z = umap_balanced_pain_3D$layout[,3], col = Data_balanced$Y_balanced)
p3c_4 <- ggplot(umap_plot_balanced_pain_3D, aes(x=x, y=y, z=z, color=col)) + theme_void() + axes_3D() + stat_3D() + ggtitle("X_balanced_pain (UMAP)")

p3c = grid.arrange(p3c_1, p3c_2, p3c_3, p3c_4, ncol = 2)
ggsave("X_other_UMAP.png", plot = p3c, width = 16, height = 12)



# Group4: X_unbalanced_top50 and X_balanced_top50
# (a) PCA
# ...


# (b) tSNE
# unbalanced
tsne_unbalanced_top50_data = as.matrix(GeoPain_unbalanced_top50)
set.seed(100) # change random seed to get different results
tsne_unbalanced_top50 <- Rtsne(tsne_unbalanced_top50_data, dims = 2, theta = 0.5, check_duplicates = FALSE)
tsne_plot_unbalanced_top50 <- data.frame(x = tsne_unbalanced_top50$Y[,1], y = tsne_unbalanced_top50$Y[,2], col = Data_unbalanced$Y_unbalanced)
p4b_1 <- ggplot(tsne_plot_unbalanced_top50) + geom_point(aes(x=x, y=y, color=col)) + ggtitle("X_Unbalanced_top50 (tSNE)")
X_top50_tSNE_record1 <- which(tsne_unbalanced_top50$Y[,2]>37)
X_top50_tSNE_record2 <- which(5*tsne_unbalanced_top50$Y[,1] + 2*tsne_unbalanced_top50$Y[,2] < 25)
X_top50_tSNE_record3 <- which(5*tsne_unbalanced_top50$Y[,1] + 2*tsne_unbalanced_top50$Y[,2] >= 25) 
X_top50_tSNE_subject1 = GeoPain_unbalanced_top50[X_top50_tSNE_record1,]
X_top50_tSNE_subject2 = GeoPain_unbalanced_top50[X_top50_tSNE_record2,]
X_top50_tSNE_subject3 = GeoPain_unbalanced_top50[X_top50_tSNE_record3,]

# 3D
#source("axes_3D.R")
tsne_unbalanced_top50_3D <- Rtsne(tsne_unbalanced_top50_data, dims = 3, theta = 0.5, check_duplicates = FALSE) 
tsne_plot_unbalanced_top50_3D <- data.frame(x = tsne_unbalanced_top50_3D$Y[,1], y = tsne_unbalanced_top50_3D$Y[,2], z = tsne_unbalanced_top50_3D$Y[,3], col = Data_unbalanced$Y_unbalanced)
p4b_2 <- ggplot(tsne_plot_unbalanced_top50_3D, aes(x=x, y=y, z=z, color=col)) + theme_void() + axes_3D() + stat_3D() + ggtitle("X_Unbalanced_top50 (tSNE)")

# balanced
tsne_balanced_top50_data = as.matrix(GeoPain_balanced_top50)
set.seed(100)
tsne_balanced_top50 <- Rtsne(tsne_balanced_top50_data, dims = 2, theta = 0.5, check_duplicates = FALSE)
tsne_plot_balanced_top50 <- data.frame(x = tsne_balanced_top50$Y[,1], y = tsne_balanced_top50$Y[,2], col = Data_balanced$Y_balanced)
p4b_3 <- ggplot(tsne_plot_balanced_top50) + geom_point(aes(x=x, y=y, color=col)) + ggtitle("X_Balanced_top50 (tSNE)")
# 3D
tsne_balanced_top50_3D <- Rtsne(tsne_balanced_top50_data, dims = 3, theta = 0.5, check_duplicates = FALSE) 
tsne_plot_balanced_top50_3D <- data.frame(x = tsne_balanced_top50_3D$Y[,1], y = tsne_balanced_top50_3D$Y[,2], z = tsne_balanced_top50_3D$Y[,3], col = Data_balanced$Y_balanced)
p4b_4 <- ggplot(tsne_plot_balanced_top50_3D, aes(x=x, y=y, z=z, color=col)) + theme_void() + axes_3D() + stat_3D() + ggtitle("X_Balanced_top50 (tSNE)")

p4b = grid.arrange(p4b_1, p4b_2, p4b_3, p4b_4, ncol = 2)
ggsave("X_top50_tSNE.png", plot = p4b, width = 16, height = 12)


# (c) UMAP
# unbalanced
umap_unbalanced_top50 <- umap(GeoPain_unbalanced_top50, n_components = 2, random_state = 15)
umap_plot_unbalanced_top50 <- data.frame(x = umap_unbalanced_top50$layout[,1], y = umap_unbalanced_top50$layout[,2], col = Data_unbalanced$Y_unbalanced)
p4c_1 <- ggplot(umap_plot_unbalanced_top50) + geom_point(aes(x=x, y=y, color=col)) + ggtitle("X_Unbalanced_top50 (UMAP)")
X_top50_umap_record1 <- which(umap_unbalanced_top50$layout[,1]>=15) # seed 100
X_top50_umap_record2 <- which(umap_unbalanced_top50$layout[,2]>=0 & umap_unbalanced_top50$layout[,1]<15) 
X_top50_umap_record3 <- which(umap_unbalanced_top50$layout[,2]<0 & umap_unbalanced_top50$layout[,1]<15) 

X_top50_umap_subject1 = GeoPain_unbalanced_top50[X_top50_umap_record1,]
X_top50_umap_subject2 = GeoPain_unbalanced_top50[X_top50_umap_record2,]
X_top50_umap_subject3 = GeoPain_unbalanced_top50[X_top50_umap_record3,]

# 3D
umap_unbalanced_top50_3D <- umap(GeoPain_unbalanced_top50, n_components = 3, random_state = 15)
umap_plot_unbalanced_top50_3D <- data.frame(x = umap_unbalanced_top50_3D$layout[,1], y = umap_unbalanced_top50_3D$layout[,2], z = umap_unbalanced_top50_3D$layout[,3], col = Data_unbalanced$Y_unbalanced)
p4c_2 <- ggplot(umap_plot_unbalanced_top50_3D, aes(x=x, y=y, z=z, color=col)) + theme_void() + axes_3D() + stat_3D() + ggtitle("X_Unbalanced_top50 (UMAP)")

# balanced
umap_balanced_top50 <- umap(GeoPain_balanced_top50, n_components = 2, random_state = 15)
umap_plot_balanced_top50 <- data.frame(x = umap_balanced_top50$layout[,1], y = umap_balanced_top50$layout[,2], col = Data_balanced$Y_balanced)
p4c_3 <- ggplot(umap_plot_balanced_top50) + geom_point(aes(x=x, y=y, color=col)) + ggtitle("X_balanced_top50 (UMAP)")
# 3D
umap_balanced_top50_3D <- umap(GeoPain_balanced_top50, n_components = 3, random_state = 15)
umap_plot_balanced_top50_3D <- data.frame(x = umap_balanced_top50_3D$layout[,1], y = umap_balanced_top50_3D$layout[,2], z = umap_balanced_top50_3D$layout[,3], col = Data_balanced$Y_balanced)
p4c_4 <- ggplot(umap_plot_balanced_top50_3D, aes(x=x, y=y, z=z, color=col)) + theme_void() + axes_3D() + stat_3D() + ggtitle("X_balanced_top50 (UMAP)")

p4c = grid.arrange(p4c_1, p4c_2, p4c_3, p4c_4, ncol = 2)
ggsave("X_top50_UMAP.png", plot = p4c, width = 16, height = 12)


# Group5: X_unbalanced_top50 and X_balanced_top50
# (a) PCA
# ...


# (b) tSNE
# unbalanced
tsne_unbalanced_top50_data = as.matrix(GeoPain_unbalanced_top50)
set.seed(100) # change random seed to get different results
tsne_unbalanced_top50 <- Rtsne(tsne_unbalanced_top50_data, dims = 2, theta = 0.5, check_duplicates = FALSE)
tsne_plot_unbalanced_top50 <- data.frame(x = tsne_unbalanced_top50$Y[,1], y = tsne_unbalanced_top50$Y[,2], col = Data_unbalanced$Y_unbalanced)
p4b_1 <- ggplot(tsne_plot_unbalanced_top50) + geom_point(aes(x=x, y=y, color=col)) + ggtitle("X_Unbalanced_top50 (tSNE)")
X_top50_tSNE_record1 <- which(tsne_unbalanced_top50$Y[,2]>37)
X_top50_tSNE_record2 <- which(5*tsne_unbalanced_top50$Y[,1] + 2*tsne_unbalanced_top50$Y[,2] < 25)
X_top50_tSNE_record3 <- which(5*tsne_unbalanced_top50$Y[,1] + 2*tsne_unbalanced_top50$Y[,2] >= 25) 
X_top50_tSNE_subject1 = GeoPain_unbalanced_top50[X_top50_tSNE_record1,]
X_top50_tSNE_subject2 = GeoPain_unbalanced_top50[X_top50_tSNE_record2,]
X_top50_tSNE_subject3 = GeoPain_unbalanced_top50[X_top50_tSNE_record3,]

# 3D
#source("axes_3D.R")
tsne_unbalanced_top50_3D <- Rtsne(tsne_unbalanced_top50_data, dims = 3, theta = 0.5, check_duplicates = FALSE) 
tsne_plot_unbalanced_top50_3D <- data.frame(x = tsne_unbalanced_top50_3D$Y[,1], y = tsne_unbalanced_top50_3D$Y[,2], z = tsne_unbalanced_top50_3D$Y[,3], col = Data_unbalanced$Y_unbalanced)
p4b_2 <- ggplot(tsne_plot_unbalanced_top50_3D, aes(x=x, y=y, z=z, color=col)) + theme_void() + axes_3D() + stat_3D() + ggtitle("X_Unbalanced_top50 (tSNE)")

# balanced
tsne_balanced_top50_data = as.matrix(GeoPain_balanced_top50)
set.seed(100)
tsne_balanced_top50 <- Rtsne(tsne_balanced_top50_data, dims = 2, theta = 0.5, check_duplicates = FALSE)
tsne_plot_balanced_top50 <- data.frame(x = tsne_balanced_top50$Y[,1], y = tsne_balanced_top50$Y[,2], col = Data_balanced$Y_balanced)
p4b_3 <- ggplot(tsne_plot_balanced_top50) + geom_point(aes(x=x, y=y, color=col)) + ggtitle("X_Balanced_top50 (tSNE)")
# 3D
tsne_balanced_top50_3D <- Rtsne(tsne_balanced_top50_data, dims = 3, theta = 0.5, check_duplicates = FALSE) 
tsne_plot_balanced_top50_3D <- data.frame(x = tsne_balanced_top50_3D$Y[,1], y = tsne_balanced_top50_3D$Y[,2], z = tsne_balanced_top50_3D$Y[,3], col = Data_balanced$Y_balanced)
p4b_4 <- ggplot(tsne_plot_balanced_top50_3D, aes(x=x, y=y, z=z, color=col)) + theme_void() + axes_3D() + stat_3D() + ggtitle("X_Balanced_top50 (tSNE)")

p4b = grid.arrange(p4b_1, p4b_2, p4b_3, p4b_4, ncol = 2)
ggsave("X_top50_tSNE.png", plot = p4b, width = 16, height = 12)


# (c) UMAP
# unbalanced
umap_unbalanced_top50 <- umap(GeoPain_unbalanced_top50, n_components = 2, random_state = 15)
umap_plot_unbalanced_top50 <- data.frame(x = umap_unbalanced_top50$layout[,1], y = umap_unbalanced_top50$layout[,2], col = Data_unbalanced$Y_unbalanced)
p4c_1 <- ggplot(umap_plot_unbalanced_top50) + geom_point(aes(x=x, y=y, color=col)) + ggtitle("X_Unbalanced_top50 (UMAP)")
X_top50_umap_record1 <- which(umap_unbalanced_top50$layout[,1]>=15) # seed 100
X_top50_umap_record2 <- which(umap_unbalanced_top50$layout[,2]>=0 & umap_unbalanced_top50$layout[,1]<15) 
X_top50_umap_record3 <- which(umap_unbalanced_top50$layout[,2]<0 & umap_unbalanced_top50$layout[,1]<15) 

X_top50_umap_subject1 = GeoPain_unbalanced_top50[X_top50_umap_record1,]
X_top50_umap_subject2 = GeoPain_unbalanced_top50[X_top50_umap_record2,]
X_top50_umap_subject3 = GeoPain_unbalanced_top50[X_top50_umap_record3,]

# 3D
umap_unbalanced_top50_3D <- umap(GeoPain_unbalanced_top50, n_components = 3, random_state = 15)
umap_plot_unbalanced_top50_3D <- data.frame(x = umap_unbalanced_top50_3D$layout[,1], y = umap_unbalanced_top50_3D$layout[,2], z = umap_unbalanced_top50_3D$layout[,3], col = Data_unbalanced$Y_unbalanced)
p4c_2 <- ggplot(umap_plot_unbalanced_top50_3D, aes(x=x, y=y, z=z, color=col)) + theme_void() + axes_3D() + stat_3D() + ggtitle("X_Unbalanced_top50 (UMAP)")

# balanced
umap_balanced_top50 <- umap(GeoPain_balanced_top50, n_components = 2, random_state = 15)
umap_plot_balanced_top50 <- data.frame(x = umap_balanced_top50$layout[,1], y = umap_balanced_top50$layout[,2], col = Data_balanced$Y_balanced)
p4c_3 <- ggplot(umap_plot_balanced_top50) + geom_point(aes(x=x, y=y, color=col)) + ggtitle("X_balanced_top50 (UMAP)")
# 3D
umap_balanced_top50_3D <- umap(GeoPain_balanced_top50, n_components = 3, random_state = 15)
umap_plot_balanced_top50_3D <- data.frame(x = umap_balanced_top50_3D$layout[,1], y = umap_balanced_top50_3D$layout[,2], z = umap_balanced_top50_3D$layout[,3], col = Data_balanced$Y_balanced)
p4c_4 <- ggplot(umap_plot_balanced_top50_3D, aes(x=x, y=y, z=z, color=col)) + theme_void() + axes_3D() + stat_3D() + ggtitle("X_balanced_top50 (UMAP)")

p4c = grid.arrange(p4c_1, p4c_2, p4c_3, p4c_4, ncol = 2)
ggsave("X_top50_UMAP.png", plot = p4c, width = 16, height = 12)





















## OLD STUFF FOR CBDA - DISREGARD
write.table(GeoPain_Metadata_CBDA_noCoMorb_final_noHN,
            file="GeoPain_noCoMorb_noAge_noHN.txt",append = F, eol = "\r\n" ,
            row.names = FALSE,col.names = FALSE,sep = ",",fileEncoding = "UTF-8")
write.table(GeoPain_Metadata_CBDA_noCoMorb_Validation_final_noHN,
            file="GeoPain_noCoMorb_noAge_noHN_Validation.txt",append = F, eol = "\r\n" ,
            row.names = FALSE,col.names = FALSE,sep = ",",fileEncoding = "UTF-8")

#save(list = ls(all.names = TRUE), file = "GeoPain_March2021.RData")

## Balancing the Geopain Data without comorbidities
## TRAINING SET
dim(GeoPain_Metadata_CBDA_noCoMorb_final_noHN) # 751  93
X <- GeoPain_Metadata_CBDA_noCoMorb_final_noHN[,-c(1,2)]
dim(X)
X_unbalanced <- as.data.frame(X)
Y_unbalanced <-  GeoPain_Metadata_CBDA_noCoMorb_final_noHN$Outcome
table(Y_unbalanced)
Data_unbalanced <- cbind(X_unbalanced,Y_unbalanced)
Data_balanced <- smotefamily::SMOTE(Data_unbalanced[,-dim(Data_unbalanced)[2]],
                                    Data_unbalanced[,dim(Data_unbalanced)[2]])
X_balanced <- Data_balanced$data[,-dim(Data_unbalanced)[2]]
Y_balanced <- as.numeric(Data_balanced$data[,dim(Data_unbalanced)[2]])
table(Y_balanced)
dim(X_balanced)
IDs <- NULL
IDs <- seq(1,dim(X_balanced)[1],1)
## add ID column first, move diagnosis to 2nd and label it "Outcome"
GeoPain_Metadata_CBDA_noCoMorb_final_noHN_balanced <- cbind(IDs,Y_balanced,X_balanced)
names(GeoPain_Metadata_CBDA_noCoMorb_final_noHN_balanced)[2]<-c("Outcome")
dim(GeoPain_Metadata_CBDA_noCoMorb_final_noHN_balanced) # 1318 x 93
GeoPain_Metadata_CBDA_noCoMorb_final_noHN_balanced$IDs <- 
  as.character((GeoPain_Metadata_CBDA_noCoMorb_final_noHN_balanced$IDs))
names(GeoPain_Metadata_CBDA_noCoMorb_final_noHN_balanced)
table(GeoPain_Metadata_CBDA_noCoMorb_final_noHN_balanced$Outcome) # 688 vs 630 (63)
write.table(GeoPain_Metadata_CBDA_noCoMorb_final_noHN_balanced,
            file="GeoPain_noCoMorb_noAge_noHN_balanced_March2021.txt",append = F, eol = "\r\n" ,
            row.names = FALSE,col.names = FALSE,sep = ",",fileEncoding = "UTF-8")

## Balancing the Geopain Data without comorbidities
## VALIDATION SET
dim(GeoPain_Metadata_CBDA_noCoMorb_Validation_final_noHN) # 1240  93
X <- GeoPain_Metadata_CBDA_noCoMorb_Validation_final_noHN[,-c(1,2)]
dim(X)
X_unbalanced <- as.data.frame(X)
Y_unbalanced <-  GeoPain_Metadata_CBDA_noCoMorb_Validation_final_noHN$Outcome
table(Y_unbalanced) # 1167 vs 73
Data_unbalanced <- cbind(X_unbalanced,Y_unbalanced)
Data_balanced <- smotefamily::SMOTE(Data_unbalanced[,-dim(Data_unbalanced)[2]],
                                    Data_unbalanced[,dim(Data_unbalanced)[2]])
X_balanced <- Data_balanced$data[,-dim(Data_unbalanced)[2]]
Y_balanced <- as.numeric(Data_balanced$data[,dim(Data_unbalanced)[2]])
table(Y_balanced) # 1167 vs 1095
dim(X_balanced) # 2262 x 91
IDs <- NULL
IDs <- seq(1,dim(X_balanced)[1],1)
## add ID column first, move diagnosis to 2nd and label it "Outcome"
GeoPain_Metadata_CBDA_noCoMorb_Validation_final_noHN_balanced <- 
  cbind(IDs,Y_balanced,X_balanced)
names(GeoPain_Metadata_CBDA_noCoMorb_Validation_final_noHN_balanced)[2]<-c("Outcome")
dim(GeoPain_Metadata_CBDA_noCoMorb_Validation_final_noHN_balanced) # 2262 x 93
GeoPain_Metadata_CBDA_noCoMorb_Validation_final_noHN_balanced$IDs <- 
  as.character((GeoPain_Metadata_CBDA_noCoMorb_Validation_final_noHN_balanced$IDs))
names(GeoPain_Metadata_CBDA_noCoMorb_Validation_final_noHN_balanced)
table(GeoPain_Metadata_CBDA_noCoMorb_Validation_final_noHN_balanced$Outcome) # 1167 vs 1095
write.table(GeoPain_Metadata_CBDA_noCoMorb_Validation_final_noHN_balanced,
            file="GeoPain_noCoMorb_noAge_noHN_Validation_March2021_balanced.txt",append = F, eol = "\r\n" ,
            row.names = FALSE,col.names = FALSE,sep = ",",fileEncoding = "UTF-8")






setwd("C:/Users/simeonem/Documents/Papers/GeoPain/")
load("./CBDA/CBDA_M5000_miss0_GeoPain_noCoMorb_noAge_noHN_balanced_VALIDATION.RData")

CBDA_object_Validation_GeoPain_noCoMorb_noAge_noHN_balanced <- 
  CBDA_object_Validation
a4 <- which.max(CBDA_object_Validation$ValidationTable[,2])
CBDA_object_Validation$ValidationTable[a4,2]
names(GeoPain_Metadata_CBDA_noCoMorb_final_noHN)[CBDA_object_Validation_GeoPain_noCoMorb_noAge_noHN_balanced$TopFeaturesAccuracy[1:a4]+2]
CBDA_object_Validation$ConfusionMatrices[a4+2]
feature_set <- NULL
feature_set_temp <- NULL
feature_set_temp <- CBDA_object_Validation$TopFeaturesAccuracy[1:(a4+2)]
feature_set <- names(GeoPain_Metadata_CBDA_noCoMorb_final_noHN)[feature_set_temp+2]
CBDA_GeoPain_Top <- data.frame(GeoPain_Feature=feature_set,
                               Likelihood=c(0,0,CBDA_object_Validation$ValidationTable[1:a4,2]))
write.csv(CBDA_GeoPain_Top,file = "./Manuscript/Tables/CBDA_noCoMorb_noAge_noHN_balanced_Aug5_2021.csv")


a2 <- which.max(CBDA_object_Validation_P32_Mar2021$ValidationTable[,2])
CBDA_object_Validation_P32_Mar2021$ConfusionMatrices[a2+2]
CBDA_object_Validation_P32_Mar2021$SL_Pred_Probability[a2+2]
SL_best <- ifelse(CBDA_object_Validation_P32_Mar2021$SL_Pred_Probability[[a2+2]]>0.5,1,0)
Missed <- ifelse((Ypred-SL_best)!=0,"Missed","Correct")
data.frame(Original=Ypred,
           PredLikelihood=CBDA_object_Validation_P32_Mar2021$SL_Pred_Probability[a2+2],
           Predicted=SL_best,Missed=Missed)





## Dermatome V9 instances
V9_records <- NULL
V9_records <- 
  c(which(GeoPain_Data_R$pain.head.dermatomes.0 == "V9"),
    which(GeoPain_Data_R$pain.head.dermatomes.1 == "V9"),
    which(GeoPain_Data_R$pain.head.dermatomes.2 == "V9"),
    which(GeoPain_Data_R$pain.head.dermatomes.3 == "V9"),
    which(GeoPain_Data_R$pain.head.dermatomes.4 == "V9"),
    which(GeoPain_Data_R$pain.head.dermatomes.5 == "V9"),
    which(GeoPain_Data_R$pain.head.dermatomes.6 == "V9"),
    which(GeoPain_Data_R$pain.head.dermatomes.7 == "V9"),
    which(GeoPain_Data_R$pain.head.dermatomes.8 == "V9"),
    which(GeoPain_Data_R$pain.head.dermatomes.9 == "V9"))
length(V9_records) # 152
V9_records <- sort(V9_records)
V9_patients <- unique(GeoPain_Data_R$guid[V9_records])
length(V9_patients) #43

## Patients that recorded V9 WITHOUT comorbidities Migr/TMD
length(unique(GeoPain_Data_R$guid[V9_records
                                  [-match(intersect(records_comorbidity
                                                    ,V9_records),V9_records)]])) # 31
## Patients that recorded V9 WITH comorbidities Migr/TMD
length(unique(GeoPain_Data_R$guid[V9_records
                                  [match(intersect(records_comorbidity
                                                    ,V9_records),V9_records)]])) # 12
# In the original dataset Eric sent, there are 152 records with V9 (out of ~2,000 records).
# These records belong to 43 patients. # Out of these 43 patients, 31 have comorbidities, 12 don't.
# We have 52 V9 records (across 12 patients) in the data used to obtain the latest CBDA results.
# So,  according to our criterium, we will NOT eliminate V9 from the data.
# I will proceed to predict the comorbidity dataset  that was set aside.
# The outcome will be a label (something like " strongly-leaning towards
# either Migraine or TMD") for each record. The label will reflect the likelihood/probability returned by the CBDA predictive model. I will use the following probability ranges to define the label:
# 1) [0,0.3] --> strongly Migraine
# 2) [0.3,0.5] --> leaning Migraine
# 3) [0.5,0.7] --> leaning TMD
# 4) [0.7,1] --> strongly TMD
# We will basically dissect comorbidities' likelihood in real-time. 
# That outcome can be compared to the set of comorbidities each patient
# declares upon enro