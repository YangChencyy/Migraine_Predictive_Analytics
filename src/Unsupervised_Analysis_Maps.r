GeoPain_Data_R_nocomorb <- GeoPain_Data_R[-records_comorbidity,]
dim(GeoPain_Data_R_nocomorb) # 779 x 129
dim(GeoPain_Data_R) # 2050 x 112
dim(GeoPain_Metadata_CBDA) # 2050 x 112
GeoPain_Metadata_CBDA_noCoMorb <- GeoPain_Metadata_CBDA[-records_comorbidity,]
dim(GeoPain_Metadata_CBDA_noCoMorb)  # 779

dim(GeoPain_Metadata_CBDA_noCoMorb_final_noHN) # 751

GeoPain_Metadata_Unsupervised_noCoMorb_final_noHN <- 
  rbind(GeoPain_Metadata_CBDA_noCoMorb_final_noHN,
        GeoPain_Metadata_CBDA_noCoMorb_Validation_final_noHN)
dim(GeoPain_Metadata_Unsupervised_noCoMorb_final_noHN) # 1991   93

NO_comorbidity_IDs_map
Comorbidity_IDs_map
Unbalanced_IDs_map <- rbind(NO_comorbidity_IDs_map,Comorbidity_IDs_map)
dim(Unbalanced_IDs_map)

## ALL
## Figure 11 in the UnsupervisedAnalysis document
X_tSNE_record1 <- which(tsne_unbalanced$Y[,1]>6 & 
                          tsne_unbalanced$Y[,2]>=30) 
X_tSNE_record2 <- which(tsne_unbalanced$Y[,1]>6 &
                          tsne_unbalanced_pain$Y[,2]>=-10 &
                          tsne_unbalanced$Y[,2]<30)
X_tSNE_record3 <- which(2.2*tsne_unbalanced_pain$Y[,1] +
                          tsne_unbalanced_pain$Y[,2] < 25)
X_tSNE_subject1 = X_unbalanced[X_tSNE_record1,]
X_tSNE_subject2 = X_unbalanced[X_tSNE_record2,]
X_tSNE_subject3 = X_unbalanced[X_tSNE_record3,]
X_tSNE_record1_IDs <- Unbalanced_IDs_map[X_tSNE_record1,]
X_tSNE_record2_IDs <- Unbalanced_IDs_map[X_tSNE_record2,]
X_tSNE_record3_IDs <- Unbalanced_IDs_map[X_tSNE_record3,]

Patients_IDs_tSNE_record1 <- 
  cbind(names(sort(table(X_tSNE_record1_IDs[,2]),decreasing = T)),
        unname(sort(table(X_tSNE_record1_IDs[,2]),decreasing = T)))
Patients_IDs_tSNE_record1 <- 
  data.frame(PatientID=Patients_IDs_tSNE_record1[,1],
             Frequency=Patients_IDs_tSNE_record1[,2])
Patients_IDs_tSNE_record2 <- 
  cbind(names(sort(table(X_tSNE_record2_IDs[,2]),decreasing = T)),
        unname(sort(table(X_tSNE_record2_IDs[,2]),decreasing = T)))
Patients_IDs_tSNE_record2 <- 
  data.frame(PatientID=Patients_IDs_tSNE_record2[,1],
             Frequency=Patients_IDs_tSNE_record2[,2])
Patients_IDs_tSNE_record3 <- 
  cbind(names(sort(table(X_tSNE_record3_IDs[,2]),decreasing = T)),
        unname(sort(table(X_tSNE_record3_IDs[,2]),decreasing = T)))
Patients_IDs_tSNE_record3 <- 
  data.frame(PatientID=Patients_IDs_tSNE_record3[,1],
             Frequency=Patients_IDs_tSNE_record3[,2])

## PAIN ONLY
## Figure 13 in the UnsupervisedAnalysis document
X_pain_tSNE_record1 <- which(tsne_unbalanced_pain$Y[,1]>20 &
                               tsne_unbalanced_pain$Y[,2]>-15) # seed 100
X_pain_tSNE_record2 <- which(tsne_unbalanced_pain$Y[,1]<=20 &
                               tsne_unbalanced_pain$Y[,1]>=10 &
                               tsne_unbalanced_pain$Y[,2]>=0)
X_pain_tSNE_record3 <- which(7*tsne_unbalanced_pain$Y[,1] + 3*tsne_unbalanced_pain$Y[,2] < 120) 
X_pain_tSNE_subject1 = X_unbalanced_pain[X_pain_tSNE_record1,]
X_pain_tSNE_subject2 = X_unbalanced_pain[X_pain_tSNE_record2,]
X_pain_tSNE_subject3 = X_unbalanced_pain[X_pain_tSNE_record3,]
X_pain_tSNE_record1_IDs <- Unbalanced_IDs_map[X_pain_tSNE_record1,]
X_pain_tSNE_record2_IDs <- Unbalanced_IDs_map[X_pain_tSNE_record2,]
X_pain_tSNE_record3_IDs <- Unbalanced_IDs_map[X_pain_tSNE_record3,]

Patients_IDs_pain_tSNE_record1 <- 
  cbind(names(sort(table(X_pain_tSNE_record1_IDs[,2]),decreasing = T)),
        unname(sort(table(X_pain_tSNE_record1_IDs[,2]),decreasing = T)))
Patients_IDs_pain_tSNE_record1 <- 
  data.frame(PatientID=Patients_IDs_other_tSNE_record1[,1],
             Frequency=Patients_IDs_other_tSNE_record1[,2])
Patients_IDs_pain_tSNE_record2 <- 
  cbind(names(sort(table(X_pain_tSNE_record2_IDs[,2]),decreasing = T)),
        unname(sort(table(X_pain_tSNE_record2_IDs[,2]),decreasing = T)))
Patients_IDs_pain_tSNE_record2 <- 
  data.frame(PatientID=Patients_IDs_pain_tSNE_record2[,1],
             Frequency=Patients_IDs_pain_tSNE_record2[,2])
Patients_IDs_pain_tSNE_record3 <- 
  cbind(names(sort(table(X_pain_tSNE_record3_IDs[,2]),decreasing = T)),
        unname(sort(table(X_pain_tSNE_record3_IDs[,2]),decreasing = T)))
Patients_IDs_pain_tSNE_record3 <- 
  data.frame(PatientID=Patients_IDs_pain_tSNE_record3[,1],
             Frequency=Patients_IDs_pain_tSNE_record3[,2])






## OTHER
## Figure 12 in the UnsupervisedAnalysis document
X_other_tSNE_record1 <- which(tsne_unbalanced_other$Y[,1]>9) # seed 100
X_other_tSNE_record2 <- which(tsne_unbalanced_other$Y[,1]<=9) 
X_other_tSNE_subject1 = X_unbalanced_other[X_other_tSNE_record1,]
X_other_tSNE_subject2 = X_unbalanced_other[X_other_tSNE_record2,]
X_other_tSNE_record1_IDs <- Unbalanced_IDs_map[X_other_tSNE_record1,]
X_other_tSNE_record2_IDs <- Unbalanced_IDs_map[X_other_tSNE_record2,]
Patients_IDs_other_tSNE_record1 <- 
  cbind(names(sort(table(X_other_tSNE_record1_IDs[,2]),decreasing = T)),
        unname(sort(table(X_other_tSNE_record1_IDs[,2]),decreasing = T)))
Patients_IDs_other_tSNE_record1 <- 
  data.frame(PatientID=Patients_IDs_other_tSNE_record1[,1],
             Frequency=Patients_IDs_other_tSNE_record1[,2])
Patients_IDs_other_tSNE_record2 <- 
  cbind(names(sort(table(X_other_tSNE_record2_IDs[,2]),decreasing = T)),
        unname(sort(table(X_other_tSNE_record2_IDs[,2]),decreasing = T)))
Patients_IDs_other_tSNE_record2 <- 
  data.frame(PatientID=Patients_IDs_other_tSNE_record2[,1],
             Frequency=Patients_IDs_other_tSNE_record2[,2])


## Top50
## Figure 14 in the UnsupervisedAnalysis document
X_top50_tSNE_record1 <- which(tsne_unbalanced_top50$Y[,2]>37)
X_top50_tSNE_record2 <- which(5*tsne_unbalanced_top50$Y[,1] + 2*tsne_unbalanced_top50$Y[,2] >= 25)
X_top50_tSNE_record3 <- which(5*tsne_unbalanced_top50$Y[,1] + 2*tsne_unbalanced_top50$Y[,2] < 25) 
X_top50_tSNE_subject1 = GeoPain_unbalanced_top50[X_top50_tSNE_record1,]
X_top50_tSNE_subject2 = GeoPain_unbalanced_top50[X_top50_tSNE_record2,]
X_top50_tSNE_subject3 = GeoPain_unbalanced_top50[X_top50_tSNE_record3,]

X_top50_tSNE_record1_IDs <- Unbalanced_IDs_map[X_top50_tSNE_record1,]
X_top50_tSNE_record2_IDs <- Unbalanced_IDs_map[X_top50_tSNE_record2,]
X_top50_tSNE_record3_IDs <- Unbalanced_IDs_map[X_top50_tSNE_record3,]

Patients_IDs_top50_tSNE_record1 <- 
  cbind(names(sort(table(X_top50_tSNE_record1_IDs[,2]),decreasing = T)),
        unname(sort(table(X_top50_tSNE_record1_IDs[,2]),decreasing = T)))
Patients_IDs_top50_tSNE_record1 <- 
  data.frame(PatientID=Patients_IDs_top50_tSNE_record1[,1],
             Frequency=Patients_IDs_top50_tSNE_record1[,2])
Patients_IDs_top50_tSNE_record2 <- 
  cbind(names(sort(table(X_top50_tSNE_record2_IDs[,2]),decreasing = T)),
        unname(sort(table(X_top50_tSNE_record2_IDs[,2]),decreasing = T)))
Patients_IDs_top50_tSNE_record2 <- 
  data.frame(PatientID=Patients_IDs_top50_tSNE_record2[,1],
             Frequency=Patients_IDs_top50_tSNE_record2[,2])
Patients_IDs_top50_tSNE_record3 <- 
  cbind(names(sort(table(X_top50_tSNE_record3_IDs[,2]),decreasing = T)),
        unname(sort(table(X_top50_tSNE_record3_IDs[,2]),decreasing = T)))
Patients_IDs_top50_tSNE_record3 <- 
  data.frame(PatientID=Patients_IDs_top50_tSNE_record3[,1],
             Frequency=Patients_IDs_top50_tSNE_record3[,2])




library(openxlsx)
Patients_IDs_tSNE_record3
dataset_names <- list('Cluster 1 - tSNE all' = data.frame(Record=X_tSNE_record1_IDs[,1],UniqueID=X_tSNE_record1_IDs[,2]),
                      'Cluster 1 - tSNE all Patients' = Patients_IDs_pain_tSNE_record1,
                      'Cluster 2 - tSNE all' = data.frame(Record=X_tSNE_record2_IDs[,1],UniqueID=X_tSNE_record2_IDs[,2]),
                      'Cluster 2 - tSNE all Patients' = Patients_IDs_pain_tSNE_record2,
                      'Cluster 3 - tSNE all' = data.frame(Record=X_tSNE_record3_IDs[,1],UniqueID=X_tSNE_record3_IDs[,2]),
                      'Cluster 3 - tSNE all Patients' = Patients_IDs_pain_tSNE_record3,
                      'Cluster 1 - tSNE PAIN' = data.frame(Record=X_pain_tSNE_record1_IDs[,1],UniqueID=X_pain_tSNE_record1_IDs[,2]),
                      'Cluster 1 tSNE PAIN Patients' = Patients_IDs_pain_tSNE_record1,
                      'Cluster 2 - tSNE PAIN' = data.frame(Record=X_pain_tSNE_record2_IDs[,1],UniqueID=X_pain_tSNE_record2_IDs[,2]),
                      'Cluster 2 tSNE PAIN Patients' = Patients_IDs_pain_tSNE_record2,
                      'Cluster 3 - tSNE PAIN' = data.frame(Record=X_pain_tSNE_record3_IDs[,1],UniqueID=X_pain_tSNE_record3_IDs[,2]),
                      'Cluster 3 tSNE PAIN Patients' = Patients_IDs_pain_tSNE_record3,
                      'Cluster 1 - tSNE OTHER' = data.frame(Record=X_other_tSNE_record1_IDs[,1],UniqueID=X_other_tSNE_record1_IDs[,2]),
                      'Cluster 1 tSNE OTHER Patients' = Patients_IDs_other_tSNE_record1,
                      'Cluster 2 tSNE OTHER' = data.frame(Record=X_other_tSNE_record2_IDs[,1],UniqueID=X_other_tSNE_record2_IDs[,2]),
                      'Cluster 2 tSNE OTHER Patients' = Patients_IDs_other_tSNE_record2,
                      'Cluster 1 - tSNE TOP50' = data.frame(Record=X_top50_tSNE_record1_IDs[,1],UniqueID=X_top50_tSNE_record1_IDs[,2]),
                      'Cluster 1 tSNE TOP50 Patients' = Patients_IDs_top50_tSNE_record1,
                      'Cluster 2 - tSNE TOP50' = data.frame(Record=X_top50_tSNE_record2_IDs[,1],UniqueID=X_top50_tSNE_record2_IDs[,2]),
                      'Cluster 2 tSNE TOP50 Patients' = Patients_IDs_top50_tSNE_record2,
                      'Cluster 3 - tSNE TOP50' = data.frame(Record=X_top50_tSNE_record3_IDs[,1],UniqueID=X_top50_tSNE_record3_IDs[,2]),
                      'Cluster 3 tSNE TOP50 Patients' = Patients_IDs_top50_tSNE_record3)
write.xlsx(dataset_names, file = 'tSNE_clusters.xlsx',colNames = TRUE)

## Figure 7 in the UnsupervisedAnalysis document
X_umap_record1 <- which(umap_unbalanced$layout[,1]>=15) # seed 100
X_umap_record2 <- which(umap_unbalanced$layout[,2]<=-1 &
                          umap_unbalanced$layout[,1]<15) 
X_umap_record3 <- which(umap_unbalanced$layout[,2]>-1 &
                          umap_unbalanced$layout[,1]<15) 
X_umap_subject1 = X_unbalanced_pain[X_umap_record1,]
X_umap_subject2 = X_unbalanced_pain[X_umap_record2,]
X_umap_subject3 = X_unbalanced_pain[X_umap_record3,]
X_umap_record1_IDs <- Unbalanced_IDs_map[X_umap_record1,]
X_umap_record2_IDs <- Unbalanced_IDs_map[X_umap_record2,]
X_umap_record2_IDs <- Unbalanced_IDs_map[X_umap_record3,]

Patients_IDs_umap_record1 <- 
  cbind(names(sort(table(X_umap_record1_IDs[,2]),decreasing = T)),
        unname(sort(table(X_umap_record1_IDs[,2]),decreasing = T)))
Patients_IDs_umap_record1 <- 
  data.frame(PatientID=Patients_IDs_umap_record1[,1],
             Frequency=Patients_IDs_umap_record1[,2])
Patients_IDs_umap_record2 <- 
  cbind(names(sort(table(X_umap_record2_IDs[,2]),decreasing = T)),
        unname(sort(table(X_umap_record2_IDs[,2]),decreasing = T)))
Patients_IDs_umap_record2 <- 
  data.frame(PatientID=Patients_IDs_umap_record2[,1],
             Frequency=Patients_IDs_umap_record2[,2])
Patients_IDs_umap_record3 <- 
  cbind(names(sort(table(X_umap_record3_IDs[,2]),decreasing = T)),
        unname(sort(table(X_umap_record3_IDs[,2]),decreasing = T)))
Patients_IDs_umap_record3 <- 
  data.frame(PatientID=Patients_IDs_umap_record3[,1],
             Frequency=Patients_IDs_umap_record3[,2])

## Figure 9 in the UnsupervisedAnalysis document
X_pain_umap_record1 <- which(umap_unbalanced_pain$layout[,1]>=15) # seed 100
X_pain_umap_record2 <- which(umap_unbalanced_pain$layout[,2]>=0 &
                               umap_unbalanced_pain$layout[,1]<15) 
X_pain_umap_record3 <- which(umap_unbalanced_pain$layout[,2]<0 &
                               umap_unbalanced_pain$layout[,1]<15 &
                               umap_unbalanced_pain$layout[,2]>-7.5) 
X_pain_umap_record4 <- which(umap_unbalanced_pain$layout[,2]<=-7.5) 
X_pain_umap_subject1 = X_unbalanced_pain[X_pain_umap_record1,]
X_pain_umap_subject2 = X_unbalanced_pain[X_pain_umap_record2,]
X_pain_umap_subject3 = X_unbalanced_pain[X_pain_umap_record3,]
X_pain_umap_subject4 = X_unbalanced_pain[X_pain_umap_record4,]
X_pain_umap_record1_IDs <- Unbalanced_IDs_map[X_pain_umap_record1,]
X_pain_umap_record2_IDs <- Unbalanced_IDs_map[X_pain_umap_record2,]
X_pain_umap_record3_IDs <- Unbalanced_IDs_map[X_pain_umap_record3,]
X_pain_umap_record4_IDs <- Unbalanced_IDs_map[X_pain_umap_record4,]

Patients_IDs_pain_umap_record1 <- 
  cbind(names(sort(table(X_pain_umap_record1_IDs[,2]),decreasing = T)),
        unname(sort(table(X_pain_umap_record1_IDs[,2]),decreasing = T)))
Patients_IDs_pain_umap_record1 <- 
  data.frame(PatientID=Patients_IDs_pain_umap_record1[,1],
             Frequency=Patients_IDs_pain_umap_record1[,2])
Patients_IDs_pain_umap_record2 <- 
  cbind(names(sort(table(X_pain_umap_record2_IDs[,2]),decreasing = T)),
        unname(sort(table(X_pain_umap_record2_IDs[,2]),decreasing = T)))
Patients_IDs_pain_umap_record2 <- 
  data.frame(PatientID=Patients_IDs_pain_umap_record2[,1],
             Frequency=Patients_IDs_pain_umap_record2[,2])
Patients_IDs_pain_umap_record3 <- 
  cbind(names(sort(table(X_pain_umap_record3_IDs[,2]),decreasing = T)),
        unname(sort(table(X_pain_umap_record3_IDs[,2]),decreasing = T)))
Patients_IDs_pain_umap_record3 <- 
  data.frame(PatientID=Patients_IDs_pain_umap_record3[,1],
             Frequency=Patients_IDs_pain_umap_record3[,2])
Patients_IDs_pain_umap_record4 <- 
  cbind(names(sort(table(X_pain_umap_record4_IDs[,2]),decreasing = T)),
        unname(sort(table(X_pain_umap_record4_IDs[,2]),decreasing = T)))
Patients_IDs_pain_umap_record4 <- 
  data.frame(PatientID=Patients_IDs_pain_umap_record4[,1],
             Frequency=Patients_IDs_pain_umap_record4[,2])


## Figure 8 in the UnsupervisedAnalysis document
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
X_other_umap_record1_IDs <- Unbalanced_IDs_map[X_other_umap_record1,]
X_other_umap_record2_IDs <- Unbalanced_IDs_map[X_other_umap_record2,]
X_other_umap_record3_IDs <- Unbalanced_IDs_map[X_other_umap_record3,]
X_other_umap_record4_IDs <- Unbalanced_IDs_map[X_other_umap_record4,]

Patients_IDs_other_umap_record1 <- 
  cbind(names(sort(table(X_other_umap_record1_IDs[,2]),decreasing = T)),
        unname(sort(table(X_other_umap_record1_IDs[,2]),decreasing = T)))
Patients_IDs_other_umap_record1 <- 
  data.frame(PatientID=Patients_IDs_other_umap_record1[,1],
             Frequency=Patients_IDs_other_umap_record1[,2])
Patients_IDs_other_umap_record2 <- 
  cbind(names(sort(table(X_other_umap_record2_IDs[,2]),decreasing = T)),
        unname(sort(table(X_other_umap_record2_IDs[,2]),decreasing = T)))
Patients_IDs_other_umap_record2 <- 
  data.frame(PatientID=Patients_IDs_other_umap_record2[,1],
             Frequency=Patients_IDs_other_umap_record2[,2])
Patients_IDs_other_umap_record3 <- 
  cbind(names(sort(table(X_other_umap_record3_IDs[,2]),decreasing = T)),
        unname(sort(table(X_other_umap_record3_IDs[,2]),decreasing = T)))
Patients_IDs_other_umap_record3 <- 
  data.frame(PatientID=Patients_IDs_other_umap_record3[,1],
             Frequency=Patients_IDs_other_umap_record3[,2])
Patients_IDs_other_umap_record4 <- 
  cbind(names(sort(table(X_other_umap_record4_IDs[,2]),decreasing = T)),
        unname(sort(table(X_other_umap_record4_IDs[,2]),decreasing = T)))
Patients_IDs_other_umap_record4 <- 
  data.frame(PatientID=Patients_IDs_other_umap_record4[,1],
             Frequency=Patients_IDs_other_umap_record4[,2])


## Figure 10 in the UnsupervisedAnalysis document
X_top50_umap_record1 <- which(umap_unbalanced_top50$layout[,1]>=15) # seed 100
X_top50_umap_record2 <- which(umap_unbalanced_top50$layout[,2]>=0 & umap_unbalanced_top50$layout[,1]<15) 
X_top50_umap_record3 <- which(umap_unbalanced_top50$layout[,2]<0 & umap_unbalanced_top50$layout[,1]<15) 

X_top50_umap_subject1 = GeoPain_unbalanced_top50[X_top50_umap_record1,]
X_top50_umap_subject2 = GeoPain_unbalanced_top50[X_top50_umap_record2,]
X_top50_umap_subject3 = GeoPain_unbalanced_top50[X_top50_umap_record3,]

X_top50_umap_record1_IDs <- Unbalanced_IDs_map[X_top50_umap_record1,]
X_top50_umap_record2_IDs <- Unbalanced_IDs_map[X_top50_umap_record2,]
X_top50_umap_record3_IDs <- Unbalanced_IDs_map[X_top50_umap_record3,]

Patients_IDs_top50_umap_record1 <- 
  cbind(names(sort(table(X_top50_umap_record1_IDs[,2]),decreasing = T)),
        unname(sort(table(X_top50_umap_record1_IDs[,2]),decreasing = T)))
Patients_IDs_top50_umap_record1 <- 
  data.frame(PatientID=Patients_IDs_top50_umap_record1[,1],
             Frequency=Patients_IDs_top50_umap_record1[,2])
Patients_IDs_top50_umap_record2 <- 
  cbind(names(sort(table(X_top50_umap_record2_IDs[,2]),decreasing = T)),
        unname(sort(table(X_top50_umap_record2_IDs[,2]),decreasing = T)))
Patients_IDs_top50_umap_record2 <- 
  data.frame(PatientID=Patients_IDs_top50_umap_record2[,1],
             Frequency=Patients_IDs_top50_umap_record2[,2])
Patients_IDs_top50_umap_record3 <- 
  cbind(names(sort(table(X_top50_umap_record3_IDs[,2]),decreasing = T)),
        unname(sort(table(X_top50_umap_record3_IDs[,2]),decreasing = T)))
Patients_IDs_top50_umap_record3 <- 
  data.frame(PatientID=Patients_IDs_top50_umap_record3[,1],
             Frequency=Patients_IDs_top50_umap_record3[,2])




dataset_names <- list('Cluster 1 - UMAP all' = data.frame(Record=X_umap_record1_IDs[,1],UniqueID=X_umap_record1_IDs[,2]),
                      'Cluster 1 - UMAP all Patients' = Patients_IDs_umap_record1,
                      'Cluster 2 - UMAP all' = data.frame(Record=X_umap_record2_IDs[,1],UniqueID=X_umap_record2_IDs[,2]),
                      'Cluster 2 - UMAP all Patients' = Patients_IDs_umap_record2,
                      'Cluster 3 - UMAP all' = data.frame(Record=X_umap_record3_IDs[,1],UniqueID=X_umap_record3_IDs[,2]),
                      'Cluster 3 - UMAP all Patients' = Patients_IDs_umap_record3,
                      'Cluster 1 - UMAP PAIN' = data.frame(Record=X_pain_umap_record1_IDs[,1],UniqueID=X_pain_umap_record1_IDs[,2]),
                      'Cluster 1 - UMAP PAIN Patients' = Patients_IDs_pain_umap_record1,
                      'Cluster 2 - UMAP PAIN' = data.frame(Record=X_pain_umap_record2_IDs[,1],UniqueID=X_pain_umap_record2_IDs[,2]),
                      'Cluster 2 - UMAP PAIN Patients' = Patients_IDs_pain_umap_record2,
                      'Cluster 3 - UMAP PAIN' = data.frame(Record=X_pain_umap_record3_IDs[,1],UniqueID=X_pain_umap_record3_IDs[,2]),
                      'Cluster 3 - UMAP PAIN Patients' = Patients_IDs_pain_umap_record3,
                      'Cluster 4 - UMAP PAIN' = data.frame(Record=X_pain_umap_record4_IDs[,1],UniqueID=X_pain_umap_record4_IDs[,2]),
                      'Cluster 4 - UMAP PAIN Patients' = Patients_IDs_pain_umap_record4,
                      'Cluster 1 - UMAP OTHER' = X_other_umap_record1_IDs,
                      'Cluster 1 - UMAP OTHER Patients' = Patients_IDs_other_umap_record1,
                      'Cluster 2 - UMAP OTHER' = X_other_umap_record2_IDs,
                      'Cluster 2 - UMAP OTHER Patients' = Patients_IDs_other_umap_record2,
                      'Cluster 3 - UMAP OTHER' = X_other_umap_record3_IDs,
                      'Cluster 3 - UMAP OTHER Patients' = Patients_IDs_other_umap_record3,
                      'Cluster 4 - UMAP OTHER' = X_other_umap_record4_IDs,
                      'Cluster 4 - UMAP OTHER Patients' = Patients_IDs_other_umap_record4,
                      'Cluster 1 - UMAP TOP50' = data.frame(Record=X_umap_record1_IDs[,1],UniqueID=X_umap_record1_IDs[,2]),
                      'Cluster 1 - UMAP TOP50 Patients' = Patients_IDs_umap_record1,
                      'Cluster 2 - UMAP TOP50' = data.frame(Record=X_umap_record2_IDs[,1],UniqueID=X_umap_record2_IDs[,2]),
                      'Cluster 2 - UMAP TOP50 Patients' = Patients_IDs_umap_record2,
                      'Cluster 3 - UMAP TOP50' = data.frame(Record=X_umap_record3_IDs[,1],UniqueID=X_umap_record3_IDs[,2]),
                      'Cluster 3 - UMAP TOP50 Patients' = Patients_IDs_umap_record3)
write.xlsx(dataset_names, file = 'UMAP_clusters.xlsx')

