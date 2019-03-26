datasetname<-c("ARUK_dataset2_full_responders_cleaned_WITH_RECODES_DEIDENTIFIED_with_DEPR.csv")
dataset <- read.csv(file=datasetname, header=TRUE, stringsAsFactors=FALSE)

## hack fix for mistake in code calculating dsm5 disorder categories
dataset$d_aud[dataset$dsm5_disorder_symptom_score<2] <- 0
dataset$d_aud[dataset$dsm5_disorder_symptom_score>=2 & dataset$dsm5_disorder_symptom_score<4] <- 1
dataset$d_aud[dataset$dsm5_disorder_symptom_score>=4 & dataset$dsm5_disorder_symptom_score<6] <- 2
dataset$d_aud[dataset$dsm5_disorder_symptom_score>=6] <- 3
dataset$dsm5_disorder <- dataset$d_aud
##

dataset$dsm5_aud1<-ifelse(dataset$dsm5_disorder=="0",c("0"),c("1"))
dataset$dsm5_aud2<-ifelse(dataset$dsm5_disorder=="0" | dataset$dsm5_disorder=="1",c("0"),c("1"))
table(dataset$dsm5_aud1)
table(dataset$dsm5_aud2)
#colnames(dataset)
save(dataset,file="dataset.Rdata")
write.csv(dataset,"ARUK_dataset2_full_responders_cleaned_WITH_RECODES_DEIDENTIFIED_with_DEPR.csv", row.names=FALSE)



