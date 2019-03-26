source("BayesPrev_functions.R")
library(ggplot2)
 
# filelist <- list.files(path="/Users/David/Documents/Coding/BayesScripts/TLFB Workbooks", pattern="*.csv")
# #length(filelist)
# TLFB_id <- list()
# TLFB_gender <- list()
# TLFB_hazardous <- list()
# TLFB_binge <- list()
# TLFB_binge_not_haz <- list()
# resvect_out <- list()
# id_string <- list()
# library(zoo)
# 
# for (i in 1:length(filelist)) {
#   ## retrieve daily drinking data from TLFB files:
#   temp <- paste("/Users/David/Documents/Coding/BayesScripts/TLFB Workbooks/",filelist[i],sep="")
#   numstring <- c()
#   numstring <- sub(".* P ", "", temp)
#   id_string <- sub(" E.*", "", numstring)
#   
#   temp <- as.data.frame(read.csv(temp,header=FALSE))
#   res <- temp[,1:7]
#   id <- c(as.vector(res[7,1]))
#   gender <- as.character(res[7,2])
#   res <- res[10:52,]
#   resvect <- c(t(res))
#   resvect <- as.numeric(resvect[resvect>=0.0])
#   resvect <- resvect[!is.na(resvect)]
#   resvect_sum <- sum(resvect)
#   resvect_out[[i]] <- list(id_string,gender,resvect,resvect_sum)
# 
#   ## all 90 days:
#   hazard1 <- rollapply(resvect, 7, sum ) 
#   hazardous_a <- c(FALSE)
#   hazardous_a[gender == "Male" & any(hazard1>21)] <- TRUE#males
#   hazardous_a[gender == "Female" & any(hazard1>14)] <- TRUE #females
#   hazard2m <- rollapply(resvect, 7, function(x) sum(x>3)) #males
#   hazard2f <- rollapply(resvect, 7, function(x) sum(x>2)) #females
#   
#   hazardous_b <- c(FALSE)
#   hazardous_b[gender == "Male" & any(hazard2m >=5)] <- TRUE #all
#   hazardous_b[gender == "Female" & any(hazard2f >=5)] <- TRUE #all
#   
#   ## TLFB refvar 1
#   hazardous <- c(FALSE)
#   hazardous[hazardous_a == TRUE || hazardous_b == TRUE] <- TRUE 
#   
#   ## TLFB refvar 2
#   binge<-c(FALSE)
#   binge[any(resvect>=8) & gender == "Male"] <- TRUE #males
#   binge[any(resvect>=6) & gender == "Female"] <- TRUE #females
#   
#   ## TLFB refvar 3
#   binge_not_haz <- c(FALSE)
#   binge_not_haz[binge == TRUE & hazardous == FALSE] <- TRUE
#   
#   ## outputs
#   TLFB_id[[i]] <- id_string
#   TLFB_gender[[i]] <- gender
#   TLFB_hazardous[[i]] <- hazardous
#   TLFB_binge[[i]] <- binge
#   TLFB_binge_not_haz[[i]] <- binge_not_haz
# 
# }
# 
# TLFBdata <- data.frame(matrix(unlist(resvect_out), nrow=length(filelist), byrow=T))
# TLFBdata_temp1 <- unfactorize(TLFBdata[,3:92])
# TLFBdata_temp2 <- as.numeric(as.character(TLFBdata[,1]))
# TLFBdata_temp3 <- as.numeric(as.character(TLFBdata[,93]))
# TLFBdata <- cbind(TLFBdata_temp2, TLFBdata[,2], TLFBdata_temp1, TLFBdata_temp3)
# colnames(TLFBdata) <- c("id", "gender", 1:90, "sum90days")
# save(TLFBdata,file="TLFBdata.Rdata")

load("TLFBdata.Rdata")
df <- TLFBdata
#colnames(df)

## 30 day period:
Period <- c("first_30_days","mid_30_days","recent_30_days")

## calculate total number of drinks consumed for each person over each 30 day period
sum30days <- list()
j <- 3
for(i in 1:3) {
  sum30days[[i]] <- apply(df[,j:(j+29)],1,sum)
  j <- j + 30 
}

Total_Consumption <- unlist(sum30days) 
TLFB1_df <- data.frame(Period,Total_Consumption)

filename<-paste0("Outfiles/total_drinks_per_person_each30days.pdf")
pdf(filename)
p <- ggplot(TLFB1_df, aes(x=Total_Consumption, group=Period, colour=Period, fill=Period)) 
p <- p + geom_density(alpha = 0.2, size=1.5) 
p <- p + scale_x_continuous(limits = c(0, 300))
p
dev.off()

testdiff1 <- summary(aov(Total_Consumption~Period,data=TLFB1_df))

## calculate number of heavy (binge) drinking days for each person over each 30 day period
binge30days <- list()
j <- 3
for(i in 1:3) {
  ifelse(df$gender == "Male",
         binge30days[[i]] <- apply(df[,j:(j+29)],1,function(x) sum(x >= 8)),
         binge30days[[i]] <- apply(df[,j:(j+29)],1,function(x) sum(x >= 6)))
  j <- j + 30
}

Num_Binge_Days <- unlist(binge30days)
TLFB2_df <- data.frame(Period,Num_Binge_Days)

filename<-paste0("Outfiles/binge_days_per_person_each30days.pdf")
pdf(filename)
p <- ggplot(TLFB2_df, aes(y=Num_Binge_Days, x=Period, colour=Period)) 
p <- p + geom_boxplot()  #density(alpha = 0.2, size=1.5) 
#p <- p + scale_x_continuous(limits = c(0,15))
p
dev.off()

filename<-paste0("Outfiles/Summary_aov_binge_days_per_person_each30days.txt")
capture.output(summary(aov(Num_Binge_Days~Period,data=TLFB2_df)),file=filename)


## calculate number of binge drinkers for each day over each 30 day period
binge30drinkers <- list()
j <- 3
for(i in 1:3) {
  ifelse(df$gender == "Male",
         binge30drinkers[[i]] <- apply(df[,j:(j+29)], 2, function(x) sum(x >= 8)),
         binge30drinkers[[i]] <- apply(df[,j:(j+29)], 2, function(x) sum(x >= 6)))
  j <- j + 30
}

Binge_Drinkers_Each_Day <- unlist(binge10drinkers)
TLFB3_df <- data.frame(Period,Binge_Drinkers_Each_Day)

filename<-paste0("Outfiles/number_binge_drinkers_each30days.pdf")
pdf(filename)
p <- ggplot(TLFB3_df, aes(x=Binge_Drinkers_Each_Day, group=Period, colour=Period, fill=Period)) 
p <- p + geom_density(alpha = 0.2, size=1.5) 
p <- p + scale_x_continuous(limits = c(5,50))
p
dev.off()

filename<-paste0("Outfiles/Summary_aov_number_binge_drinkers_each30days.txt")
capture.output(summary(aov(Binge_Drinkers_Each_Day~Period,data=TLFB3_df)),file=filename)


## 10 day Period:
Period <- c("days 1-10",
            "days 11-20",
            "days 21-30",
            "days 31-40",
            "days 41-50",
            "days 51-60",
            "days 61-70",
            "days 71-80",
            "days 81-90")

## calculate total number of drinks consumed for each person over each 10 day period
sum10days <- list()
j <- 3
for(i in 1:9) {
  sum10days[[i]] <- apply(df[,j:(j+9)],1,sum)
  j <- j + 10 
}

Total_Consumption <- unlist(sum10days)
TLFB4_df <- data.frame(Period,Total_Consumption)

filename<-paste0("Outfiles/total_drinks_per_person_each10days.pdf")
pdf(filename)
p <- ggplot(TLFB4_df, aes(y=Total_Consumption, x= Period, colour=Period)) 
p <- p + geom_boxplot()  #alpha = 0.2, size=1.5) 
#p <- p + scale_x_continuous(limits = c(0, 300))
p <- p + theme(legend.position="none")
p
dev.off()  

filename<-paste0("Outfiles/Summary_aov_total_drinks_per_person_each10days.txt")
capture.output(summary(aov(Total_Consumption~Period,data=TLFB4_df)),file=filename)


## calculate number of heavy (binge) drinking days for each person over each 10 day period
binge10days <- list()
j <- 3
for(i in 1:9) {
  ifelse(df$gender == "Male",
         binge10days[[i]] <- apply(df[,j:(j+9)],1,function(x) sum(x >= 8)),
         binge10days[[i]] <- apply(df[,j:(j+9)],1,function(x) sum(x >= 6)))
  j <- j + 10
}

Num_Binge_Days <- unlist(binge10days)
TLFB5_df <- data.frame(Period,Num_Binge_Days)

filename<-paste0("Outfiles/binge_days_per_person_each10days.pdf")
pdf(filename)
p <- ggplot(TLFB5_df, aes(y=Num_Binge_Days, x=Period, colour=Period)) 
p <- p + geom_boxplot()  #density(alpha = 0.2, size=1.5) 
#p <- p + scale_x_continuous(limits = c(0,15))
p <- p + theme(legend.position="none")
p
dev.off()

filename<-paste0("Outfiles/Summary_aov_binge_days_per_person_each10days.txt")
capture.output(summary(aov(Num_Binge_Days~Period,data=TLFB5_df)),file=filename)


## calculate total number of binge drinkers for each day over each 10 day period
binge10drinkers <- list()
j <- 3
for(i in 1:9) {
  ifelse(df$gender == "Male",
         binge10drinkers[[i]] <- apply(df[,j:(j+9)], 2, function(x) sum(x >= 8)),
         binge10drinkers[[i]] <- apply(df[,j:(j+9)], 2, function(x) sum(x >= 6)))
         j <- j + 10
}

filename<-paste0("Outfiles/number_binge_drinkers_each10days.pdf")
pdf(filename)
Binge_Drinkers_Each_Day <- unlist(binge10drinkers)
TLFB6_df <- data.frame(Period,Binge_Drinkers_Each_Day)

p <- ggplot(TLFB6_df, aes(y=Binge_Drinkers_Each_Day, x= Period, colour=Period)) 
p <- p + geom_boxplot()  #alpha = 0.2, size=1.5) 
#p <- p + scale_x_continuous(limits = c(0, 300))
p <- p + theme(legend.position="none")
p
dev.off()

filename<-paste0("Outfiles/Summary_aov_number_binge_drinkers_each10days.txt")
capture.output(summary(aov(Binge_Drinkers_Each_Day~Period,data=TLFB6_df)),file=filename)



######### other flavours of outcomes:
library(zoo)

## 30 day period:
TLFB_hazardous30 <- list()
TLFB_hazardous_30 <- list()
TLFB_binge30 <- list()
TLFB_binge_30 <- list()
TLFB_binge_not_haz30 <- list()
TLFB_binge_not_haz_30 <- list()

j <- 3
for(k in 1:3) {
  for (i in 1:length(df[,1])) {
    resvect <- c(t(df[i,j:(j+29)]))
    hazard1<-rollapply(resvect, 7, sum ) 
    hazardous_a<-c(FALSE)
    hazardous_a[df$gender == "Male" & any(hazard1>21)]<-TRUE#females
    hazardous_a[df$gender == "Female" & any(hazard1>14)]<-TRUE #females
    #hazard2<-c()
    hazard2m <- rollapply(resvect, 7, function(x) sum(x>3)) #males
    hazard2f <- rollapply(resvect, 7, function(x) sum(x>2)) #females
  
    #hazard2[gender == "Male"]<-rollapply(resvect, 5, function(x) all(x>=3)) #males
    #hazard2[gender == "Female"]<-rollapply(resvect, 5, function(x) all(x>=2))  #females
    hazardous_b<-c(FALSE)
    hazardous_b[df$gender == "Male" & any(hazard2m >=5)]<-TRUE #all
    hazardous_b[df$gender == "Female" & any(hazard2f >=5)]<-TRUE #all
    
    ## TLFB refvar 1
    hazardous<-c(FALSE)
    hazardous[hazardous_a == TRUE || hazardous_b == TRUE]<- TRUE 
    
    ## TLFB refvar 2
    binge<-c(FALSE)
    binge[any(resvect>=8) && df$gender == "Male"]<-TRUE #males
    binge[any(resvect>=6) && df$gender == "Female"]<-TRUE #females
    
    ## TLFB refvar 3
    binge_not_haz<-c(FALSE)
    binge_not_haz[binge == TRUE && hazardous == FALSE]<-TRUE
      
    #outputs
    TLFB_id[[i]]<-df$id
    TLFB_gender[[i]]<-df$gender
    TLFB_hazardous30[[i]]<-hazardous
    TLFB_binge30[[i]]<-binge
    TLFB_binge_not_haz30[[i]]<-binge_not_haz
    
  }
  TLFB_hazardous_30[[k]] <- TLFB_hazardous30
  TLFB_binge_30[[k]] <- TLFB_binge30
  TLFB_binge_not_haz_30[[k]] <- TLFB_binge_not_haz30
  j <- j + 30
}

# table(unlist(TLFB_hazardous_30[[1]]))
# table(unlist(TLFB_hazardous_30[[2]]))
# table(unlist(TLFB_hazardous_30[[3]]))
# table(unlist(TLFB_binge_30[[1]]))
# table(unlist(TLFB_binge_30[[2]]))
# table(unlist(TLFB_binge_30[[3]]))
# table(unlist(TLFB_binge_not_haz_30[[1]]))
# table(unlist(TLFB_binge_not_haz_30[[2]]))
# table(unlist(TLFB_binge_not_haz_30[[3]]))


## 10 day period:
TLFB_hazardous10 <- list()
TLFB_hazardous_10 <- list()
TLFB_binge10 <- list()
TLFB_binge_10 <- list()
TLFB_binge_not_haz10 <- list()
TLFB_binge_not_haz_10 <- list()

j <- 3
for(k in 1:9) {
  for (i in 1:length(df[,1])) {
    resvect <- c(t(df[i,j:(j+9)]))
    hazard1<-rollapply(resvect, 7, sum ) 
    hazardous_a<-c(FALSE)
    hazardous_a[df$gender == "Male" & any(hazard1>21)]<-TRUE#females
    hazardous_a[df$gender == "Female" & any(hazard1>14)]<-TRUE #females
    #hazard2<-c()
    hazard2m <- rollapply(resvect, 7, function(x) sum(x>3)) #males
    hazard2f <- rollapply(resvect, 7, function(x) sum(x>2)) #females
    
    #hazard2[gender == "Male"]<-rollapply(resvect, 5, function(x) all(x>=3)) #males
    #hazard2[gender == "Female"]<-rollapply(resvect, 5, function(x) all(x>=2))  #females
    hazardous_b<-c(FALSE)
    hazardous_b[df$gender == "Male" & any(hazard2m >=5)]<-TRUE #all
    hazardous_b[df$gender == "Female" & any(hazard2f >=5)]<-TRUE #all
    
    ## TLFB refvar 1
    hazardous<-c(FALSE)
    hazardous[hazardous_a == TRUE || hazardous_b == TRUE]<- TRUE 
    
    ## TLFB refvar 2
    binge<-c(FALSE)
    binge[any(resvect>=8) && df$gender == "Male"]<-TRUE #males
    binge[any(resvect>=6) && df$gender == "Female"]<-TRUE #females
    
    ## TLFB refvar 3
    binge_not_haz<-c(FALSE)
    binge_not_haz[binge == TRUE && hazardous == FALSE]<-TRUE
    
    #outputs
    TLFB_id[[i]]<-df$id
    TLFB_gender[[i]]<-df$gender
    TLFB_hazardous10[[i]]<-hazardous
    TLFB_binge10[[i]]<-binge
    TLFB_binge_not_haz10[[i]]<-binge_not_haz
    
  }
  TLFB_hazardous_10[[k]] <- TLFB_hazardous10
  TLFB_binge_10[[k]] <- TLFB_binge10
  TLFB_binge_not_haz_10[[k]] <- TLFB_binge_not_haz10
  j <- j + 10
}




## create vars to write to main data file
## 30 days
df.haz <- data.frame(matrix(unlist(TLFB_hazardous_30), nrow = length(df[,1]), byrow=T))
haz.30days.all3 <- apply(df.haz,1,function(x) all(x==TRUE))
haz.30days.2outof3 <- apply(df.haz,1,function(x) (sum(x==TRUE))>=2)

df.binge <- data.frame(matrix(unlist(TLFB_binge_30), nrow = length(df[,1]), byrow=T))
binge.30days.all3 <- apply(df.binge,1,function(x) all(x==TRUE))
binge.30days.2outof3 <- apply(df.binge,1,function(x) (sum(x==TRUE))>=2)

df.binge.not.haz <- data.frame(matrix(unlist(TLFB_binge_not_haz_30), nrow = length(df[,1]), byrow=T))
binge.not.haz.30days.all3 <- apply(df.binge.not.haz,1,function(x) all(x==TRUE))
binge.not.haz.30days.2outof3 <- apply(df.binge.not.haz,1,function(x) (sum(x==TRUE))>=2)

## 10 days
df.haz <- data.frame(matrix(unlist(TLFB_hazardous_10), nrow = length(df[,1]), byrow=T))
haz.10days.all9 <- apply(df.haz,1,function(x) all(x==TRUE))
haz.10days.5outof9 <- apply(df.haz,1,function(x) (sum(x==TRUE))>=5)

df.binge <- data.frame(matrix(unlist(TLFB_binge_10), nrow = length(df[,1]), byrow=T))
binge.10days.all9 <- apply(df.binge,1,function(x) all(x==TRUE))
binge.10days.5outof9 <- apply(df.binge,1,function(x) (sum(x==TRUE))>=5)

df.binge.not.haz <- data.frame(matrix(unlist(TLFB_binge_not_haz_10), nrow = length(df[,1]), byrow=T))
binge.not.haz.10days.all9 <- apply(df.binge.not.haz,1,function(x) all(x==TRUE))
binge.not.haz.10days.5outof9 <- apply(df.binge.not.haz,1,function(x) (sum(x==TRUE))>=5)

TLFB <- cbind(df$id, df$sum90days, TLFB_hazardous, haz.30days.all3, haz.30days.2outof3,
              TLFB_binge, binge.30days.all3, binge.30days.2outof3,
              TLFB_binge_not_haz, binge.not.haz.30days.all3, binge.not.haz.30days.2outof3,
              haz.10days.all9, haz.10days.5outof9,
              binge.10days.all9, binge.10days.5outof9,
              binge.not.haz.10days.all9, binge.not.haz.10days.5outof9
              )
colnames(TLFB)[1:2] <- c("ID_CIDI", "sum90days")
TLFB <- as.data.frame(apply(TLFB,2,as.numeric))
save(TLFB,file="TLFBvars.Rdata")
datasetname <- c("ARUK_dataset2_full_responders_cleaned_WITH_RECODES_DEIDENTIFIED_with_DEPR.csv")
dataset <- read.csv(file=datasetname, header=TRUE)
m1 <- merge(dataset,TLFB, by="ID_CIDI",suffixes="")
#write.csv(m1,datasetname, row.names=FALSE)



