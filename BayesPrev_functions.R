# BayesPrev. 
# Copyright (C) 2013, David Foxcroft
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

source("BayesPrev.R")

######################################################################
### function to remove factor levels from data frame
unfactorize <- function(df) {
  for(i in which(sapply(df, class) == "factor")) df[[i]] = as.numeric(as.character(df[[i]]))
  return(df)
}
### end function

######################################################################
### function to analyse time line follow back (TLFB) data
TLFB.analysis <- function() {
  library(ggplot2)
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
  p <- ggplot(TLFB1_df, aes(x=Total_Consumption, group=Period, colour=Period, fill=Period)) 
  p <- p + geom_density(alpha = 0.2, size=1.5) 
  p <- p + scale_x_continuous(limits = c(0, 300))
  ggsave(filename)
  
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
  p <- ggplot(TLFB2_df, aes(y=Num_Binge_Days, x=Period, colour=Period)) 
  p <- p + geom_boxplot()  
  ggsave(filename)
  
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
  
  Binge_Drinkers_Each_Day <- unlist(binge30drinkers)
  TLFB3_df <- data.frame(Period,Binge_Drinkers_Each_Day)
  
  filename<-paste0("Outfiles/number_binge_drinkers_each30days.pdf")
  p <- ggplot(TLFB3_df, aes(x=Binge_Drinkers_Each_Day, group=Period, colour=Period, fill=Period)) 
  p <- p + geom_density(alpha = 0.2, size=1.5) 
  p <- p + scale_x_continuous(limits = c(5,50))
  ggsave(filename)
  
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
  p <- ggplot(TLFB4_df, aes(y=Total_Consumption, x= Period, colour=Period)) 
  p <- p + geom_boxplot()  
  p <- p + theme(legend.position="none")
  ggsave(filename)
  
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
  p <- ggplot(TLFB5_df, aes(y=Num_Binge_Days, x=Period, colour=Period)) 
  p <- p + geom_boxplot()  
  p <- p + theme(legend.position="none")
  ggsave(filename)
  
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
  
  Binge_Drinkers_Each_Day <- unlist(binge10drinkers)
  TLFB6_df <- data.frame(Period,Binge_Drinkers_Each_Day)
  
  p <- ggplot(TLFB6_df, aes(y=Binge_Drinkers_Each_Day, x= Period, colour=Period)) 
  p <- p + geom_boxplot()  
  p <- p + theme(legend.position="none")
  ggsave(filename)
  dev.off()
  filename<-paste0("Outfiles/Summary_aov_number_binge_drinkers_each10days.txt")
  capture.output(summary(aov(Binge_Drinkers_Each_Day~Period,data=TLFB6_df)),file=filename)
  
  
  return(NULL)
  
}
### end function

############################################################################
### plot sample using postcodes and google maps
get.sites <- function() {
  library(XML)
  library(googleVis)
  
  calc.sites <- function() {
    datasetname<-c("ARUK_dataset2_full_responders_cleaned_WITH_RECODES_DEIDENTIFIED_with_DEPR.csv")
    dataset <- read.csv(file=datasetname, header=TRUE, stringsAsFactors=FALSE)
    
    study.postcodes <- unlist(dataset$depr.standard_postcode[!is.na(dataset$depr.standard_postcode)])
    study.postcodes.short <- substr(study.postcodes,1,nchar(study.postcodes)-3)
    
    ll <- lapply(study.postcodes,
                 function(str){
                   u <- paste('http://maps.google.com/maps/api/geocode/xml?sensor=false&address=',str)
                   doc <-  xmlTreeParse(u, useInternal=TRUE)
                   lat=xpathApply(doc,'/GeocodeResponse/result/geometry/location/lat',xmlValue)[1]
                   lng=xpathApply(doc,'/GeocodeResponse/result/geometry/location/lng',xmlValue)[1]
                   Sys.sleep(0.2)
                   c(code = str,lat = lat, lng = lng)
                 })
    
    llshort <- lapply(study.postcodes.short,
                      function(str){
                        u <- paste('http://maps.google.com/maps/api/geocode/xml?sensor=false&address=',str)
                        doc <-  xmlTreeParse(u, useInternal=TRUE)
                        lat=xpathApply(doc,'/GeocodeResponse/result/geometry/location/lat',xmlValue)[1]
                        lng=xpathApply(doc,'/GeocodeResponse/result/geometry/location/lng',xmlValue)[1]
                        Sys.sleep(0.2)
                        c(code = str,lat = lat, lng = lng)
                      })
    
    
    lldata <- data.frame(do.call(rbind,ll))
    lldata.short <- data.frame(do.call(rbind,llshort))
    
    #format the latlong coordinates into the proper format lat:long
    #latLongFormatted<-lapply(lldata,FUN=function(x){
    #  paste(lldata$lat,lldata$lng,sep=":")
    #})
    latLongFormatted<-lapply(lldata.short,FUN=function(x){
      paste(lldata.short$lat,lldata.short$lng,sep=":")
    })
    plotData<-data.frame(latlong=latLongFormatted)
    save(plotData, file="sites.Rdata")
    return(plotData)
  }
  
  ifelse(file.exists("sites.Rdata"),load("sites.Rdata"),calc.sites())
  
  sites<-gvisMap(plotData,locationvar="latlong.lng", options=list(mapType='normal'))
  #plot(sites)
}
### end function

############################################################################
### Read in data for iterative analysis through different reference / index tests
getdata <- function() {
  
  ########### Read in data from full dataset ################################
  rwdata <- function(){
    #source("analset.R")
    path1 <- paste(path,datasetname,sep="")
    dataset <- read.csv(file=path1, header=TRUE, stringsAsFactors=FALSE)
    #dataset <- droplevels(dataset)
    ifelse(gender=="Male",sex<-1,sex<-2)   # replace male and female with codes 1 and 2
    #newdata <- dataset  # all data
    newdata <- subset(dataset, Gender==sex)  #by gender
    #newdata <- subset(dataset, *AgeVar*==agegrp) #by age
    #newdata <- subset(dataset, Gender==sex && *AgeVar*==agegrp)  # gender / age combination
    testdata <- c()
    testdata <- table(newdata[[testname]],newdata[[refname]])
    testdata <- as.data.frame.matrix(testdata)
    score <- row.names(testdata)
    score <- as.numeric(score)+1[scoremin==0]
    testdata <- cbind(score,testdata)
    testdata <- as.data.frame.matrix(testdata)
    row.names(testdata) <- c(1:length(testdata$score))
    colnames(testdata) <- c("score","nn","np")
    countmin <- scoremin+1[scoremin==0]
    countmax <- scoremax
    for(i in countmin:countmax) {             
      newrow <- c(i+1,0,0)
      testdata$score <- as.numeric(testdata$score)
      if ((testdata$score[i+1]-testdata$score[i])>1 || is.na(testdata$score[i+1])) testdata <- rbind(testdata[1:3],newrow[1:3])
      row.names(testdata) <- c(1:length(testdata$score))
      testdata <- testdata[order(testdata$score),]
    }
    filename <- paste(testname,"~",refname,"_",gender,"_",agegrp,".csv", sep="")
    write.csv(testdata,paste(pathout,filename, sep=""), row.names=FALSE)
    testdata <- list(testdata=testdata)
  }
  
  ## Read in data from testdata and prevscores from csv file
  
  filename <- paste(testname,"~",refname,"_",gender,"_",agegrp,".csv", sep="") 
  if(file.exists(paste(pathout,filename, sep="")))testdata <- read.csv(paste(pathout,filename, sep=""), header=TRUE)
  if(!file.exists(paste(pathout,filename, sep=""))) {
    rwdata_out <- rwdata()
    testdata <- rwdata_out$testdata
  }
  
  #trial <- paste("UK_trial_a") # for analysis of trial data
  #testname <- paste(testname,"score",sep=" ")
  
  path1 <- paste(path,prevdata,".csv",sep="")
  prevscores <- read.csv(file=path1, header=TRUE)
  getdata_out <- list(testdata=testdata,prevscores=prevscores)
}
### end function

##########################################################################################
### Calculate Descriptives, corrs, ICCs and ORs (Binom Regression) 
firststats <- function() {
  df <- read.csv("ARUK_dataset2_full_responders_cleaned_WITH_RECODES_DEIDENTIFIED_with_DEPR.csv", header=TRUE, stringsAsFactors=FALSE)
  #table(df$TLFB_binge,df$TLFB_hazardous)
  #table(df$TLFB_binge[df$Gender == 2])
  #colnames(df)
    
  library(gmodels)
  dftab <- list()
  for(i in seq_along(refnames))  {
    df1 <- subset(df, select = c(get(refnames[i]),Gender))
    tab1 <- CrossTable(df1[,1], df1[,2])
    dftab1 <- c(tab1$prop.col[2,1],tab1$t[2,1],tab1$prop.col[2,2],tab1$t[2,2])
    dftab <- cbind(dftab,dftab1)
  }
  
  dftab <- data.frame(matrix(unlist(dftab), nrow=length(refnames), byrow=T))
  
  dftab[,1] <- format(100*as.numeric(dftab[,1]),digits=2,nsmall=1)
  dftab[,3] <- format(100*as.numeric(dftab[,3]),digits=2,nsmall=1)
  dftab <- rbind(c("%","N","%","N"),dftab)
  dftab <- cbind(c("",reflabels),dftab)
  colnames(dftab) <- c(" ","Males, N=138"," ","Females, N=282"," ")
  
  library(psych)
  df2tab <- list()
  #colnames(df)
  df2 <- describeBy(df$depr.IMD_200710nov, df$Gender)
  df2tab1 <- c(df2[[1]][3],df2[[1]][13],df2[[2]][3],df2[[2]][13])
  df2tab <- rbind(df2tab,df2tab1)
  df2 <- describeBy(df$AUDITscore, df$Gender)
  df2tab1 <- c(df2[[1]][3],df2[[1]][13],df2[[2]][3],df2[[2]][13])
  df2tab <- rbind(df2tab,df2tab1)
  df2 <- describeBy(df$AUDITC, df$Gender)
  df2tab1 <- c(df2[[1]][3],df2[[1]][13],df2[[2]][3],df2[[2]][13])
  df2tab <- rbind(df2tab,df2tab1)
  
  
  df2tab <- t(df2tab)
  df2tab <- data.frame(matrix(unlist(df2tab), nrow=3, byrow=T))
  df2tab <- format(df2tab,digits=2,nsmall=2)
  df2tab <- rbind(c("mean","s.e.","mean","s.e."),df2tab)
  df2tab <- cbind(c("","IMD score","AUDIT score", "AUDITC score"),df2tab)
  colnames(df2tab) <- c(" ","Males, N=138"," ","Females, N=282"," ")
  dfcomb <- rbind(dftab,df2tab)
  
  
  #Quintiles for IMD
  
  #England LLSOA 2010 quintile cut points
  #low - 8.492 - 13.79 - -21.35 - 34.17 - 87.8 (high score is more deprived)
  df$ind_IMD_Qint <- c()
  
  df$ind_IMD_Qint[df$depr.IMD_200710nov<8.492] <- 1
  df$ind_IMD_Qint[df$depr.IMD_200710nov>=8.492 & df$depr.IMD_200710nov<13.79] <- 2 
  df$ind_IMD_Qint[df$depr.IMD_200710nov>=13.79 & df$depr.IMD_200710nov<21.35] <- 3
  df$ind_IMD_Qint[df$depr.IMD_200710nov>=21.35 & df$depr.IMD_200710nov<34.17] <- 4
  df$ind_IMD_Qint[df$depr.IMD_200710nov>=34.17] <- 5
  
  IMDqint <- as.data.frame(table(df$ind_IMD_Qint))
  IMDqint <- cbind(IMDqint,100*IMDqint[,2]/sum(IMDqint[,2]))
  IMDqint <- format(IMDqint,digits=3,nsmall=1)
  colnames(IMDqint) <- c("Quintile, high=higher deprivation","N","%")
  
  tabIMDa <- describeBy(df$depr.IMD_200710nov, df$Practice)
  tabIMDb <- list()
  for(i in 1:14) {
    tabIMDb[[i]] <- c(tabIMDa[[i]][5],tabIMDa[[i]][8],tabIMDa[[i]][9])
  }
  
  tab2IMD <- c()
  tab2IMDc <- c()
  for(i in 1:14) {
    tab2IMDc <- c(unlist(tabIMDb[i]))
    tab2IMD <- rbind(tab2IMD,tab2IMDc)
  }
  
  rownames(tab2IMD)<-c(1:14)
  tab2IMD<-data.frame(tab2IMD)
  tab2IMD<-cbind(c(1:14),tab2IMD)
  colnames(tab2IMD)<-c("Practice","median","min","max")

  library(ggplot2)
  filename <- paste0("Outfiles/IMD_Practice.pdf")
  pdf(filename)
  p <- ggplot(data=tab2IMD, aes(x = factor(Practice), y = median)) 
  p <- p + geom_point(stat = "identity")
  p <- p + geom_crossbar(aes(ymax = max, ymin=min),fill="grey") 
  p <- p + geom_hline(yintercept=c(8.492,13.79,21.35,34.17,87.8))
  p <- p + theme_bw()
  p <- p + annotate("text", x=16, y=42, label="High   \nQuintile", size=3, hjust=1.05)
  p <- p + annotate("text", x=16, y=5, label="Low   \nQuintile", size=3, hjust=1.05)
  p <- p + xlab("Practice") + ylab("IMD score\n")
  print(p)  
  IMDplot <- p
  dev.off()
  
  ## Calculate ICCs and Binom Regression
  library(ICC)

  ## ICC
  icc.fun <- function(x) {
    df.icc <- data.frame(c(with(df, get(refnames[x]))),df$Practice)
    df.icc[,2] <- as.factor(df.icc[,2])
    colnames(df.icc) <- c("reference", "Practice")  
    ICCest(Practice,reference,df.icc)[1:3]  ###NB check order of variables
  }
  ICC_out <- lapply(seq_along(refnames), icc.fun)
  
  #CrossTable(df$Practice,df$dsm4_dependence)  #check on why ICC for this var is so much higher...need to think about this
  
  dfICC <- data.frame(matrix(unlist(ICC_out), nrow=length(refnames), byrow=T))
  dfICC <- cbind(reflabels,dfICC)
  colnames(dfICC) <- c("Reference Tests", "ICC", "Lower CI", "Upper CI")
  dfICC <- format(dfICC[,2:4],digits=0,nsmall=2)
  
  dfICCcomb <- paste0(dfICC[,1]," (95% CI ",dfICC[,2]," to ",dfICC[,3],")")


  ## Correlations
  library(psychometric)
  yvars <- c("sum90days", "TLFBHeavDrinkDay")
  corrplot <- list()
  for(i in seq_along(yvars)) {
    rcor.fun <- function(x) {
      rcor <- with(df,cor(get(refnames[[x]]),get(yvars[i])))
      CIrcor <- CIr(rcor, n = length(df[,1]), level = .95)
      rescor <- c(rcor,CIrcor)
    }
    
    rescor <- data.frame(t(sapply(seq_along(refnames), rcor.fun)))
    rescor <- cbind(refnames,rescor)
    colnames(rescor) <- c("refnames", "r", "LCI", "UCI")
    rescor[,2:4] <- as.numeric(unlist(format(rescor[,2:4], digits=1, nsmall=2)))
    
    filename <- paste0("Outfiles/Pearson_r_with_",yvars[i],".pdf")
    p <- ggplot(data = rescor, aes(x = refnames, y = r, 
                            ymin = LCI, ymax = UCI))
    p <- p + geom_point(aes(), size=3)
    p <- p + geom_linerange() + coord_flip()
    p <- p + ylab(paste0("\nPearson r and 95% CI, yvar = ",yvars[i]))
    p <- p + xlab("xvars")
    ggsave(filename)
    corrplot[[i]] <- p
    }
    
  
  ## Binom Regression:
  
  df$CurrentSmoker[df$CurrentSmoker == "-1"] <- NA
  df1 <- data.frame(lapply(seq_along(refnames), function(x) with(df,get(refnames[[x]]))))
  colnames(df1) <- refnames
  df1 <- cbind(df1, df$Practice,
                    df$Gender,
                    df$Age_GLQ,
                    df$depr.IMD_200710nov,
                    df$CurrentSmoker,
                    df$AUDITscore,
                    df$AUDITC,
                    df$sum90days,
                    df$TLFBHeavDrinkDay
               )
  
  colnames(df1)[(length(refnames)+1):(length(colnames(df1)))] <- substring(colnames(df1)[(length(refnames)+1):(length(colnames(df1)))],4)    
  df1 <- na.omit(df1)
  df1$Practice <- as.factor(df1$Practice)
  df1$Age_GLQ <- as.factor(df1$Age_GLQ)
  df1$CurrentSmoker <- as.factor(df1$CurrentSmoker)
  df1$sum90days_pos <- df1$sum90days + 1 # because some family arguments require positive (i.e. non-zero) variables
  df1$TLFBHeavDrinkDay <- df1$TLFBHeavDrinkDay + 1 # because some family argiments require positive (i.e. non-zero) variables
  df1 <<- df1
  
  fit0<-list()
  fit1a<-list()
  fit1<-list()
  fitdiff<-list()
  fit0_ORs<-list()
  fit2_ORs<-list()
  ICC.fit<-list()  
  
  library(lme4)
  library(arm)

  fit.fun <- function(x) {
    fitform <- as.formula(paste0(refnames[x]," ~ (1 | Practice)"))
    fit1[[x]] <-glmer(fitform, family = binomial('logit'), data=df1)
    fitforma <- as.formula(paste0(refnames[x]," ~ 1"))
    fit1a[[x]] <- glm(fitforma, data = df1, family = binomial("logit")) 
    fitdiff[[x]] <- logLik(fit1a[[x]])-logLik(fit1[[x]])
    ICC.fit <- as.numeric(VarCorr(fit1[[x]])$Practice)/(as.numeric(VarCorr(fit1[[x]])$Practice)+(15/16)^2*pi^2/3)
    
    ## with caterpillar plot:
    u0 <- ranef(fit1[[x]], condVar = TRUE)  
    u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ])  
    commid <- as.numeric(rownames(u0[[1]])) 
    u0tab <- cbind("commid" = commid, "u0" = u0[[1]], "u0se" = u0se) 
    colnames(u0tab)[2] <- "u0" 
    u0tab <- u0tab[order(u0tab$u0), ] 
    u0tab <- cbind(u0tab, c(1:dim(u0tab)[1])) 
    u0tab <- u0tab[order(u0tab$commid), ] 
    colnames(u0tab)[4] <- "u0rank" 
    
    filename<-paste0("Outfiles/Regresserrors_Caterpillar_Random_",refnames[x],".pdf")
    pdf(filename)
    plot(u0tab$u0rank, u0tab$u0, type = "n", xlab = "u_rank", ylab = "conditional 
         modes of r.e. for comm_id:_cons", ylim = c(min(u0tab$u0 - 4*u0tab$u0se), max(u0tab$u0 + 4*u0tab$u0se)))
    segments(u0tab$u0rank, u0tab$u0 - 1.96*u0tab$u0se, u0tab$u0rank, u0tab$u0 + 
               1.96*u0tab$u0se) 
    points(u0tab$u0rank, u0tab$u0, col = "blue") 
    abline(h = 0, col = "red")
    dev.off()
    
    ## with deprivation, gender, Age Group and smoking as predictors
    
    ## standard model (GLM):  
    fit0form <- as.formula(paste0(refnames[x]," ~ depr.IMD_200710nov + Gender + Age_GLQ + CurrentSmoker"))
    fit0 <- glm(fit0form, data = df1, family = binomial('logit')) 
    
    #summary(fit0)
    filename<-paste0("Outfiles/Summary_Fit_noHLM_GLM_",refnames[x],".txt")
    capture.output(summary(fit0),file=filename)
    
    fit0_ORs <- format(exp(cbind(OR = coef(fit0), confint(fit0))),digits=1,nsmall=2)
    
    ## standard model errors:
    filename<-paste0("Outfiles/Regresserrors_BinnedPlot_StandardGLM_",refnames[x],".pdf")
    pdf(filename)
    binnedplot(fitted(fit0),resid(fit0)) #http://stats.stackexchange.com/questions/63566/unexpected-residuals-plot-of-mixed-linear-model-using-lmer-lme4-package-in-r
    dev.off()
    
    
    ## General Linear Mixed Model (GLMM)
    ## unscaled: 
    fit2form<-as.formula(paste0(refnames[x]," ~ (1 | Practice) + depr.IMD_200710nov + Gender + Age_GLQ + CurrentSmoker"))
    fit2 <- glmer(fit2form, data = df1, family = binomial('logit'))
    fit2s <- standardize(fit2)
    #summary(fit2)
    filename<-paste0("Outfiles/Summary_Fit_GLMM_",refnames[x],".txt")
    capture.output(display(fit2),file=filename)
    filename<-paste0("Outfiles/Summary_Fit_Summary_Fit_GLMM_standardized_",refnames[x],".txt")
    capture.output(display(fit2s),file=filename)    
    
    #fixef(fit2)
    #ranef(fit2)
    
    ## GLMM errors:
    filename<-paste0("Outfiles/Regresserrors_GLMM_BinnedPlot_",refnames[x],".pdf")
    pdf(filename)
    binnedplot(fitted(fit2),resid(fit2)) #http://stats.stackexchange.com/questions/63566/unexpected-residuals-plot-of-mixed-linear-model-using-lmer-lme4-package-in-r
    dev.off()
    
    
    ## iterate through Practice levels, selectively remove and check their influence using Cook's distance
    library(influence.ME)
    alt.est <- influence(fit2, "Practice")
    filename<-paste0("Outfiles/Regresserrors_GLMM_CookDistance_Random_",refnames[x],".pdf")
    pdf(filename)
    print(plot(alt.est, which="cook", sort=TRUE))
    dev.off()
    
    ## curves of fixed effects:
    #install.packages("coefplot2",
    #                 repos="http://www.math.mcmaster.ca/bolker/R",
    #                 type="source")
    library(reshape)
    library(coefplot2)
    filename<-paste0("Outfiles/Regress_CoeffPlot_GLMM_Fixed_",refnames[x],".pdf")
    pdf(filename)
    coefplot2(fit2s)
    dev.off()
    
    fitconf <- confint(fit2s,method="boot")   #boot",nsim=100)    #boot",.progress="txt",PBargs=list(style=3))  # takes long time ? parallelize
    ## remove extra parameter row if method = boot
    ifelse(length(fitconf[,1]) == 8,fitconf <- fitconf[-1,],fitconf <- fitconf)  
    
    fit2_est <- fixef(fit2s)
    
    IMD_est <- exp(fit2_est[2])
    IMD_LCI <- exp(fitconf[2,1])
    IMD_UCI <- exp(fitconf[2,2])
    
    Sex_est <- exp(fit2_est[3])
    Sex_LCI <- exp(fitconf[3,1])
    Sex_UCI <- exp(fitconf[3,2])
    
    Age_GLQ2_est <- exp(fit2_est[4])
    Age_GLQ2_LCI <- exp(fitconf[4,1])
    Age_GLQ2_UCI <- exp(fitconf[4,2])
    
    Age_GLQ3_est <- exp(fit2_est[5])
    Age_GLQ3_LCI <- exp(fitconf[5,1])
    Age_GLQ3_UCI <- exp(fitconf[5,2])
    
    CurrentSmoker2_est <- exp(fit2_est[6])
    CurrentSmoker2_LCI <- exp(fitconf[6,1])
    CurrentSmoker2_UCI <- exp(fitconf[6,2])
    
    CurrentSmoker3_est <- exp(fit2_est[7])
    CurrentSmoker3_LCI <- exp(fitconf[7,1])
    CurrentSmoker3_UCI <- exp(fitconf[7,2])
    
    fit2_ORs <- format(rbind(cbind(IMD_est,IMD_LCI,IMD_UCI),
                                  cbind(Sex_est,Sex_LCI,Sex_UCI),
                                  cbind(Age_GLQ2_est,Age_GLQ2_LCI,Age_GLQ2_UCI),
                                  cbind(Age_GLQ3_est,Age_GLQ3_LCI,Age_GLQ3_UCI),
                                  cbind(CurrentSmoker2_est,CurrentSmoker2_LCI,CurrentSmoker2_UCI),
                                  cbind(CurrentSmoker3_est,CurrentSmoker3_LCI,CurrentSmoker3_UCI)),
                            digits=1,nsmall=2)
    fit.fun <- list(fit0_ORs,fit2_ORs,ICC.fit)
  }

  set.seed(42)
  library(parallel)
  ncores <- detectCores()
  system.time(
    fit.fun <- mclapply(seq_along(refnames),fit.fun, mc.cores = ncores)
  )
  
  
  ## model for continuous var sum90days:
  ## with deprivation, gender, Age Group and smoking as predictors
  
  ## Dec 19 2013: after trying many different distibutions and link functions, the Gamma and log link provide the 
  ## best model in terms of normally distributed residuals (see plot). Gamma also makes sense for the dep variable.
  
  fit3form<-as.formula(paste0("sum90days_pos ~ (1 | Practice) + depr.IMD_200710nov + Gender + Age_GLQ + CurrentSmoker + AUDITC"))
  fit3 <- glmer(fit3form, data = df1, family=Gamma(link="log"))
  fit3s <- standardize(fit3)

  #summary(fit3)
  filename<-paste0("Outfiles/Summary_Fit_GLMM_sum90days.txt")
    capture.output(display(fit3),file=filename)
  filename<-paste0("Outfiles/Summary_Fit_GLMM_standardized_sum90days.txt")
    capture.output(display(fit3s),file=filename)

  #fixef(fit3)
  #ranef(fit3)

  ICC.fit3 <- format(as.numeric(VarCorr(fit3)$Practice)/(as.numeric(VarCorr(fit3)$Practice)+(15/16)^2*pi^2/3),digits=0,nsmall=2)

  ## errors:
  filename<-paste0("Outfiles/Regresserrors_GLMM_ResidPlot_sum90days.pdf")
  pdf(filename)
    plot(resid(fit3))
  dev.off()
  
  filename<-paste0("Outfiles/Regresserrors_GLMM_QQPlot_sum90days.pdf")
  pdf(filename)
    qqnorm(resid(fit3))
    qqline(resid(fit3)) 
  dev.off()
  
  library(MASS)
  par(mfrow=c(2,3))
  res<-residuals(fit3)
  filename<-paste0("Outfiles/Regresserrors_GLMM_Resid_Histogram_sum90days.pdf")
  pdf(filename)  
    truehist(res,main="Histogram of Residuals") 
    curve(dnorm(x,mean=mean(res),sd=sd(res)),add=TRUE) 
  dev.off()
  #qqnorm(fitted(fit3),resid(fit3), ylim=range(fitted(fit3)), main="QQNorm Plot") 
  #plot(residuals(fit3)~fitted(fit3),main="Resid vs.Fits",xlab="Fits",ylab="Resids");abline(h=0) 
  #plot(residuals(fit3),type="l",main="Resid vs Order",ylab="Resids") 
  #acf(residuals(fit3), main="Resid Autocorrelation") 
  #with(df1, scatter.smooth(sum90days,depr.IMD_200710nov))
  filename<-paste0("Outfiles/sum90days~Gender_Boxplot_sum90days.pdf")
  pdf(filename)  
    boxplot(sum90days~Gender, data=df1,main="Box plot of Gender", ylab="90 day total", xlab="Gender")
  dev.off()
  filename<-paste0("Outfiles/sum90days~AgeGrp_Boxplot_sum90days.pdf")
  pdf(filename)  
    boxplot(sum90days~Age_GLQ, data=df1,main="Box plot of Age Group", ylab="90 day total", xlab="Age")
  dev.off()
  filename<-paste0("Outfiles/sum90days~Smoker_Boxplot_sum90days.pdf")
  pdf(filename)  
    boxplot(sum90days~CurrentSmoker, data=df1,main="Box plot of Smoker Status", ylab="90 day total", xlab="Smoker Status")
  dev.off()
  filename<-paste0("Outfiles/sum90days~AUDITC_Scatterplot_sum90days.pdf")
  pdf(filename)  
    with(df1, scatter.smooth(AUDITC,TLFBHeavDrinkDay))
  dev.off()
  filename<-paste0("Outfiles/sum90days~AUDITscore_Scatterplot_sum90days.pdf")
  pdf(filename)  
    with(df1, scatter.smooth(AUDITscore,TLFBHeavDrinkDay))
  dev.off()


  ## with caterpillar plot:
  u0 <- ranef(fit3, condVar = TRUE)  
  u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ])  
  commid <- as.numeric(rownames(u0[[1]])) 
  u0tab <- cbind("commid" = commid, "u0" = u0[[1]], "u0se" = u0se) 
  colnames(u0tab)[2] <- "u0" 
  u0tab <- u0tab[order(u0tab$u0), ] 
  u0tab <- cbind(u0tab, c(1:dim(u0tab)[1])) 
  u0tab <- u0tab[order(u0tab$commid), ] 
  colnames(u0tab)[4] <- "u0rank" 
  
  
  filename<-paste0("Outfiles/Regresserrors_GLMM_Caterpillar_Random_sum90days.pdf")
  pdf(filename)
    plot(u0tab$u0rank, u0tab$u0, type = "n", xlab = "u_rank", ylab = "conditional 
       modes of r.e. for comm_id:_cons" , ylim = c(min(u0tab$u0 - 4*u0tab$u0se), max(u0tab$u0 + 4*u0tab$u0se)))
    segments(u0tab$u0rank, u0tab$u0 - 1.96*u0tab$u0se, u0tab$u0rank, u0tab$u0 + 
             1.96*u0tab$u0se) 
    points(u0tab$u0rank, u0tab$u0, col = "blue") 
    abline(h = 0, col = "red")
  dev.off()
  
  
  ## iterate through Practice levels, selectively remove and check their influence using Cook's distance
  library(influence.ME)
  alt.est <- influence(fit3, "Practice")
  filename<-paste0("Outfiles/Regresserrors_GLMM_CookDistance_Random_sum90days.png")
  png(filename)
    printplot <- plot(alt.est, which="cook", sort=TRUE)
    print(printplot)
  dev.off()
  
  ## curves of fixed effects:
  #install.packages("coefplot2",
  #                 repos="http://www.math.mcmaster.ca/bolker/R",
  #                 type="source")
  library(reshape)
  library(coefplot2)
  filename<-paste0("Outfiles/Regress_GLMM_CoeffPlot_Fixed_sum90days.pdf")
  pdf(filename)
    coefplot2(fit3s)
  dev.off()
  
  
  fit3confint <- confint(fit3s,method="Wald") # boot method not implemented for Gamma, so using Wald method
  fit3_betas <- format(cbind(beta = fixef(fit3s), fit3confint),digits=0,nsmall=2)

  beta_IMD <-paste(fit3_betas[2,1],"(95% CI ",fit3_betas[2,2]," to ",fit3_betas[2,3],")")
  beta_gender<- paste(fit3_betas[3,1],"(95% CI ",fit3_betas[3,2]," to ",fit3_betas[3,3],")")
  beta_Agegrp2 <-paste(fit3_betas[4,1],"(95% CI ",fit3_betas[4,2]," to ",fit3_betas[4,3],")")
  beta_Agegrp3 <-paste(fit3_betas[5,1],"(95% CI ",fit3_betas[5,2]," to ",fit3_betas[5,3],")")
  beta_CurrentSmoker2 <-paste(fit3_betas[6,1],"(95% CI ",fit3_betas[6,2]," to ",fit3_betas[6,3],")")
  beta_CurrentSmoker3 <-paste(fit3_betas[7,1],"(95% CI ",fit3_betas[7,2]," to ",fit3_betas[7,3],")")    
  beta_AUDITC <-paste(fit3_betas[8,1],"(95% CI ",fit3_betas[8,2]," to ",fit3_betas[8,3],")")

  df.betas <- cbind(ICC.fit3,beta_IMD,beta_gender,beta_Agegrp2,beta_Agegrp3,
                    beta_CurrentSmoker2,beta_CurrentSmoker3,beta_AUDITC)
  df.betas <- as.data.frame(df.betas)
  firstcol <- c("Total Consumption (Units) over Last 90")
  df.betas <- cbind(firstcol, df.betas)  
  colnames(df.betas) <- c("", "ICC", "IMD, beta", 
                          "Gender, beta", 
                          "Agegrp2, beta", 
                          "Agegrp3, beta", 
                          "CurrentSmoker2, beta", 
                          "Currentsmoker3, beta",
                          "AUDIT-C score, beta")
  

  ## model for count var TLFBHeavDrinkDay:
  ## with deprivation, gender, Age Group and smoking as predictors
  ## transform dep var for use with lmer (not needed for glmer)

  fit4form<-as.formula(paste0("TLFBHeavDrinkDay ~ (1 | Practice) + depr.IMD_200710nov + Gender + Age_GLQ + CurrentSmoker + AUDITC"))
  fit4 <- glmer(fit4form, data = df1, family=Gamma(link="log"))
  fit4s <- standardize(fit4)

  filename<-paste0("Outfiles/Summary_Fit_GLMM_TLFBHeavDrinkDay.txt")
    capture.output(display(fit4),file=filename)
  filename<-paste0("Outfiles/Summary_Fit_GLMM_standardized_TLFBHeavDrinkDay.txt")
    capture.output(display(fit4s),file=filename)


  #fixef(fit4)
  #ranef(fit4)
  
  ICC.fit4 <- format(as.numeric(VarCorr(fit4)$Practice)/(as.numeric(VarCorr(fit4)$Practice)+(15/16)^2*pi^2/3),digits=0,nsmall=2)
  
  ## errors:
  filename<-paste0("Outfiles/Regresserrors_GLMM_ResidPlot_TLFBHeavDrinkDay.pdf")
  pdf(filename)
    plot(resid(fit4))
  dev.off()
  
  filename<-paste0("Outfiles/Regresserrors_GLMM_QQPlot_TLFBHeavDrinkDay.pdf")
  pdf(filename)
    qqnorm(resid(fit4))
    qqline(resid(fit4)) 
  dev.off()
  
  library(MASS)
  par(mfrow=c(2,3))
  res<-residuals(fit4)
  filename<-paste0("Outfiles/Regresserrors_GLMM_Resid_Histogram_TLFBHeavDrinkDay.pdf")
  pdf(filename)  
    truehist(res,main="Histogram of Residuals") 
    curve(dnorm(x,mean=mean(res),sd=sd(res)),add=TRUE) 
  dev.off()
  #qqnorm(fitted(fit3),resid(fit3), ylim=range(fitted(fit3)), main="QQNorm Plot") 
  #plot(residuals(fit3)~fitted(fit3),main="Resid vs.Fits",xlab="Fits",ylab="Resids");abline(h=0) 
  #plot(residuals(fit3),type="l",main="Resid vs Order",ylab="Resids") 
  #acf(residuals(fit3), main="Resid Autocorrelation") 
  #with(df1, scatter.smooth(sum90days,depr.IMD_200710nov))
  filename<-paste0("Outfiles/TLFBHeavDrinkDay~Gender_Boxplot_TLFBHeavDrinkDay.pdf")
  pdf(filename)  
    boxplot(TLFBHeavDrinkDay~Gender, data=df1,main="Box plot of Gender", ylab="90 day total", xlab="Gender")
  dev.off()
  filename<-paste0("Outfiles/TLFBHeavDrinkDay~AgeGrp_Boxplot_TLFBHeavDrinkDay.pdf")
  pdf(filename)  
    boxplot(TLFBHeavDrinkDay~Age_GLQ, data=df1,main="Box plot of Age Group", ylab="90 day total", xlab="Age")
  dev.off()
  filename<-paste0("Outfiles/TLFBHeavDrinkDay~Smoker_Boxplot_TLFBHeavDrinkDay.pdf")
  pdf(filename)  
    boxplot(TLFBHeavDrinkDay~CurrentSmoker, data=df1,main="Box plot of Smoker Status", ylab="90 day total", xlab="Smoker Status")
  dev.off()
  filename<-paste0("Outfiles/TLFBHeavDrinkDay~AUDITC_Scatterplot_TLFBHeavDrinkDay.pdf")
  pdf(filename)  
    with(df1, scatter.smooth(AUDITC,TLFBHeavDrinkDay))
  dev.off()
  filename<-paste0("Outfiles/TLFBHeavDrinkDay~AUDITscore_Scatterplot_TLFBHeavDrinkDay.pdf")
  pdf(filename)  
    with(df1, scatter.smooth(AUDITscore,TLFBHeavDrinkDay))
  dev.off()


  ## with caterpillar plot:
  u0 <- ranef(fit4, condVar = TRUE)  
  u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ])  
  commid <- as.numeric(rownames(u0[[1]])) 
  u0tab <- cbind("commid" = commid, "u0" = u0[[1]], "u0se" = u0se) 
  colnames(u0tab)[2] <- "u0" 
  u0tab <- u0tab[order(u0tab$u0), ] 
  u0tab <- cbind(u0tab, c(1:dim(u0tab)[1])) 
  u0tab <- u0tab[order(u0tab$commid), ] 
  colnames(u0tab)[4] <- "u0rank" 
  
  
  filename<-paste0("Outfiles/Regresserrors_Caterpillar_Random_TLFBHeavDrinkDay.pdf")
  pdf(filename)
  plot(u0tab$u0rank, u0tab$u0, type = "n", xlab = "u_rank", ylab = "conditional 
       modes of r.e. for comm_id:_cons" , ylim = c(min(u0tab$u0 - 4*u0tab$u0se), max(u0tab$u0 + 4*u0tab$u0se)))
  segments(u0tab$u0rank, u0tab$u0 - 1.96*u0tab$u0se, u0tab$u0rank, u0tab$u0 + 
             1.96*u0tab$u0se) 
  points(u0tab$u0rank, u0tab$u0, col = "blue") 
  abline(h = 0, col = "red")
  dev.off()
  
  
  ## iterate through Practice levels, selectively remove and check their influence using Cook's distance
  library(influence.ME)
  alt.est <- influence(fit4, "Practice")
  filename<-paste0("Outfiles/Regresserrors_CookDistance_Random_TLFBHeavDrinkDay.pdf")
  pdf(filename)
    print(plot(alt.est, which="cook", sort=TRUE))
  dev.off()
  
  ## curves of fixed effects:
  #install.packages("coefplot2",
  #                 repos="http://www.math.mcmaster.ca/bolker/R",
  #                 type="source")
  library(reshape)
  library(coefplot2)
  filename<-paste0("Outfiles/Regress_CoeffPlot_Fixed_TLFBHeavDrinkDay.pdf")
  pdf(filename)
    coefplot2(fit4s)
  dev.off()
  
  
  fit4confint <- confint(fit4s,method="Wald") # boot method not implemented for Gamma, so using Wald method
  fit4_betas <- format(cbind(beta = fixef(fit4s), fit4confint),digits=0,nsmall=2)
  
  beta4_IMD <-paste(fit4_betas[2,1],"(95% CI ",fit4_betas[2,2]," to ",fit4_betas[2,3],")")
  beta4_gender<- paste(fit4_betas[3,1],"(95% CI ",fit4_betas[3,2]," to ",fit4_betas[3,3],")")
  beta4_Agegrp2 <-paste(fit4_betas[4,1],"(95% CI ",fit4_betas[4,2]," to ",fit4_betas[4,3],")")
  beta4_Agegrp3 <-paste(fit4_betas[5,1],"(95% CI ",fit4_betas[5,2]," to ",fit4_betas[5,3],")")
  beta4_CurrentSmoker2 <-paste(fit4_betas[6,1],"(95% CI ",fit4_betas[6,2]," to ",fit4_betas[6,3],")")
  beta4_CurrentSmoker3 <-paste(fit4_betas[7,1],"(95% CI ",fit4_betas[7,2]," to ",fit4_betas[7,3],")")    
  beta4_AUDITC <-paste(fit4_betas[8,1],"(95% CI ",fit4_betas[8,2]," to ",fit4_betas[8,3],")")


  dfbetas4 <- cbind(ICC.fit4,beta4_IMD,beta4_gender,beta4_Agegrp2,beta4_Agegrp3,
                   beta4_CurrentSmoker2,beta4_CurrentSmoker3,beta4_AUDITC)
  dfbetas4 <- as.data.frame(dfbetas4)
  firstcol <- c("Number of Heavy Drinking Days in Last 90")
  dfbetas4 <- cbind(firstcol, dfbetas4)  
  colnames(dfbetas4) <- c("", "ICC", "IMD, beta", 
                         "Gender, beta", 
                         "Agegrp2, beta", 
                         "Agegrp3, beta", 
                         "CurrentSmoker2, beta", 
                         "Currentsmoker3, beta",
                         "AUDIT-C score, beta")


  df.betas <- rbind(df.betas,dfbetas4)

  unlist(fit.fun[[i]][[1]][2,1]) # fit0_ORs
  unlist(fit.fun[[1]][[2]]) # fit2_ORs
  unlist(fit.fun[[1]][[3]]) # ICC.fit
  
  
  ## create values for outputs  
  dfORs <- list()
  for(i in seq_along(refnames)) {
    OR_IMD <- paste(unlist(fit.fun[[i]][[1]][2,1]),"(95% CI ",unlist(fit.fun[[i]][[1]][2,2])," to ",unlist(fit.fun[[i]][[1]][2,3]),")")
    OR_gender <- paste(unlist(fit.fun[[i]][[1]][3,1]),"(95% CI ",unlist(fit.fun[[i]][[1]][3,2])," to ",unlist(fit.fun[[i]][[1]][3,3]),")")
    OR_Agegrp2 <- paste(unlist(fit.fun[[i]][[1]][4,1]),"(95% CI ",unlist(fit.fun[[i]][[1]][4,2])," to ",unlist(fit.fun[[i]][[1]][4,3]),")")
    OR_Agegrp3 <- paste(unlist(fit.fun[[i]][[1]][5,1]),"(95% CI ",unlist(fit.fun[[i]][[1]][5,2])," to ",unlist(fit.fun[[i]][[1]][5,3]),")")
    OR_CurrentSmoker2 <- paste(unlist(fit.fun[[i]][[1]][6,1]),"(95% CI ",unlist(fit.fun[[i]][[1]][6,2])," to ",unlist(fit.fun[[i]][[1]][6,3]),")")
    OR_CurrentSmoker3 <- paste(unlist(fit.fun[[i]][[1]][7,1]),"(95% CI ",unlist(fit.fun[[i]][[1]][7,2])," to ",unlist(fit.fun[[i]][[1]][7,3]),")")
    
    dfORs <- cbind(dfORs,OR_IMD,OR_gender,OR_Agegrp2,OR_Agegrp3,OR_CurrentSmoker2,OR_CurrentSmoker3)
  }
  dfORs <- data.frame(matrix(unlist(dfORs), nrow=length(refnames), byrow=T))
  dfORs <- cbind(c(dfICCcomb),dfORs)
  dfORs <- cbind(c(reflabels),dfORs)
  colnames(dfORs) <- c(" ","ICC by Practice", "Odds Ratio, IMD", 
                          "Odds Ratio, Gender",
                          "Odds Ratio, Age Group2",
                          "Odds Ratio, Age Group3",
                          "Odds Ratio, CurrentSmoker2",
                          "Odds Ratio, CurrentSmoker3"
                     )

  df2_ORs<-list()
  for(i in seq_along(refnames)) {
    ICC.est <- format(unlist(fit.fun[[i]][[3]]),digits=1,nsmall=2)
    OR_IMD <-paste(unlist(fit.fun[[i]][[2]])[1,1],"(95% CI ",unlist(fit.fun[[i]][[2]])[1,2]," to ",unlist(fit.fun[[i]][[2]])[1,3],")")
    OR_gender<- paste(unlist(fit.fun[[i]][[2]])[2,1],"(95% CI ",unlist(fit.fun[[i]][[2]])[2,2]," to ",unlist(fit.fun[[i]][[2]])[2,3],")")
    OR_Agegrp2 <-paste(unlist(fit.fun[[i]][[2]])[3,1],"(95% CI ",unlist(fit.fun[[i]][[2]])[3,2]," to ",unlist(fit.fun[[i]][[2]])[3,3],")")
    OR_Agegrp3 <-paste(unlist(fit.fun[[i]][[2]])[4,1],"(95% CI ",unlist(fit.fun[[i]][[2]])[4,2]," to ",unlist(fit.fun[[i]][[2]])[4,3],")")
    OR_CurrentSmoker2 <-paste(unlist(fit.fun[[i]][[2]])[5,1],"(95% CI ",unlist(fit.fun[[i]][[2]])[5,2]," to ",unlist(fit.fun[[i]][[2]])[5,3],")")
    OR_CurrentSmoker3 <-paste(unlist(fit.fun[[i]][[2]])[6,1],"(95% CI ",unlist(fit.fun[[i]][[2]])[6,2]," to ",unlist(fit.fun[[i]][[2]])[6,3],")")
    
    df2_ORs<-cbind(df2_ORs,ICC.est,OR_IMD,OR_gender,OR_Agegrp2,OR_Agegrp3,OR_CurrentSmoker2,OR_CurrentSmoker3)
  }
  df2_ORs <- data.frame(matrix(unlist(df2_ORs), nrow=length(refnames), byrow=T))
  #df1_ORs <- cbind(c(ICC.fit),df1_ORs)
  df2_ORs <- cbind(c(reflabels),df2_ORs)
  colnames(df2_ORs) <- c(" ","ICC by Practice", "Odds Ratio, IMD", 
                         "Odds Ratio, Gender",
                         "Odds Ratio, Age Group2",
                         "Odds Ratio, Age Group3",
                         "Odds Ratio, CurrentSmoker2",
                         "Odds Ratio, CurrentSmoker3"
                      )

  firststats_out<-list(corrplot=corrplot,dfcomb=dfcomb,df.betas=df.betas,dfORs=dfORs,df2_ORs=df2_ORs,IMDqint=IMDqint,tab2IMD=tab2IMD,IMDplot=IMDplot)
}
### end function

##########################################################################
### PREPARE PRE-TEST PROBABILITY VARIABLES
pretest <- function() {
 if(prevest=="Data") {
   prev <- diagtable_out$tprev
   seprev <- diagtable_out$tseprev
 }
 else {
  prevscores <- getdata_out$prevscores
  prev <- prevscores$prev[prevscores$dep=="Yes" & prevscores$gender==gender & prevscores$agegrp==agegrp]   # might need to vary this if available  pre-test data is presented differently
  recipprev <- 1-prev
  nprev <- sum(prevscores$basen[prevscores$dep=="Yes" & prevscores$gender==gender & prevscores$agegrp==agegrp])
  secalc <- (prev*recipprev/nprev)
  seprev <- sqrt(secalc)
 }
 
 pretestout <- list(prev=prev,seprev=seprev)
}
### end function

##########################################################################
### calculate s.e.
s.err <- function(x) sd(x)/(sqrt(length(x))) 
### end function

##########################################################################
#### RESAMPLE TEST DATA (bootstrap)
boot1 <- function() {
 testdata <- getdata_out$testdata
 np <- t(subset(testdata,select = np))
 nn <- t(subset(testdata,select = nn))
 gp <- c()
 gn <- c()
 # create vector with all values to sample from
 for(h in 1:length(np)) {
  gp <- append(gp,rep(h,np[h]))   
  gn <- append(gn,rep(h,nn[h]))
 } 
 npx <- sample(gp,size=it*length(gp),replace=T)
 nnx <- sample(gn,size=it*length(gn),replace=T)
 npos <- c()
 nneg <- c()
 for(h in 1:length(np)) {
  npos[h] <- length(which(npx == h))
  nneg[h] <- length(which(nnx == h))
 }
 npos <- data.frame(npos)
 nneg <- data.frame(nneg)
 timewrite <- format(Sys.time(), "%b%d_%Y_%H%M")
 path1 <- paste(pathout,"resamplednp_",testname,"~",refname,"_",gender,"_",agegrp,"_",timewrite,".txt",sep="")
 write.table(npos, file=path1,quote=FALSE)
 path1 <- paste(pathout,"resamplednn_",testname,"~",refname,"_",gender,"_",agegrp,"_",timewrite,".txt",sep="")
 write.table(nneg, file=path1,quote=FALSE)
 
 boot1_out <- list(np=np,nn=nn,gp=gp,gn=gn,npx=npx,nnx=nnx)
}
### end function

###############################################################################
### Calculate Neg Binomial fit or Poisson fit
distr_nbinomial <- function(npx,nnx,gp,gn) { 
 library(MASS)
 testdata <- getdata_out$testdata
 curve <- paste("Negative_Binomial_")
 max = length(testdata$score)  
 
 eps <- sqrt(.Machine$double.eps)
 NBp.fit <- fitdistr(npx,'negative binomial',lower=c(eps,eps))
 fitp <- dnbinom(c(1:max),size=NBp.fit$estimate["size"], mu=NBp.fit$estimate["mu"])

 NBn.fit <- fitdistr(nnx,'negative binomial',lower=c(eps,eps))
 fitn <- dnbinom(c(1:max),size=NBn.fit$estimate["size"], mu=NBn.fit$estimate["mu"])

 ypfit <- fitp * length(gp) * it
 ynfit <- fitn * length(gn) * it
 dp <- sum(ypfit)
 dn <- sum(ynfit)
 fdistout <- list(ypfit=ypfit,ynfit=ynfit,dp=dp,dn=dn,max=max,curve=curve)
}
distr_poisson <- function(npx,nnx,gp,gn) { 
  library(MASS)
  testdata <- getdata_out$testdata
  curve <- paste("Poisson_")
  max <- length(testdata$score)  
  
  eps <- sqrt(.Machine$double.eps)
  NBp.fit <- fitdistr(npx,'poisson',lower=c(eps,eps))
  fitp <- dpois(c(1:max),size=NBp.fit$estimate["size"])
  
  NBn.fit <- fitdistr(nnx,'poisson',lower=c(eps,eps))
  fitn <- dnbinom(c(1:max),size=NBn.fit$estimate["size"])
  
  ypfit <- fitp * length(gp) * it
  ynfit <- fitn * length(gn) * it
  dp <- sum(ypfit)
  dn <- sum(ynfit)
  fdistout <- list(ypfit=ypfit,ynfit=ynfit,dp=dp,dn=dn,max=max,curve=curve)
}
### end fit functions

####################################################################################
### RESAMPLE INITIAL BOOTSTRAP SAMPLE; CALC Z, SEZ, AND COR(Z,Z) WITH FINITE 
### DIFFERENCE TECHNIQUE FOR ERROR PROPAGATION (ADVICE FROM JOHN PEZZULLO)   
boot2 <- function() {
    testdata <- getdata_out$testdata
    #Range of test scores e.g. 0 to 40 (max=41), or 1 to 10 (max=10)
    max <- length(testdata$score)  
    
    pretestout <- pretest()
    prev <- pretestout$prev
    seprev <- pretestout$seprev
    gp <- boot1_out$gp
    gn <- boot1_out$gn
    npx <- boot1_out$npx
    nnx <- boot1_out$nnx
    
    system.time(
    fdistout <- distr_nbinomial(npx,nnx,gp,gn)[distribution == 1]
    #fdistout <- distr_poisson(npx,nnx,gp,gn)[distribution == 2]
    )
    
    ypfit <- fdistout$ypfit
    ynfit <- fdistout$ynfit
    dp <- fdistout$dp
    dn <- fdistout$dn
    
    sdnp <- c()
    sdnn <- c()
    z <- rep(1,max)
    dfdx <- c()
    dfdy <- c()
    dfdv <- c()
    sez <- rep(1,max)
    
    system.time(
    for(i in 1:max) 
    {
      #P(B|A) AND ERRORs
      sdnp <- sqrt((ypfit[i]*(dp-ypfit[i])/dp))
      sdnn <- sqrt((ynfit[i]*(dn-ynfit[i])/dn))
      z[i] <- (ypfit[i]/dp*prev)/(ypfit[i]/dp*prev+ynfit[i]/dn*(1-prev))
      dfdx <- ((ypfit[i]+sdnp/2)/dp*prev/((ypfit[i]+sdnp/2)/dp*prev+ynfit[i]/dn*(1-prev)) ) - ((ypfit[i]-sdnp/2)/dp*prev/((ypfit[i]-sdnp/2)/dp*prev+ynfit[i]/dn*(1-prev)))
      dfdy <- (ypfit[i]/dp*prev/(ypfit[i]/dp*prev+(ynfit[i]+sdnn/2)/dn*(1-prev))) -  (ypfit[i]/dp*prev/(ypfit[i]/dp*prev+(ynfit[i]-sdnn/2)/dn*(1-prev)))
      dfdv <- (ypfit[i]/dp*(prev+seprev/2)/(ypfit[i]/dp*(prev+seprev/2)+ynfit[i]/dn*(1-(prev+seprev/2)))) - (ypfit[i]/dp*(prev-seprev/2)/(ypfit[i]/dp*(prev-seprev/2)+ynfit[i]/dn*(1-(prev-seprev/2))))
      sez[i] <- sqrt(dfdx^2 + dfdy^2 + dfdv^2 )
    }
    )
    
    ## LOOP THROUGH TEST TO CREATE VALS FOR COR MATRIX
    weightp <- rep(1,max)
    resnp <- rep(1,max)
    combp <- rep(1,max)
    weightn <- rep(1,max)
    resnn <- rep(1,max)
    combn <- rep(1,max)
    meanpos <- rep(1,max)
    meanneg <- rep(1,max)
    zct <- matrix(1, nrow=it, ncol=max)
    
    posneg.fun <- function(x) {
      resamp <- function(r,m) {   ### RESAMPLE BOOTSTRAP FUNCTION - FOR IDENTIFIYING ERRORS AROUND POSTERIOR PROBS
        n <- length(r)
        Data <- matrix(sample(resnp,size=n*m,replace=T),nrow=m)
        thetastar <- rowSums(Data)/m 
        
        return(thetastar)
      }
      
      #POSITIVES
      weightp <- c((dp-ypfit[x])/dp,ypfit[x]/dp)
      resnp <- sample(nprob,size=it,replace=T,prob=weightp)
      combp <- resamp(resnp,it) 
      
      #NEGATIVES
      weightn <- c((dn-ynfit[x])/dn,ynfit[x]/dn)
      resnn <- sample(nprob,size=it,replace=T,prob=weightn)
      combn <- resamp(resnn,m=it) 
      
      meanpos <- sum(combp)/length(combp)
      meanneg <- sum(combn)/length(combn)
      
      #CORR MATRIX
      zc <- rep(1:it)
      
      for(j in 1:it) {
        ifelse(combp[j]==0 && combn[j]==0, zc[j] <- 0, zc[j] <- (combp[j]/dp*prev)/(combp[j]/dp*prev+combn[j]/dn*(1-prev)))
      }
      zct[,x]<-zc
    }
  
  set.seed(42)  
  library(parallel)
  ncores <- detectCores()
  
  system.time(  
    mclapply(1:max, try(posneg.fun), mc.cores=ncores)
  )
  

    #SOME OUTPUTS
    timewrite <- format(Sys.time(), "%b%d_%Y_%H%M")
    tout <- data.frame(ypfit,ynfit)
    path1 <- paste(pathout,"fit_",testname,"~",refname,"_",gender,"_",agegrp,"_",timewrite,".csv",sep="")
    write.csv(tout, file=path1,quote=FALSE)
    
    b2 <- data.frame(z,sez)
    path1 <- paste(pathout,fdistout$curve,"_",testname,"~",refname,"_",gender,"_",agegrp,"_",timewrite,"_posterior_out.csv",sep="")
    write.csv(b2,path1,quote=FALSE)
    
    mat <- cor(zct,zct)
    mat[which(is.na(mat))] <- 0  # RESET ALL MISSING TO ZERO BECAUSE CORRELATION WHEN x or y ALL SAME VALUE PRODUCES "NA"
    path1 <- paste(pathout,fdistout$curve,"_",testname,"~",refname,"_",gender,"_",agegrp,"_",timewrite,"_zcormatrix.csv",sep="")
    write.csv(mat,file=path1)

    
    boot2_out <- list(b2,fdistout,mat)
    
  }
### end function BOOT2

#######################################################################################
### write outputs to file and plots
outputs1 <- function() {
 library(ggplot2)
 testdata<-getdata_out$testdata
 np<-boot1_out$np
 nn<-boot1_out$nn
 gp<-boot1_out$gp
 gn<-boot1_out$gn
 npx<-boot1_out$npx
 nnx<-boot1_out$nnx
 ypfit<-boot2_out[[2]]$ypfit
 ynfit<-boot2_out[[2]]$ynfit
 dp<-boot2_out[[2]]$dp
 dn<-boot2_out[[2]]$dn
 b2<-boot2_out[[1]]  
 mat<-boot2_out[[3]]
 pretestout<-pretest()
 baseprev<-pretestout$prev
 
 Npos <- c(testdata$np)
 Nneg <- c(testdata$nn)
 fitp <- ypfit / (length(gp) * it)
 fitn <- ynfit / (length(gn) * it)
 ntimes <- length(Npos)
 
 Scorecount <- length(testdata$score)
 if (Scorecount > 81) Scorebreaks <- as.numeric(c("20"))
 if (Scorecount < 80) Scorebreaks <- as.numeric(c("10"))
 if (Scorecount < 45) Scorebreaks <- as.numeric(c("5"))
 if (Scorecount < 20) Scorebreaks <- as.numeric(c("2"))
  
 grplabs1 <- paste("(a) Probability of positive diagnosis (N=",length(gp),")",sep="")
 grplabs2 <- paste("(b) Probability of negative diagnosis (N=",length(gn),")",sep="")
 grplabs3 <- paste("(c) Post-test Probability, from Bayes theorem\n (shaded area shows 95% Confidence Interval)")
 grplabs4 <- paste("Post-test Probability, from Bayes theorem\n (shaded area shows 95% Confidence Interval)")
  
 lb <- b2$z-(1.96*b2$sez) 
 lb <- ifelse(lb>0,lb,0)   
 ub <- b2$z+(1.96*b2$sez) 
 ub <- ifelse(ub<1,ub,1)   
 
 df2 <- data.frame (
 Score=rep(c(scoremin:scoremax), times=3),
 Nplot=c(Npos/sum(testdata$np),Nneg/sum(testdata$nn),rep(c(NA),times=ntimes)),
 Nfit=c(fitp,fitn,rep(c(NA),times=ntimes)),
 zplot=c(rep(c(NA), times=ntimes*2),b2$z),
 zplotlb=c(rep(c(NA), times=ntimes*2),lb),
 zplotub=c(rep(c(NA), times=ntimes*2),ub),
 Group=rep(c(grplabs1,grplabs2,grplabs3), each=ntimes)
 )

 p <- ggplot(data=df2) 
 p <- p + geom_bar(aes(Score,Nplot),binwidth=40, colour="white", fill="lightgreen", stat="identity")
 p <- p + geom_line(aes(Score,Nfit),size=.8,linetype=2, colour="blue")
 p <- p + geom_line(aes(Score,zplot),size=.8,linetype=2, colour="blue") 
 p <- p + geom_ribbon(aes(Score,ymin=zplotlb,ymax=zplotub), alpha=0.1)
 p <- p + scale_y_continuous("")
 p <- p + scale_x_continuous(testname, breaks = seq(scoremin,scoremax, by = Scorebreaks))
 p <- p + facet_wrap(~Group, ncol=1, scales="free") 
 p <- p + theme(strip.text.y = element_text()) 
 path1<-paste(pathout,testname,"~",refname,"_",gender,"_",agegrp,"_graph1.tiff",sep="")
 ggsave(filename=path1, width=8,height=11) 
 graph1<-p 
 
 
 
#  
#  p <- ggplot(data=df2) 
#  p <- p + geom_bar(aes(Score,Nplot),binwidth=40, colour="black", fill="grey", stat="identity")
#  p <- p + geom_line(aes(Score,Nfit),size=.8,linetype=2, colour="black")
#  p <- p + geom_line(aes(Score,zplot),size=.8,linetype=2, colour="black")
#  p <- p + geom_ribbon(aes(Score,ymin=zplotlb,ymax=zplotub), alpha=0.2)
#  p <- p + scale_y_continuous("")
#  p <- p + scale_x_continuous(testname, breaks = seq(scoremin,scoremax, by = Scorebreaks))
#  p <- p + facet_wrap(~Group, ncol=1, scales="free") 
#  p <- p + theme(strip.text.y = element_text()) 
#  p <- p + theme_bw()
#  graph2<-p
#  path1<-paste(pathout,testname,"~",refname,"_",gender,"_",agegrp,"_graph2.tiff",sep="")
#  ggsave(filename=path1, width=8,height=11) 
#  #print(graph2)
#  
#  df2a <- data.frame (
#    Score=c(scoremin:scoremax),
#    zplot=c(b2$z),
#    zplotlb=c(lb),
#    zplotub=c(ub)
#  )
#  
#  p <- ggplot(data=df2a) 
#  p <- p + geom_line(aes(Score,zplot),size=.8,linetype=1, colour="blue") 
#  p <- p + geom_ribbon(aes(Score,ymin=zplotlb,ymax=zplotub), alpha=0.1)
#  p <- p + scale_y_continuous("Probability")
#  p <- p + scale_x_continuous(testname, breaks = seq(scoremin,scoremax, by = Scorebreaks)) 
#  p <- p + ggtitle(grplabs4)
#  p <- p + theme(axis.text.y = element_text(size = rel(1.3), angle = 00))
#  p <- p + theme(axis.text.x = element_text(size = rel(1.3), angle = 00))
#  p <- p + theme(axis.title.y = element_text(size = rel(1.3), angle = 90))
#  p <- p + theme(axis.title.x = element_text(size = rel(1.3), angle = 00))
#  graph1a<-p
#  path1<-paste(pathout,testname,"~",refname,"_",gender,"_",agegrp,"_graph1a.tiff",sep="")
#  ggsave(filename=path1,width=11,height=8.5)
#  #print(graph1a)
#  
#  p <- ggplot(data=df2a) 
#  p <- p + geom_line(aes(Score,zplot),size=.8,linetype=1, colour="black") 
#  p <- p + geom_ribbon(aes(Score,ymin=zplotlb,ymax=zplotub), alpha=0.1)
#  p <- p + scale_y_continuous("Probability")
#  p <- p + scale_x_continuous(testname, breaks = seq(scoremin,scoremax, by = Scorebreaks))
#  p <- p + ggtitle(grplabs4)
#  p <- p + theme(axis.text.y = element_text(size = rel(1.3), angle = 00))
#  p <- p + theme(axis.text.x = element_text(size = rel(1.3), angle = 00))
#  p <- p + theme(axis.title.y = element_text(size = rel(1.3), angle = 90))
#  p <- p + theme(axis.title.x = element_text(size = rel(1.3), angle = 00))
#  graph2a<-p
#  path1<-paste(pathout,testname,"~",refname,"_",gender,"_",agegrp,"_graph2a.tiff",sep="")
#  ggsave(filename=path1,width=11,height=8.5)
#  #print(graph2a)
#  

 out_out1<-list(graph1=graph1) #,graph2=graph2,graph1a=graph1a,graph2a=graph2a,baseprev=baseprev,df2a=df2a)

}
### end function 

################################################################################
### WRITE FILES FOR USE IN SHINY APP
stufftoshiny<-function() {
  stuff<-paste(path,"ShinyApps\\bayes\\df2a_",testname,"~",refname,"_",gender,"_",agegrp,".csv", sep="")
  write.csv(out_out1$df2a,file=stuff)
  baseprev<-pretest()$prev
  stuff<-paste(path,"ShinyApps\\bayes\\baseprev_",testname,"~",refname,"_",gender,"_",agegrp,".csv", sep="")
  write.csv(baseprev,file=stuff,row.names=FALSE)
  return(NULL)
}
### end function


#####################################################################################
#                                                                                             
#                    Code for estimating prevalence in trial data below                       
#                                                                                           
#####################################################################################

############################################################################
### SEARCH AND REPLACE FUNCTION FOR ADDING CORRESPONDING P(B|A) TO TRIAL DATASET, IN FOLLOWING CODE
SR <- function(x, Search, Replace, tol = .Machine$double.eps ^ 0.5) {
  Ind <- sapply(x, function(i) which(abs(i - Search) < tol))
  names(Ind) <- seq(length(Ind))
  Ind <- unlist(Ind)
  x[as.integer(names(Ind))] <- Replace[Ind]
  x
}
### end function

#############################################################################
### ADD P(B|A) TO TRIAL DATA SET
bayestheorem<-function() {
 path1<-paste(path,trial,".csv",sep="")
 trialdata<-read.csv(path1)
 z<-boot2_out[[1]]$z
 Search<-c(0:40)
 Replace<-z
 zbase<-SR(trialdata$audit,Search,Replace)
 zfu<-SR(trialdata$audit12,Search,Replace)
 trialdata<-cbind(trialdata,zbase,zfu)
 trialdata<-subset(trialdata,condit_rev=="Intervention" | condit_rev=="Control")
 path1 <- paste(pathout,"trialdata_rev_",timewrite,"_.txt",sep="")
 write.table(trialdata,file=path1)
 return(trialdata)
}
### end function

#############################################################################
### CALC SE FOR EACH STAGE / CONDITION COMBINATION
fox1<-function(x,stage,cond,mat,sez,se) {
 j<-1
 h<-0
 sum1<-0
 n<-nrow(subset(x,condit_rev==cond))
 while (j <= max)
 {
  k<-1
  sum2<-0
  nj<-nrow(subset(x,stage==j & condit_rev==cond))
   while (k <= max) 
   {
    nk<-nrow(subset(x,stage==k & condit_rev==cond))
    h<- nj * nk * sez[j] * sez[k] * mat[j,k] 
    sum2<-h+sum2
    k<-k+1
    }
  sum1<-sum1+sum2
  j<-j+1
 }
 s.e.<-sqrt(se^2/n + 1/n^2 * sum1)
}
### end function

##################################################################################
### CALC SE FOR ARR
fox2<-function(x,stage,cond1,cond2,mat,sez,s1,s2) {
 j<-1
 h<-0
 sum1<-0
 nt1<-nrow(subset(x,condit_rev==cond1))
 nt2<-nrow(subset(x,condit_rev==cond2))
 while (j <= max)
 {
  k<-1
  sum2<-0
  nj1<-nrow(subset(x,stage==j & condit_rev==cond1))
  nj2<-nrow(subset(x,stage==j & condit_rev==cond2))
  while (k <= max) 
   {
   nk1<-nrow(subset(x,stage==k & condit_rev==cond1))
   nk2<-nrow(subset(x,stage==k & condit_rev==cond2))
   h<- (nj1/nt1 - nj2/nt2) * (nk1/nt1 - nk2/nt2) * sez[j] * sez[k] * mat[j,k] 
   sum2<-h+sum2
   k<-k+1
   }
  sum1<-sum1+sum2
  j<-j+1
 }
 s.e.<-sqrt(s1^2/nt1 + s2^2/nt2 + sum1)
}
### end function

#############################################################################
### CALC AND WRITE FINAL OUTPUTS
outputs2<-function() {
 np<-boot1_out$np
 nn<-boot1_out$nn
 gp<-boot1_out$gp
 gn<-boot1_out$gn
 npx<-boot1_out$npx
 nnx<-boot1_out$nnx
 ypfit<-boot2_out[[2]]$ypfit
 ynfit<-boot2_out[[2]]$ynfit
 dp<-boot2_out[[2]]$dp
 dn<-boot2_out[[2]]$dn
 trialdata<-bayes_out
 b2<-boot2_out[[1]]  
 mat<-boot2_out[[3]]

 aud1<-subset(trialdata,condit_rev=="Control")
 seaud1<-s.err(aud1$audit)
 n1<-nrow(aud1)
 aud1<-mean(aud1$audit)

 aud2<-subset(trialdata,condit_rev=="Intervention")
 seaud2<-s.err(aud2$audit)
 n2<-nrow(aud2)
 aud2<-mean(aud2$audit)

 aud3<-subset(trialdata,condit_rev=="Control")
 seaud3<-s.err(aud3$audit12)
 n3<-nrow(aud3)
 aud3<-mean(aud3$audit12)

 aud4<-subset(trialdata,condit_rev=="Intervention")
 seaud4<-s.err(aud4$audit12)
 n4<-nrow(aud4)
 aud4<-mean(aud4$audit12)

 lciaud1<-aud1-1.96*seaud1
 lciaud2<-aud2-1.96*seaud2
 lciaud3<-aud3-1.96*seaud3
 lciaud4<-aud4-1.96*seaud4

 uciaud1<-aud1+1.96*seaud1
 uciaud2<-aud2+1.96*seaud2
 uciaud3<-aud3+1.96*seaud3
 uciaud4<-aud4+1.96*seaud4

 prev1<-subset(trialdata,condit_rev=="Control")
 prev1<-mean(prev1$zbase)
 prev2<-subset(trialdata,condit_rev=="Intervention")
 prev2<-mean(prev2$zbase)
 prev3<-subset(trialdata,condit_rev=="Control")
 prev3<-mean(prev3$zfu)
 prev4<-subset(trialdata,condit_rev=="Intervention")
 prev4<-mean(prev4$zfu)

 s1<-subset(trialdata,condit_rev=="Control")
 s1<-sd(s1$zbase)
 s2<-subset(trialdata,condit_rev=="Intervention")
 s2<-sd(s2$zbase)
 s3<-subset(trialdata,condit_rev=="Control")
 s3<-sd(s3$zfu)
 s4<-subset(trialdata,condit_rev=="Intervention")
 s4<-sd(s4$zfu)

 seprev1<-fox1(trialdata,trialdata$audit,"Control",mat,b2$sez,s1)
 seprev2<-fox1(trialdata,trialdata$audit,"Intervention",mat,b2$sez,s2)
 seprev3<-fox1(trialdata,trialdata$audit12,"Control",mat,b2$sez,s3)
 seprev4<-fox1(trialdata,trialdata$audit12,"Intervention",mat,b2$sez,s4)

 lciprev1<-prev1-1.96*seprev1
 lciprev2<-prev2-1.96*seprev2
 lciprev3<-prev3-1.96*seprev3
 lciprev4<-prev4-1.96*seprev4

 uciprev1<-prev1+1.96*seprev1
 uciprev2<-prev2+1.96*seprev2
 uciprev3<-prev3+1.96*seprev3
 uciprev4<-prev4+1.96*seprev4

 arr<-prev3-prev4
 searr<-fox2(trialdata,trialdata$audit12,"Intervention","Control",mat,b2$sez,s4,s3)
 lcisearr<-arr-1.96*searr
 ucisearr<-arr+1.96*searr

 #WRITE FINAL OUTPUT
 df <- data.frame(
 Timepoint = rep(c("Baseline", "Follow-up"), each = 2),
 Condition = rep(c("Control", "Intervention") ),
 Mean_Score = c(aud1,aud2,aud3,aud4),
 se_Score = c(seaud1,seaud2,seaud3,seaud4),
 LCI_Score = c(lciaud1,lciaud2,lciaud3,lciaud4),
 UCI_Score = c(uciaud1,uciaud2,uciaud3,uciaud4),
 Prevalence = c(prev1,prev2,prev3,prev4),
 se_Prevalence = c(seprev1,seprev2,seprev3,seprev4),
 LCI_Prev = c(lciprev1,lciprev2,lciprev3,lciprev4),
 UCI_Prev = c(uciprev1,uciprev2,uciprev3,uciprev4),
 N = c(n1,n2,n3,n4),
 ARR = c("-","-","-",arr),
 seARR = c("-","-","-",searr),
 LCI_ARR = c("-","-","-",lcisearr),
 UCI_ARR = c("-","-","-",ucisearr)
 )

 path1 <- paste(pathout,"FINAL_RESULTS_",timewrite,".csv",sep="")
 write.csv(df,file=path1)

 df1 <- data.frame(
 Timepoint = rep(c("Baseline", "Follow-up"), each = 2),
 Condition = rep(c("Intervention", "Control") ),
 Prevalence = c(prev2,prev1,prev4,prev3),
 se_Prevalence = c(seprev2,seprev1,seprev4,seprev3)
 )

 path1 <- paste(pathout,test,".txt",sep="")
 testdata<-read.table(file=path1, header=TRUE)

 Npos <- c(testdata$np)
 Nneg <- c(testdata$nn)
 fitp <- ypfit / (length(gp) * it)
 fitn <- ynfit / (length(gn) * it)

 grplabs4 <- paste(" Control, N=",n1,sep="")
 grplabs5 <- paste(" Intervention, N=",n2,sep="")
 yaxmax <- max(Prevalence) + 0.1
 dodge <- position_dodge(width = 0.3)
 
 graph3<-qplot(Timepoint, Prevalence, data = df1, colour=Condition,ylim = c(0,yaxmax), xlab="", ylab="Prevalence (mean post-test probability)", position = dodge) + 
 geom_point(aes(Timepoint, Prevalence, colour=Condition, shape=Condition), size=3, position=dodge) + 
 geom_errorbar(aes(ymin = Prevalence - 1.96*se_Prevalence, 
 ymax = Prevalence + 1.96*se_Prevalence), width = 0.15, position = dodge) +
 geom_line(aes(Timepoint,Prevalence,group=Condition),position=dodge) +
 theme(legend.position = c(0.2, 0.8)) +
 scale_colour_manual(name="", labels=c(grplabs4,grplabs5), values=c("red","blue")) +
 scale_shape_manual(name="", labels=c(grplabs4,grplabs5), values=c(19,17))
 
 path1<-paste(pathout,"graph3.wmf",sep="")
 ggsave(filename=path1)

 graph4<-qplot(Timepoint, Prevalence, data = df1, shape = Condition, ylim = c(0,yaxmax), xlab="", ylab="Prevalence (mean post-test probability)", position = dodge) + 
 geom_point(aes(Timepoint, Prevalence, shape=Condition), size=3, position=dodge) + 
 geom_errorbar(aes(ymin = Prevalence - 1.96*se_Prevalence, 
 ymax = Prevalence + 1.96*se_Prevalence), width = 0.15, size=0.3, position = dodge) +
 geom_line(aes(Timepoint,Prevalence,group=Condition),linetype=1,position=dodge) +
 theme(legend.position = c(0.2, 0.8)) +
 scale_shape(name="", labels=c(grplabs4,grplabs5))
 
 
 path1<-paste(pathout,"graph4.wmf",sep="")
 ggsave(filename=path1)

 out_out1<-list(graph3,graph4)
}
### end function
