##########################################################################
setwd("~/Documents/Coding/BayesScripts")
rm(list = ls(all = TRUE)) 
pkgs = names(sessionInfo()$otherPkgs)
pkgs = paste('package:', pkgs, sep = "")
lapply(pkgs, detach, character.only = TRUE, unload = TRUE)

### pandoc syntax
name="BayesPrev"
library(markdown)
library(knitr)
knitsDoc <- function(name) {
  bib<-paste0("~/Documents/Bibtex/Tests~Bayes.bib")  
  csl<-paste0("~/Documents/Bibtex/BMJ.csl")
  knit(paste0(name, ".Rmd"), encoding = "utf-8")
  system(paste0("pandoc -S -c custom.css -o ", name, ".html ", name, ".md --bibliography ",bib," --csl ",csl," --self-contained"))
  system(paste0("pandoc -S -o ", name, ".docx ", name, ".html"))  
}
cat("\014")  # clear console
knitsDoc(name)


##
##
##






###dataset manipulations (usually only once)

df1<-read.csv("ARUK_dataset1_all responders_merged_trimmed_clean_NO_RECODES.csv", header=TRUE, stringsAsFactors=FALSE)
df2<-read.csv("depr_matched.csv",header=TRUE, stringsAsFactors=FALSE)
colnames(df1)
colnames(df2)
depr<-merge(df1,df2, by="input_row", all.x=TRUE)
colnames(depr)
depr$AUDIT1<-recode(depr$AUDIT1, "1=0; 2=1; 3=2; 4=3; 5=4")
depr$AUDIT2<-recode(depr$AUDIT2, "1=0; 2=1; 3=2; 4=3; 5=4")
depr$AUDIT3<-recode(depr$AUDIT3, "1=0; 2=1; 3=2; 4=3; 5=4")
depr$AUDIT4<-recode(depr$AUDIT4, "1=0; 2=1; 3=2; 4=3; 5=4")
depr$AUDIT5<-recode(depr$AUDIT5, "1=0; 2=1; 3=2; 4=3; 5=4")
depr$AUDIT6<-recode(depr$AUDIT6, "1=0; 2=1; 3=2; 4=3; 5=4")
depr$AUDIT7<-recode(depr$AUDIT7, "1=0; 2=1; 3=2; 4=3; 5=4")
depr$AUDIT8<-recode(depr$AUDIT8, "1=0; 2=1; 3=2; 4=3; 5=4")
depr$AUDIT9<-recode(depr$AUDIT9, "1=0; 3=4")
depr$AUDIT10<-recode(depr$AUDIT10, "1=0; 3=4")
depr$AUDIT1[which(depr$AUDIT1==-1)] <- NA 
depr$AUDIT2[which(depr$AUDIT2==-1)] <- NA 
depr$AUDIT3[which(depr$AUDIT3==-1)] <- NA 
depr$AUDIT4[which(depr$AUDIT4==-1)] <- NA 
depr$AUDIT5[which(depr$AUDIT5==-1)] <- NA 
depr$AUDIT6[which(depr$AUDIT6==-1)] <- NA 
depr$AUDIT7[which(depr$AUDIT7==-1)] <- NA 
depr$AUDIT8[which(depr$AUDIT8==-1)] <- NA 
depr$AUDIT9[which(depr$AUDIT9==-1)] <- NA 
depr$AUDIT10[which(depr$AUDIT10==-1)] <- NA 

depr$AUDITscore<-rowSums(depr[,128:137])
depr$AUDITC<-rowSums(depr[,128:130])

write.csv(depr,"ARUK_dataset1_all responders_merged_trimmed_clean_NO_RECODES.csv_with_DEPR.csv", row.names=FALSE)


df4<-data.frame(depr$ID_CIDI,depr$ID,depr$input_row,depr$standard_postcode,depr$IMD_200710nov,
                depr$IMD_2007_RANK10nov,depr$OAC10nov,depr$URINDEW10nov,depr$LUT_FILTER10nov,
                depr$input_postcode)
df5 <-read.csv("ARUK_dataset2_full_responders_cleaned_WITH_RECODES_DEIDENTIFIED.csv",header=TRUE, stringsAsFactors=FALSE)

df6<-merge(df4,df5, by.x="depr.ID_CIDI", by.y="ID_CIDI")
write.csv(df6,"ARUK_dataset2_full_responders_cleaned_WITH_RECODES_DEIDENTIFIED_with_DEPR.csv", row.names=FALSE)



##
##



filename<-datasetname
path1<-paste(path,filename,sep="")
data <- read.csv(file=path1, header=TRUE)

data$dsm4_dependence<-recode(data$dsm4_dependence, "5=0")
data$dsm4_abuse<-recode(data$dsm4_abuse, "5=0")
dsm5_aud1<-c(1:nrow(data))
data$dsm5_aud1<-recode(data$dsm5_disorder, "2:3=1" )
data$dsm5_aud2<-recode(data$dsm5_disorder, "1=0; 2:3=1")

table(data$dsm5_disorder)
table(data$dsm5_aud1)
table(data$dsm5_aud2)
write.csv(data,file=path1,row.names=FALSE)


##
##




##################### "run the code" in area below #######################
source("BayesPrev.R")
source("BayesPrev_functions.R")

gender<-c("Female", "Male")
refname=factor(c("dsm4_dep","dsm4_abu","dsm5_aud1","dsm5_aud2"))
testname=c("AUDIT","AUDIT-C")
prevest=c("Data", "Other")
agegrp=c("18-35")

require(utils)
group <- as.matrix(expand.grid(testname,gender,refname))
colnames(group) <- c("testname","gender","refname")
for(i in 1:nrow(group)) {
  gender <- group[i,2]
  testname <- group[i,1]
  refname <- group[i,3]
  analset_out<-analset(refname=refname,
                     testname=testname,
                     prevest="Data",
                     gender=gender,
                     agegrp="18-35",
                     distr="Negative Binomial",
                     it="100")
  source("analset.R")
  setwd(path)
  source("BayesPrev_functions.R")

  #these routines if just have diagnostic accuracy data
  getdata_out <- getdata()
  source("ROC.R")
  roccalc_out<-roccalc()
  rocplot()
  source("diagtables.R")
  diagtable_out<-diagtable()
  boot1_out<-boot1()
  boot2_out<-boot2()
  out_out1<-outputs1() # all charts

  #stuff to use for shiny app
  #stufftoshiny()

  #plus these routines to add trial data  NB need to edit getdata() function in BayesPrev_functions.R
  #bayes_out<-bayestheorem()
  #out_out2<-outputs2()
}
##########################################################################
