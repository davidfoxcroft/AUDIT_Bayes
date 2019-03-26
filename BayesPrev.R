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

it<-10000 # specify number of iterations for various bootstraps (lower = quicker but less accurate)
gender <- c("Male", "Female")
agegrp <- c("18-35")

## use reduced parameters below for quick testing
#gender <- c("Female")
#refname <-c ("dsm5_aud2")
#testname <- c("AUDIT")
#it<-100


refnames <- c("TLFB_hazardous", 
#               "haz.30days.all3", 
#               "haz.30days.2outof3", 
#               "haz.10days.5outof9",
               "TLFB_binge",
#               "binge.30days.all3", 
#               "binge.30days.2outof3", 
#               "binge.10days.5outof9",
#               "TLFB_binge_not_haz", 
#               "binge.not.haz.30days.2outof3",
               "dsm4_dependence", 
               "dsm4_abuse", 
               "dsm5_aud1", 
               "dsm5_aud2"
              )

reflabels <- c("Hazardous Drinking (any)",
#               "Hazardous drinker (monthly)",
#               "Hazardous drinker (at least 2 out of every 3 months)",
#               "Hazardous drinker (at least 5 out of every 9 10-day periods)",
               "Binge drinker (any)",
#               "Binge drinker (monthly)",
#               "Binge drinker (at least 2 out of every 3 months)",
#               "Binge drinker (at least 5 out of every 9 10-day periods)",
#               "Binge drinker (but not hazardous) (any)",
#               "Binge drinker (but not hazardous) (at least 2 out of every 3 months)",
               "DSM-IV Alcohol Dependence",
               "DSM-IV Alcohol Abuse",
               "DSM-5 Alcohol Use Disorder: none vs. mild-severe",
               "DSM-5 Alcohol Use Disorder: none/mild vs. moderate/severe"
              )

testnames <- c("AUDITscore",
             "AUDITC"
             )
testlabels <- c("AUDIT", 
               "AUDIT-C"
              )

prevest <- c("Data") # prior probability from sample data
prevdata<-c("APMS_2007_Dependence_Prevalence")

datasetname <- c("ARUK_dataset2_full_responders_cleaned_WITH_RECODES_DEIDENTIFIED_with_DEPR.csv")

path <- paste("~/Documents/Coding/BayesScripts/")
pathout <- paste(path,"Outfiles/",sep="")
setwd(path)


cutrange_default <- TRUE
#lowcut<-5 # low cutpoint for table for sens, sepcm, ppv etc
#highcut<-14 # high cutpoint ...

distribution <- c()
distribution <- 1 # Negative Binomial
#distribution <- 2 # Poisson 

nprob <- c(0,it) # FOR ESTIMATING PROBABILITY OF EACH TEST SCORE IN BOOTSTRAP

