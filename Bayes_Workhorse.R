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


#### Workhorse for BayesPrev.Rmd

library(knitr)
source("BayesPrev_functions.R")
TLFB.analysis()
firststats_out<-firststats()
sites <- get.sites()

library(utils)
groupstuff <- as.matrix(expand.grid(testnames,gender,refnames))
colnames(groupstuff) <- c("testname","gender","refname")

Sectitle <- list()
plotatext <- list()
plotbtitle <- list()
roctitle <- list()
bayestext <- list()
plota <- list()
sum_out <- list()
sum_row <- list()
plotb <- list()
bigtabletitle <- list()
bigtable <- list()
bling <- list()

#groupstuff
#i <- 8 # for testing
for(i in 1:nrow(groupstuff)) {
  gender <- groupstuff[i,2]
  testname <- groupstuff[i,1]
  testlabel <- testlabels[[match(testname, testnames)]]
  refname <- groupstuff[i,3] 
  reflabel <- reflabels[[match(refname, refnames)]]
  scoremax <- c()
  scoremin <- c()
  scoremin[testname=="AUDITscore"] <- 0
  scoremax[testname=="AUDITscore"] <- 40
  scoremin[testname=="AUDITC"] <- 0
  scoremax[testname=="AUDITC"] <- 12
  Sectitle[[i]] <- knit_expand(text = "<h4><u>{{i}}. {{reflabel}} by {{testlabel}} Score: {{gender}}s aged {{agegrp}}</u></h4><p>")
  getdata_out <- getdata()
  source("ROC.R")
  roccalc_out <- roccalc()
  plotroc <- rocplot()
  plota[[i]] <- plotroc$plotroc
  roctitle[[i]] <- plotroc$roctitle
  source("diagtables.R")
  diagtable_out <- diagtable()
  bigtabletitle[[i]] <- diagtable_out$tabletitle
  bigtable[[i]] <- diagtable_out$out_tab
  out_mat <- diagtable_out$out_mat
  Yindex <- roccalc_out$Yindex
   
Txt <- as.character(list(Yindex$score,
       as.numeric(out_mat[,6][out_mat[,1]==Yindex$score]),
       as.numeric(out_mat[,7][out_mat[,1]==Yindex$score]),
       as.numeric(out_mat[,8][out_mat[,1]==Yindex$score]), 
       as.numeric(out_mat[,9][out_mat[,1]==Yindex$score]), 
       as.numeric(out_mat[,10][out_mat[,1]==Yindex$score]), 
       as.numeric(out_mat[,11][out_mat[,1]==Yindex$score]), 
       as.numeric(out_mat[,18][out_mat[,1]==Yindex$score]), 
       as.numeric(out_mat[,19][out_mat[,1]==Yindex$score]), 
       as.numeric(out_mat[,20][out_mat[,1]==Yindex$score]),
       format(1/(as.numeric(out_mat[,21]))[out_mat[,1]==Yindex$score],digits=3, nsmall=2),
       format(1/(as.numeric(out_mat[,22]))[out_mat[,1]==Yindex$score],digits=3,nsmall=2),
       format(1/(as.numeric(out_mat[,23]))[out_mat[,1]==Yindex$score],digits=3,nsmall=2), 
       ceiling(10*1/(as.numeric(out_mat[,21]))[out_mat[,1]==Yindex$score]),          
       min(as.numeric(bigtable[[i]][,1])[as.numeric(bigtable[[i]][,12])==max(as.numeric(bigtable[[i]][,12]))]),
       min(as.numeric(bigtable[[i]][,1])[as.numeric(bigtable[[i]][,13])==max(as.numeric(bigtable[[i]][,13]))])
  ))
  
  sum_out[[i]] <- c(Txt[[1]], Txt[[15]], Txt[[16]])
  sum_row[[i]] <- paste(reflabel,": ",gender,"s",sep="")

  plotatext[[i]] <- knit_expand(text="<br><br>This plot shows the overall accuracy of {{testlabel}} in predicting {{reflabel}}. The Receiver Operating Characteristic (ROC) curve is shown in red (dotted line) with a smoothed version in blue. A 95% confidence interval band for the smoothed ROC is also shown. The area under the curve (AUC) is shown with a 95% Confidence Interval. One approach to selecting an optimal cut-point for a test score is to use the point on the ROC curve which is closest to the upper left corner of the plot. An alternative approach is the use the Youden J index, which represents the point on the ROC curve which is furthest away from the diagonal 45º line that indicates an uninformative test. The Youden J index is used in the study and is shown in the Table below along with a discussion of alternative weightings and interpretations of J.<br><br><br>")
  
  
  bling[[i]] <- knit_expand(text="<br><br>Test sensitivity at the optimal cut-point {{Txt[1]}} using the Youden-Index J threshold is {{Txt[2]}} (95% CI {{Txt[3]}} to {{Txt[4]}}). Test specificity is {{Txt[5]}} (95% CI {{Txt[6]}} to {{Txt[7]}}). The likelihood ratio of a positive test is {{Txt[8]}} (95% CI {{Txt[9]}} to {{Txt[10]}}). The number needed to diagnose (NND) is {{Txt[11]}} (95% CI {{Txt[13]}} to {{Txt[12]}}). Around {{Txt[14]}} persons need to be tested to return 10 positive tests.<p><p>When the J index is weighted (Jw) for different considerations the threshold changes. If sensitivity is favoured (75:25 weight on sensitivity compared with specificity), which may be more desirable when the costs of a false positive test are low, for example in a screening scenario which triggers further investigations or brief advice on healthy behaviour, then a lower optimal cut point may be desirable: {{Txt[15]}} from the Table. This threshold may be desirable from a clinical or population health perspective. However, individual's may value costs differently, and it is feasible that an individual would take umbrage at being labelled as a heavy or risky drinker if in fact they are not; it also runs the theoretical risk of making false positive individuals less likely to respond to health care advice / interventions. If the costs of a false positive are high, then specificity may be weighted as more favourable (25:75 in the example provided in the Table: producing a cut-point of {{Txt[16]}} according to the weighted Jw score.)<br>")
  
  boot1_out <- boot1()
  boot2_out <- boot2()
  out_out1 <- outputs1() # all charts  
  plotb[[i]] <- out_out1$graph1
 
plotbtitle[[i]] <- knit_expand(text="<center><b>Figure: Plot of observed and estimated probability, by {{testlabel}}, of {{reflabel}}, for individuals with a positive diagnosis (a), and negative diagnosis (b); and post-test probabilities using Bayes’ Theorem &#40;c)</b></center><br><br><br>")
  
bayestext[[i]] <- knit_expand(text="<br>While using a single, simply applied threshold as a cut-point for screening and diagnostic tests may have advantages in practice settings,  there  are  disadvantages  if  a  simple  threshold approach  is  used  in  epidemiological  research  or  policy work where questions focus upon population prevalence and effectiveness of  interventions. Each point, or score, on  a  diagnostic  scale  provides  useful  information  that may be lost if  scores are collapsed together into negative or positive categories. However, studies of such diagnostic tests often simplify their results by calculating sensitivity and specificity and related indicators when compared to a reference standard criterion for the presence or absence of a condition or disease, where all values above a single threshold level are considered 'positive' and all those below it are considered 'negative'. This simplistic approach implies that all test results above the threshold increase the likelihood that the condition or disease is present to exactly the same degree. However, if  the likelihood associated with a range of  different thresholds, for example each point on a diagnostic test  scale,  can  be  calculated  then  much  more  precise estimates  of  the  risk  of  a  condition  or  disease  can  be made. This is particularly important when risk increases proportionately or exponentially with test score. In the Figure below, we show that post-test probability, calculated using Bayes Theorem, varies according to the slope of a curve across the range of test scores.<br><br>")
  
}

