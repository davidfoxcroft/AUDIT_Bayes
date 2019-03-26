diagtable<-function() {
  library(epiR)
  library(xtable)

  testdata <- getdata_out$testdata
  Yindex <- roccalc_out$Yindex
  lowcut <- Yindex$score-5
  highcut <- Yindex$score+5
  ifelse(lowcut<=min(testdata$score),lowcut <- min(testdata$score)+1,lowcut <- lowcut)
  ifelse(highcut>=max(testdata$score),highcut <- max(testdata$score)-1,highcut <- highcut)
  
  lengthcut <- length(c(lowcut:highcut))
  out_mat <- c()
  testdata$score[scoremin==0] <- testdata$score-1

  system.time(
  for(cutpoint in lowcut:highcut) {
    fn <- sum(testdata$np[testdata$score<cutpoint])
    tp <- sum(testdata$np[testdata$score>=cutpoint])
    tn <- sum(testdata$nn[testdata$score<cutpoint])
    fp <- sum(testdata$nn[testdata$score>=cutpoint])
    diagtable <- as.table(matrix(c(tp,fp,fn,tn),nrow=2,byrow=TRUE))
    colnames(diagtable) <- c("Dis+","Dis-")
    rownames(diagtable) <- c("Test+","Test-")
    res <- epi.tests(diagtable, conf.level = 0.95, verbose = TRUE)
    out <- c(cutpoint,tp,fp,fn,tn,res$se,res$sp,res$ppv,res$npv,res$plr,res$youden)
    out_mat <- rbind(out_mat,out)
  }
  )
  tprev <- as.data.frame(res$tprev)
  tseprev <- (tprev$est-tprev$lower)/1.96
  tprev <- tprev$est
  
  rownames(out_mat) <- lowcut:highcut
  out_mat[,1] <- c(lowcut:highcut)
  out_mat[,1][scoremin==0] <- as.numeric(out_mat[,1])  
  out_mat[1:lengthcut,1:5] <- sprintf("%.0f", out_mat[1:lengthcut,1:5])
  out_mat[1:lengthcut,6:23] <- sprintf("%.2f", out_mat[1:lengthcut,6:23])
  
  
  # Weighted Youden Index
  SensW <- c(0.25,0.5,0.75)
  Jw <- lapply(SensW, function(x) 2*(x*as.numeric(out_mat[,6])+(1-x)*as.numeric(out_mat[,9]))-1)
  Jw <- data.frame(Jw)
  colnames(Jw) <- SensW
  rownames(Jw) <- c(lowcut:highcut)
  Jw <- cbind(rownames(Jw),Jw)
  #colnames(Jw) <- c("Test Score", "Favour Low Sensitivity", "Equal Sensitivity / Specificity", "Favour High Sensitivity")
  
  
  out1 <- paste(out_mat[,6]," (",out_mat[,7]," to ",out_mat[,8],")", sep="")
  out2 <- paste(out_mat[,9]," (",out_mat[,10]," to ",out_mat[,11],")", sep="")
  out3 <- paste(out_mat[,12]," (",out_mat[,13]," to ",out_mat[,14],")", sep="")
  out4 <- paste(out_mat[,15]," (",out_mat[,16]," to ",out_mat[,17],")", sep="")
  out5 <- paste(out_mat[,18]," (",out_mat[,19]," to ",out_mat[,20],")", sep="")
  out6 <- paste(out_mat[,21]," (",out_mat[,22]," to ",out_mat[,23],")", sep="")
  
  out_tab <- as.data.frame(cbind(out_mat[,1],out_mat[,2],out_mat[,3],out_mat[,4],out_mat[,5],
                               out1,out2,out3,out4,out5,out6,Jw[,4],Jw[,2]))
  colnames(out_tab) <- c("Test Score (TS)", "TP", "FP", "FN", 
                       "TN", "Sens (95% CI)","Spec (95% CI)",
                       "PPV (95% CI)","NPV (95% CI)","+LR (95% CI)","Youden J (95% CI)",
                       "TS: Jw for Sens",
                       "TS: Jw for Spec")
  
    
  tabletitle <- paste("<br><b>Table: Predicting ",reflabel," with ",testlabel," score (",gender,"s aged ",
                  agegrp,"):<br>Summary Statistics, 95% Confidence Intervals and Weighted Thresholds</b><br>", sep="")

  
  
  diagtable_out <- list(tabletitle=tabletitle,out_tab=out_tab,out_mat=out_mat,tprev=tprev,tseprev=tseprev)
}