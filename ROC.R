roccalc<-function() {
  library(memisc)
  library(car)
  library(pROC)

  filename<-datasetname
  path1<-paste(path,filename,sep="")
  data <- read.csv(file=path1, header=TRUE)
  
  refvar<-data[[refname]]
  testvar<-data[[testname]]

  datam <- subset(data, Sex==1, select=c(refname,testname))
  dataf <- subset(data, Sex==2, select=c(refname,testname))
  refvarm<-datam[[refname]]
  testvarm<-datam[[testname]]
  refvarf<-dataf[[refname]]
  testvarf<-dataf[[testname]]
  
  ifelse(gender=="Male",testvar<-testvarm,testvar<-testvarf)
  ifelse(gender=="Male",refvar<-refvarm,refvar<-refvarf)
  
  
  ###### ROC Analyses #######
  rocobj_emp<-roc(refvar,testvar)
  Yindex<-as.data.frame(coords(rocobj_emp, "best", ret=c("threshold","sens","spec"), 
                               best.method="youden")) 
  Yindex<-rbind(Yindex,round(Yindex[,1]))
  Yindex<-data.frame(t(as.numeric(unlist(Yindex))))
  colnames(Yindex)<-c("threshold","sens","spec","score")
  Yindex$score<-ceiling(Yindex$threshold)
  
  spec_emp<-rocobj_emp$specificities
  sens_emp<-rocobj_emp$sensitivities
    
  newdat.emp<-data.frame(cbind(rev(1-spec_emp)),rev(sens_emp))
  colnames(newdat.emp)=c("spec_emp","sens_emp")
  
  set.seed(42)
  library(parallel)
  ncores <- detectCores()
  system.time(  
  rocobj <- mclapply(1:ncores, function(x) (roc(refvar,testvar, 
                ci=TRUE, 
                auc=TRUE, 
                smooth=TRUE, 
                smooth.method="density",
                boot.n=300,
                progress="none")),
                mc.cores=ncores
            )
  )
  
  #### for comparing ROC curves
  #rocobj <- roc(refvar,testvar, 
  #              auc=TRUE, 
  #              ci=TRUE, 
  #              smooth=TRUE, 
  #              smooth.method="density",
  #              boot.n=2400)
  #rocobj.frst <- rocobj
  #testobj <- roc.test(rocobj.frst, rocobj) 
  #print(testobj)
  #
  ##
  
  sens <- data.frame(sapply(1:ncores, function(x) unlist(rocobj[[x]][[1]])))  
  sens <- rowSums(sens)/ncores
  spec <- data.frame(sapply(1:ncores, function(x) unlist(rocobj[[x]][[2]])))  
  spec <- rowSums(spec)/ncores
  
  system.time(
  ciobj <- mclapply(1:ncores, function(x) (ci.se(smooth(rocobj[[1]], method="density", 
                                reuse.ci=FALSE),# CI of sensitivity
                                specificities=spec,
                                boot.n=300,
                                progress="none")), 
                                mc.cores=ncores)
  )
  LCI <- data.frame(sapply(1:ncores, function(x) unlist(ciobj[[x]][,1])))  
  LCI <- rowSums(LCI)/ncores
  UCI <- data.frame(sapply(1:ncores, function(x) unlist(ciobj[[x]][,3])))  
  UCI <- rowSums(UCI)/ncores
  
  newdat<-cbind(rev(1-spec),rev(sens),rev(LCI),rev(UCI))
  newdat<-data.frame(newdat)
  colnames(newdat)=c("spec","sens","LCI","UCI")
  
  cis <- as.list(unclass(rocobj[[1]][[8]]))
  auc <- paste("AUC: ",round(rocobj[[1]][[7]], digits=2)," (95% CI ",round(cis[[1]], digits=2),
               " - ",round(cis[[3]],digits=2),") ",sep="")
  
  roccalc_out<-list(newdat=newdat,newdat.emp=newdat.emp,auc=auc,Yindex=Yindex)
}
  
rocplot <- function() {
  library(ggplot2)
  newdat<-roccalc_out$newdat
  newdat.emp<-roccalc_out$newdat.emp
  auc<-roccalc_out$auc
  Yindex<-roccalc_out$Yindex
  
  roctitle<-paste("<br><center><b>Figure: Predicting ",reflabel," with ",testlabel," score (",gender,"s aged ",
                  agegrp,"): ROC Plot with 95% Confidence Region</b></center>",sep="")
  lab_emp<-expression(paste(italic("Density")," Smoothed ROC curve\n",auc,sep=""))
  lab_smooth<-paste("Empirical ROC curve")

  p <- ggplot(data=newdat)
  p <- p + geom_ribbon(aes(spec,ymin=LCI,ymax=UCI),inherit.aes=FALSE,alpha=0.1,color="grey90")
  p <- p + geom_line(aes(spec,sens,colour="smooth",linetype="smooth",size="smooth"))
  p <- p + geom_line(data=newdat.emp, aes(spec_emp,sens_emp,colour="emp",linetype="emp",size="emp"))
  #p <- p + geom_point(data=Yindex, aes(spec,sens), colour="red", size=4)
  p <- p + geom_abline (intercept = 0, slope = 1, size=.3,color="darkgrey")
  p <- p + scale_colour_manual("", breaks = c("smooth", "emp"),
                      values = c("red", "blue"),
                      labels = c(paste("Density smoothed ROC curve\n",auc,sep=""), "Empirical ROC curve"))
  p <- p + scale_linetype_manual(values = c(2,1))
  p <- p + scale_size_manual(values = c(.6,.8))
  p <- p + guides(linetype=FALSE, size=FALSE, 
                  colour=guide_legend(keywidth=3,label.hjust=0,
                                      override.aes=list(linetype=c(1,2),size=c(.8,.6))))
  p <- p + geom_line(aes(spec,sens,colour="smooth"),size=.8,linetype=1)
  p <- p + geom_line(data=newdat.emp, aes(spec_emp,sens_emp,colour="emp"),size=.6,linetype=2)
  p <- p + geom_abline (intercept = 0, slope = 1, linetype="dotted", size=1,color="darkgrey")
  p <- p + scale_x_continuous("\nFalse Positive Rate (1-Specificity)")
  p <- p + scale_y_continuous("True Positive Rate (Sensitivity)\n")
  #p <- p + annotate("text", label = auc, x = .6, y = .4, hjust = 0, size = 4, colour = "black")
  #p <- p + ggtitle(roctitle)
  p <- p + theme(
    plot.title = element_text(face="bold", size=10),
    axis.title.x = element_text(face="bold", size=9),
    axis.title.y = element_text(face="bold", size=9, angle=90),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.key = element_blank(),
    legend.position = c(1, 0),
    legend.justification = c(1, 0))
  plotroc<-p
  path1<-paste(pathout,refname,"~",testname,"_",gender,"_",agegrp,"_ROCplot.tiff",sep="")
  ggsave(filename=path1, width=7, height=7)
  rocplot_out<-list(plotroc=plotroc,roctitle=roctitle)
}










############## ROC playground below



#rocobj <- plot.roc(refvar,testvar,  main="Confidence intervals", percent=TRUE,  ci=TRUE, # compute AUC (of AUC by default)
#  print.auc=TRUE) # print the AUC (will contain the CI) 
#  ciobj <- ci.se(rocobj, # CI of sensitivity 
#  plot(ciobj, type="shape", col="#1c61b6AA") # plot as a blue shape 
#  specificities=seq(0, 100, 5)) # over a select set of specificities  
#  plot(ci(rocobj, of="thresholds", thresholds="best")) # add one threshold
#
#
#rocobj <- plot.roc(refvar,testvar, percent = TRUE, main="Smoothing")  
#lines(smooth(rocobj), (default: binormal)  col = "#1c61b6")  
#lines(smooth(rocobj, method = "density"), col = "#008600")  
#lines(smooth(rocobj, method = "fitdistr", density = "lognormal"), col = "#840000")  
#legend("bottomright", legend = c("Empirical", "Binormal", "Density", "Fitdistr\n(Log-normal)"), col = c("black", "#1c61b6", "#008600", "#840000"),lwd = 2)
#  
#plot.roc(refvar,testvar,  main="Confidence interval of a threshold", percent=TRUE,  ci=TRUE, of="thresholds", thresholds="best",   print.thres="best") #
#
#plot.roc(refvarm,testvarm,  main="Confidence interval of a threshold, Males", percent=TRUE,  ci=TRUE, of="thresholds", thresholds="best",   print.thres="best") 
#plot.roc(refvarf,testvarf,  main="Confidence interval of a threshold, Females", percent=TRUE,  ci=TRUE, of="thresholds", thresholds="best",   print.thres="best") #
#
#rocobj1 <- plot.roc(refvarm,testvarm,  main="Statistical comparison", percent=TRUE, col="#1c61b6")   #males
#rocobj2 <- lines.roc(refvarf,testvarf, percent=TRUE, col="#008600")  #females
#testobj <- roc.test(rocobj1, rocobj2)  
#text(50, 50, labels=paste("p-value =", format.pval(testobj$p.value)), adj=c(0, .5))  
#legend("bottomright", legend=c("Males", "Females"), col=c("#1c61b6", "#008600"), lwd=2)

