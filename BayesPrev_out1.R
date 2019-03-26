library(xtable)
library(ggplot2)

cat(knit_expand(text = "<br><h4>Descriptives and Summary across different Tests and Reference Outcomes:</h4><p>"))

tab2IMD<-firststats_out$tab2IMD

print(xtable(firststats_out$dfcomb,caption="<br><b>Table: Sample characteristics: summary by Gender</b>"),comment=FALSE, include.rownames=FALSE, size="tiny", type="html")

cat(knit_expand(text="<br><br><br>"))

print(xtable(firststats_out$IMDqint,caption="<br><b>Table: Study sample breakdown by Lower Layer Super Output Area IMD Quintiles for England (2010)</b>"),comment=FALSE, include.rownames=FALSE, size="tiny", type="html")

cat(knit_expand(text="<br><br><br>"))

print(firststats_out$IMDplot)

cat(knit_expand(text="<br><center><b>Figure: Boxplots of median, min and max IMD score by Study Practice; with LLSOA Quintles for England</center></b><br><br>"))

cat(knit_expand(text="<br><br><br>"))

print(sites, "chart")

cat(knit_expand(text="<br><center><b>Figure: Google Map Plot of Sample Geography, based on postcode sectors</center></b><br><br>"))

cat(knit_expand(text="<br><br><br>"))

print(firststats_out$corrplot[[1]])

cat(knit_expand(text="<br><center><b>Figure: Pearson correlations between Reference Outcomes and Total 90 day consumption</center></b><br><br>"))

cat(knit_expand(text="<br><br><br>"))

print(firststats_out$corrplot[[2]])

cat(knit_expand(text="<br><center><b>Figure: Pearson correlations between Reference Outcomes and Number of Heavy Drinking Days (out of 90 days)</center></b><br><br>"))

cat(knit_expand(text="<br><br><br>"))

cat(knit_expand(text="<br><br><br>"))

print(xtable(firststats_out$dfORs,caption="<br><b>Table: ICC by Practice (N=14) and GLM Odds Ratos for Reference Outcomes by Index of Multiple Deprivation score, Gender, Age Group and Smoking Status</b>"),comment=FALSE, include.rownames=FALSE, size="tiny", type="html")

cat(knit_expand(text="<br><br><br>"))

print(xtable(firststats_out$df2_ORs,caption="<br><b>Table: ICC by Practice (N=14) and HLM Odds Ratos for Reference Outcomes by Index of Multiple Deprivation (IMD) score, Gender, Age Group and Smoking Status</b>"),comment=FALSE, include.rownames=FALSE, size="tiny", type="html")

cat(knit_expand(text="<br><br><br>"))

print(xtable(firststats_out$df.betas,caption="<br><b>Table: ICC by Practice (N=14) and GLMM beta coefficients for other TLFB Outcomes by Index of Multiple Deprivation (IMD) score, Gender, Age Group and Smoking Status</b>"),comment=FALSE, include.rownames=FALSE, size="tiny", type="html")

cat(knit_expand(text="<br><br><br>"))


df <- data.frame(t(sapply(sum_out,c)))
df <- cbind(unlist(sum_row),df)
colnames(df) <- c("Reference Outcome, by Test Index", "Youden J threshold", "Jw; weighted for Sensitivity", "Jw; weighted for Specificity")
df1 <- df[seq(1, nrow(df), by=2),]
df2 <- df[seq(2, nrow(df), by=2),]
summarytitle1 <- paste("<b>Table: ",testlabels[1]," test score thresholds (cut-points) for different reference outcomes<br><br></b>", sep="")
summarytitle2 <- paste("<b>Table: ",testlabels[2]," test score thresholds (cut-points) for different reference outcomes<br><br></b>", sep="")

print(xtable(df1,caption=summarytitle1, align=c("l", "p{3in}",rep("p{0.5in}",3))), caption.placement="bottom", comment=FALSE, include.rownames=FALSE, size="tiny", type="html")

cat(knit_expand(text="<br><br><br>"))

print(xtable(df2,caption=summarytitle2, align=c("l", "p{3in}",rep("p{0.5in}",3))), caption.placement="bottom", comment=FALSE, include.rownames=FALSE, size="tiny", type="html")


cat(knit_expand(text="<br><br><br>"))


