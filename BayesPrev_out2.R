for(j in 1:nrow(groupstuff)) {
  cat(Sectitle[[j]])
  print(plota[[j]])
  cat(roctitle[[j]])
  cat(plotatext[[j]])
  print(xtable(bigtable[[j]],caption=bigtabletitle[[j]], align=c("l",rep("p{1in}",13))), caption.placement="bottom", comment=FALSE, include.rownames=FALSE, size="tiny", type="html")
  cat(bling[[j]]) 
  cat(bayestext[[j]])
  print(plotb[[j]])
  cat(plotbtitle[[j]])
}

#library(gridExtra)
#pdf("Outfiles/combinedplots_b.pdf", onefile = TRUE)
#grid.arrange(plotb[[1]],plotb[[2]],plotb[[3]],plotb[[4]],plotb[[5]],plotb[[6]],ncol=2)
#dev.off()

