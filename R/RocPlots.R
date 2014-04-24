#' Given a list of predictive scores, and a list of corresponding "true" outcomes, this function
#' calculates various predictive performance measures. Note that this function can be used in
#' the context of multiple imputation (if the argument n.mi.chains is used), in which case the
#' performance measures are calculated within each chain, then averaged.
#' 
#' @export
#' @title Takes a list of objects returned by PredictivePerformance.
#' @name RocPlots
#' 
#' @param cvperfs A named list of objects from the PredictivePerformance function
#' @param main Main plot title (default NULL)
#' @param fpr.stop An optional maximum false positive rate (x-axis) to calculate truncated
#' ROC curves
#' @param lwd Line width of ROC curves (default 1)
#' @param roc.cols Optional named vector of colours to use for the different ROC curves
#' 
#' @author Paul Newcombe

RocPlots <- function(
  cvperfs,
  main.title=NULL,
  fpr.stop=1,
  tpr.stop=1,
  lwd=2,
  roc.cols=NULL,
  include.legend=TRUE,
  include.percentage.diffs=FALSE,
  avg.type="threshold"
  ) {
  require(ROCR)
  analysis.names <- names(cvperfs) 
  
  if (is.null(roc.cols)) {
    if (length(analysis.names)==2) {
      roc.cols <- c("blue","red")
    } else {
      roc.cols <- rainbow(length(cvperfs))
    }
    names(roc.cols) <- analysis.names    
  }
  
  aucs <- rep(NA,length(analysis.names))
  names(aucs) <- analysis.names
  auc.percentage.diffs <- aucs
  formatted.aucs <- aucs
  formatted.auc.percentage.diffs <- aucs
  for (m in analysis.names) {
    plot(cvperfs[[m]]$roc,
         avg=avg.type,
         col = roc.cols[m],
         add = (which(names(cvperfs)==m)!=1),
         lwd = lwd,
         xlim=c(0,fpr.stop),
         ylim=c(0,tpr.stop))
    aucs[m] <-  mean(unlist(performance(cvperfs[[m]]$rocr,"auc", fpr.stop=fpr.stop)@y.values))*(1/fpr.stop)
    formatted.aucs[m] <- sprintf("%.2f", aucs[m])
  }
  abline(a=0,b=1, lty=2, col="darkgrey")
  
  # Calulate percentage differences  
  baseline.auc <- analysis.names[1]
  for (m in analysis.names[analysis.names!=baseline.auc]) {
    auc.percentage.diffs[m] <- (aucs[m]-aucs[baseline.auc])/aucs[baseline.auc]
    if (auc.percentage.diffs[m]>=0) {sign <- "+"} else {sign <- "-"}
    formatted.auc.percentage.diffs[m] <- paste("(",sign,sprintf("%2.0f", 100*auc.percentage.diffs[m]),"%)",sep="")    
    if (include.percentage.diffs) {
      formatted.aucs[m] <- paste(formatted.aucs[m], formatted.auc.percentage.diffs[m])
    }
  }
  
  leg.pos <- "bottomright"
  if (fpr.stop<1) {
    leg.pos <- "topleft"
  }
  if (include.legend) {
    legend(
      leg.pos,
      paste(formatted.aucs, analysis.names ),
      col=roc.cols,
      lwd=lwd,
      lty=1
    )    
  }
  
  title(main=main.title)
}
