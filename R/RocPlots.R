#' Given a list of predictive scores, and a list of corresponding "true" outcomes, this function
#' calculates various predictive performance measures. Note that this function can be used in
#' the context of multiple imputation (if the argument n.mi.chains is used), in which case the
#' performance measures are calculated within each chain then averaged.
#' 
#' @export
#' @title Takes a list of objects returned by the ROCR function PredictivePerformance.
#' @name RocPlots
#' 
#' @param rocr.list A named list of objects created using the ROCR function PredictivePerformance,
#' or by using the Pmisc function FormatRocrObjects
#' @param main Main plot title (default NULL)
#' @param fpr.stop An optional maximum false positive rate (x-axis) to calculate truncated
#' ROC curves
#' @param lwd Line width of ROC curves (default 1)
#' @param roc.cols Optional named vector of colours to use for the different ROC curves
#' @param legend.name.map A character vector of different names to display for the analyses in the legend.
#' The elements are named with the current analysis names.
#' @param legend.cex cex argument to pass to legend - controls overall size.
#' @param legend.text.width text.width argument to pass to legend - controls width in terms of x scale.
#' @example Examples/RocPlots_Example.R
#' 
#' @author Paul Newcombe

RocPlots <- function(
  rocr.list,
  main.title=NULL,
  fpr.stop=1,
  tpr.stop=1,
  lwd=1,
  roc.cols=NULL,
  include.legend=TRUE,
  include.percentage.diffs=FALSE,
  avg.type="threshold",
  legend.name.map=NULL,
  legend.cex=1.2,
  legend.text.width=0.2
  ) {
  require(ROCR)
  if (is.null(names(rocr.list))) {
    names(rocr.list) <- paste(c(1:length(rocr.list)))    
  }
  analysis.names <- names(rocr.list) 
  
  if (is.null(roc.cols)) {
    if (length(analysis.names)==2) {
      roc.cols <- c("blue","red")
    } else {
      roc.cols <- rainbow(length(rocr.list))
    }
    names(roc.cols) <- analysis.names    
  }
  
  aucs <- rep(NA,length(analysis.names))
  names(aucs) <- analysis.names
  auc.percentage.diffs <- aucs
  formatted.aucs <- aucs
  formatted.auc.percentage.diffs <- aucs
  for (m in analysis.names) {
    plot(rocr.list[[m]]$roc,
         avg=avg.type,
         col = roc.cols[m],
         add = (which(names(rocr.list)==m)!=1),
         lwd = lwd,
         xlim=c(0,fpr.stop),
         ylim=c(0,tpr.stop))
    aucs[m] <-  mean(unlist(performance(rocr.list[[m]]$rocr,"auc", fpr.stop=fpr.stop)@y.values))*(1/fpr.stop)
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
    if (!is.null(legend.name.map)) {
      analysis.names <- legend.name.map[analysis.names]
    }
    legend(
      leg.pos,
      paste(formatted.aucs, analysis.names),
      col=roc.cols,
      lwd=lwd,
      lty=1,
      cex=legend.cex,
      text.width=legend.text.width
    )    
  }
  
  title(main=main.title)
}
