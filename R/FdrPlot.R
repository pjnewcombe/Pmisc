#' Diagnostic plot of impact of including increasing number of permutations
#' @export
#' @title Diagnostic plot of FDR information
#' @name FdrPlot
#' @param fdr.info A list of FDR information generated with the function FdrInf
#' @param fdrs.to.plot A vector of FDRs to plot diagnostics for
#' @param target.fdr.cols A named vector of colours for each FDR line in the plot
#' @author Paul Newcombe
FdrPlot <- function(
  fdr.info,
  fdrs.to.plot=NULL,
  fdr.cols=c("0.01"="blue", "0.05"="green", "0.1"="red")
  ) {
  
  plot(
    NULL,
    xlab="No. permutations used to calculate threshold",
    ylab=paste("No. predictors selected by threshold"),
    type="n",
    xlim=c(1,fdr.info$n.permute),
    ylim=c(
      floor( min(fdr.info$vars.selected.uptoperm.i)/10 )*10,
      ceiling( max(fdr.info$vars.selected.uptoperm.i)/10 )*10
    )
  )
  
  if ( is.null(fdrs.to.plot) ) {
    fdrs.to.plot <- fdr.info$target.fdrs
  }
  for (fdr in fdrs.to.plot) {
    points(
      x=c(1:fdr.info$n.permute),
      y=fdr.info$vars.selected.uptoperm.i[paste(fdr),],
      type="l",
      col=fdr.cols[paste(fdr)]
    )
  }
  legend(
    x="topleft",
    legend=paste("FDR", format(fdrs.to.plot)),
    col=fdr.cols[paste(fdrs.to.plot)],
    lty=1
  )    
}

