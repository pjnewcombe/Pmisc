#' Formats predictions into a ROCR `object' to plot using the RocPlots function.
#' 
#' @export
#' @title Formats predictions into a ROCR `object' to plot using the RocPlots function.
#' @name FormatRocrObjects
#' @param predictions.list A named list of predictions for each different analysis/model,
#' for each person/variable, and for each fold/replicate. First level is the analysis/model
#' names. Second level can be matrices (rows people/vars, cols folds/replicates) or a list
#' where each element corresponds to a different fold/replicate, and contains a vector of
#' predictions across people/replicates.
#' @param true.outcomes The true outcomes of disease/variable causality. Can be a matrix
#' (rows people/vars, cols folds/replicates) or a list where each element corresponds to
#' a different fold/replicate, and contains a vector of truth indicators. NOTE: These are
#' assumed to be the same across analyses.
#' @param fpr.stop Maximum x to go up to on the x-axis for a partial ROC
#' 
#' @author Paul Newcombe

FormatRocrObjects <- function(
  predictions.list,
  true.outcomes,
  fpr.stop=1
  ) {
  require(ROCR)
  
  # Analysis names
  if (is.null(names(predictions.list))) {
    names(predictions.list) <- paste(c(1:length(predictions.list)))    
  }
  analysis.names <- names(predictions.list) 
  
  # Convert to lists
  for (a in analysis.names) {
    if (! is.list(predictions.list[[a]]) ) {
      predictions.list[[a]] <- lapply(1:ncol(predictions.list[[a]]),function(x) predictions.list[[a]][,x])      
    }
  }
  if (! is.list(true.outcomes) ) {
    true.outcomes <- lapply(1:ncol(true.outcomes),function(x) true.outcomes[,x])      
  }
  
  rocs <- list()
  for (a in analysis.names) {
    rocs[[a]] <- list()
    rocs[[a]]$rocr <- prediction(predictions.list[[a]], true.outcomes)
    rocs[[a]]$roc <- performance(rocs[[a]]$rocr,"tpr","fpr",fpr.stop=fpr.stop)
    rocs[[a]]$ppvTpr <- performance(rocs[[a]]$rocr,"ppv","tpr")
    rocs[[a]]$aucs.reps <- unlist(performance(rocs[[a]]$rocr,"auc",fpr.stop=fpr.stop)@y.values)
    rocs[[a]]$auc <- mean(rocs[[a]]$aucs.reps)    
  }
  
  return(rocs)
}
