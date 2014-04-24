#' Given a list of predictive scores, and a list of corresponding "true" outcomes, this function
#' calculates various predictive performance measures. Note that this function can be used in
#' the context of multiple imputation (if the argument n.mi.chains is used), in which case the
#' performance measures are calculated within each chain, then averaged.
#' 
#' @export
#' @title Calculates various predictive performance measures.
#' @name PredictivePerformance
#' 
#' @param predictions A list of predictions for each cross validation fold. If multiple
#' imputation was used this should be a list of lists, the first level corresponding to
#' multiple imputation chains, and the second level to cross validation folds. I.e. 
#' predictions[[1]][[2]] would contain predictions from the 1st multiple imputation chain,
#' for patients in the 2nd cross-validation fold.
#' @param outcomes A list of "true" binary outcomes for each cross validation fold. Note
#' the patients ordering must be identical to that in predictions above.
#' 
#' @return A list containing various measures of predictive performance:
#' rocr - the ROCR base object
#' roc - ROCR object containing ROC curve information
#' ppvTpr - ROCR object containing positive predictive value vs Trupe positive rate information
#' auc - The average ROC auc over all folds (and, if relevant, MI chains)
#' obs.risk.qs.e - Average observed risk in the predicted risk quantiles
#' @author Paul Newcombe

PredictivePerformance <- function(
  predictions,
  outcomes
  ) {
  require(ROCR)
  
  K <- length(outcomes)
  # Establish is multiple imputation has been performed
  if (is.list(predictions[[1]])) {
    n.mi.chains <- length(predictions)
  } else {
    n.mi.chains <- NULL
  }
  
  ### --- In the context of multiple imputation, combine into one long list
  if (!is.null(n.mi.chains)) {
    predictions.orig <- predictions
    outcomes.orig <- outcomes
    predictions <- list()
    outcomes <- list()
    index.counter <- 1
    for (m in c(1:n.mi.chains) ) {
      for (k in 1:K) {
        predictions[[index.counter]] <- predictions.orig[[m]][[k]]
        outcomes[[index.counter]] <- outcomes.orig[[k]]
        index.counter <- index.counter + 1
      }
    }
  }
  
  ### --- Predictive performance
  cvperf <- list()
  cvperf$K <- K  
  cvperf$n.mi.chains <- n.mi.chains  
  cvperf$rocr <- prediction(predictions, outcomes)
  cvperf$roc <- performance(cvperf$rocr,"tpr","fpr")
  cvperf$ppvTpr <- performance(cvperf$rocr,"ppv","tpr")
  cvperf$auc <- mean(unlist(performance(cvperf$rocr,"auc")@y.values))
  
  ### --- by mi chain if relevant
  if (!is.null(n.mi.chains)) {
    cvperf$aucs.chains <- c(1:n.mi.chains)
    for (m in 1:n.mi.chains) {
      inds <- c(1:K)+(m-1)*K
      rocr.chain <- prediction(predictions[inds], outcomes[inds])
      cvperf$aucs.chains[m] <- mean(unlist(performance(rocr.chain,"auc")@y.values))
    }
  }
  
  # Calibration (not ROCR)
  cvperf$obs.risk.qs.e <- matrix(NA, nrow=length(outcomes), ncol=4)
  cvperf$pred.risk.qs <- cvperf$obs.risk.qs.e
  for (i in 1:length(outcomes)) {
    risk.qs <- as.integer( cut(predictions[[i]], quantile(predictions[[i]]), ordered_results=T) )
    obs.risk.qs.e.fold <- rep(NA,4)
    pred.risk.qs.fold <- rep(NA,4)
    for (q in 1:4) {
      pred.risk.qs.fold[q] <- mean(predictions[[i]][which(risk.qs==q)])
      obs.risk.qs.e.fold[q] <- mean(outcomes[[i]][which(risk.qs==q)])
    }
    cvperf$pred.risk.qs[i,] <- pred.risk.qs.fold
    cvperf$obs.risk.qs.e[i,] <- obs.risk.qs.e.fold
  }    
  
  return(cvperf)
}
