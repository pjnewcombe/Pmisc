#' @include Functions.R
NULL

#' This implements the FDR calculation developed by Shah and Samworth for Complementary Pairs Stability Selection
#' @export
#' @title Estimate FDR thresholds based on Complementary Pairs Stability Selection
#' @name CpssFdrInfo
#' @param cpss.selections A matrix of logical/binary indicators, or 0-1 probabilties if averaged over multiple imputation
#' chains, of selection of variables across the complementary pairs subset analyses. Rows are variables, columns are subset analyses
#' @param predictors Which subset of predictors to perform the FDR calculation over. Default NULL means all will be
#' considered.
#' @param target.fdrs A vector of FDR levels to calculate posterior probability thresholds for. Default NULL
#' @param selection.thresholds Alternatively, a vector of selection probability thresholds, for which to calculate the FDR for,
#' can be provided. Default NULL
#' @param q Optional estimate of q - the true number of "signal" variables. If left NULL the sum of CPSS selection probabilities
#' is used as a plug-in estimate (assuming the lambda's are cross-validated)
#' @param fdr whether or not to return FDRs rather than "p-value" type measure
#' 
#' @return fdr.info A list containing the target FDR rates, info for each including the posterior probability
#' threhsold and the estimated FDR rate at that threshold across the permutations. The list of covariates reaching
#' each threshold is also included
#' @author Paul Newcombe
CpssFdrInfo <- function(
  cpss.selections,
  predictors=NULL,
  target.fdrs=NULL,
  selection.thresholds=NULL,
  q=NULL,
  fdr=TRUE
  ) {
  
  if (!is.null(predictors)) {
    predictors.rows <- which(rownames(cpss.selections)%in%predictors)
  } else {
    predictors.rows <- c(1:nrow(cpss.selections))
  }
  
  B <- ncol(cpss.selections)/2
  p <- length(predictors.rows)
  
  pi_hat_B <- apply(cpss.selections[predictors.rows,], MAR=1, mean)
  if (is.null(q)) {
    q_hat <- sum(pi_hat_B) # as per 3.4.3 since we selected lambda using cross-validation within each fold    
  } else {
    q_hat <- q
  }
  theta_hat <- q_hat/p
  tau <- seq( 1:(2*B) )/(2*B)  # I verified this is how the taus are generated using the tables in the paper
  tau.fdr <- .minD(theta_hat, B)
  if(fdr) {
    # Multiply by q_hat and divide by number of selected vars at each threshold
    tau.fdr <- tau.fdr*p
    selec.probs <- apply(cpss.selections,MAR=1,function(x)sum(x)/length(x))
    for (i in 1:length(tau.fdr) ) {
      tau.fdr[i] <- tau.fdr[i]/sum(selec.probs>=tau[i])
    }    
  }
  fdr.info <- cbind(tau, tau.fdr)
  
  # Obtain FDR thresholds
  if (!is.null(target.fdrs)) {
    fdr.thresholds <- matrix(NA,length(target.fdrs),3,
                             dimnames=list(paste(target.fdrs),c("FDR", "CpssThreshold", "FDR_hat")))
    for (fdr in target.fdrs) {
      fdr.diff <- fdr - tau.fdr
      cut.index <- which(fdr.diff==min(fdr.diff[fdr.diff>=0]))
      if (length(cut.index)>1) {
        cut.index <- cut.index[1]
      }
      fdr.thresholds[paste(fdr),"FDR"] <- fdr
      fdr.thresholds[paste(fdr),"CpssThreshold"] <- tau[cut.index]
      fdr.thresholds[paste(fdr),"FDR_hat"] <- tau.fdr[cut.index]
    }    
  } else if (!is.null(selection.thresholds)) {
    fdr.thresholds <- matrix(NA,length(selection.thresholds),2,
                             dimnames=list(paste(selection.thresholds),c("CpssThreshold", "FDR_hat")))
    for (cut in selection.thresholds) {
      cut.index <- which(abs(tau-cut)==min(abs(tau-cut)))
      fdr.thresholds[paste(cut),"CpssThreshold"] <- cut
      fdr.thresholds[paste(cut),"FDR_hat"] <- tau.fdr[cut.index]
    }
  }
  
  # Return info
  fdr.info <- list(
    "target.fdrs"=target.fdrs,
    "selection.thresholds"=selection.thresholds,
    "fdr.info"=fdr.thresholds,
    "B"=B,
    "theta_hat"=theta_hat,
    "pi_hat_B"=pi_hat_B,
    "cpss.selections"=cpss.selections)
  for (fdr in target.fdrs) {
    fdr.info[[paste(fdr)]] <- names(
      which(pi_hat_B>=fdr.thresholds[paste(fdr),"CpssThreshold"])
    )
  }
  
  return(fdr.info)
}
