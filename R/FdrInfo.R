#' @include Functions.R
NULL

#' Given observed posterior probabilties, and those calculated under permutations of the outcome labels, this
#' function calculates posterior probability thresholds for different target false discovery rates
#' @export
#' @title Estimate FDR thresholds based on permuted outcome label analyses
#' @name FdrInfo
#' @param observed.post.probs A vector of posterior probabilities from the observed (i.e. `real') data analysis. Note - MUST
#' be named. Either this must be provided, or the results object below.
#' @param results Java MCMC results class object. Either this must be provided, or the vector of posterior probabilities above.
#' @param permuted.post.probs A matrix of posterior probabilities from permuted analyses - rows are variables and columns are 
#' the different permuted outcome analyses. Either this must be provided, or the list of results files below.
#' @param perm.res.files A list of results files for all the permuted outcomes analyses with identifier "_perm1" included
#' in the file name for, e.g., the first permutation. If multiple imputation was carried out the results file
#' corresponding to each imputation is included in the list. Either this must be provided, or the matrix of posterior
#' probabilities from each permuted label analysis (see above).
#' @param predictors Which subset of predictors to perform the FDR calculation over. Default NULL means all will be
#' considered.
#' @param target.fdrs A vector of FDR levels to calculate posterior probability thresholds for. Default is 0.01,
#' 0.05, 0.1
#' @param n.cuts.order Order of magnitude for vector length of possible thresholds to explore
#' @param diagnostic.plot.info Whether to generate information for a diagnostic plot (involves repeating
#' calulcations adding each permutation one by one) which can be time consuming. Default it TRUE.
#' 
#' @return fdr.info A list containing the target FDR rates, info for each including the posterior probability
#' threhsold and the estimated FDR rate at that threshold across the permutations. The list of covariates reaching
#' each threshold is also included
#' @author Paul Newcombe
FdrInfo <- function(
  observed.post.probs=NULL,
  results=NULL,
  permuted.post.probs=NULL,
  perm.res.files=NULL,
  predictors=NULL,
  target.fdrs=c(0.01, 0.05, 0.1),
  n.cuts.order=5,
  diagnostic.plot.info=TRUE
  ) {
  
  if ("n.mi.chains" %in% names(results) ) {
    m.i <- results$n.mi.chains
  } else {
    m.i <- 1
  }
  
  # Posterior probabilities from actual analysis (extracted from results object if not provided)
  if (is.null(observed.post.probs)) {
    obs.tab <- ResultsTable(results)
    if (!is.null(predictors)) {
      predictors.rows <- which(rownames(obs.tab)%in%predictors)
    } else {
      predictors.rows <- which(!is.na(obs.tab[,"BF"]))
    }
    observed.post.probs <- obs.tab[predictors.rows,"PostProb"]
    names(observed.post.probs) <- rownames(obs.tab[predictors.rows,])
  }
  
  # Posterior probabilities from permuted analyses
  if (is.null(permuted.post.probs)) {
    n.permute <- length(perm.res.files[grep("_perm", perm.res.files)])/m.i # Incase includes non-permuted results
    for (seed in 1:n.permute) {
      if (m.i > 1) {
        res.p <- ReadResults(
          mi.results.files=perm.res.files[grep(paste("_perm",seed,"_",sep=""),perm.res.files)]
        )      
      } else {
        res.p <- ReadResults(
          results.file=perm.res.files[grep(paste("_perm",seed,"_",sep=""),perm.res.files)]
        )      
      }
      res.tab.p <- ResultsTable(res.p)
      permuted.post.probs <- cbind(permuted.post.probs, res.tab.p[predictors.rows,"PostProb"])
      cat(paste("Results read for permutation",seed,"\n"))
    }    
  } else {
    n.permute <- ncol(permuted.post.probs)
  }
  
  # Obtain FDR thresholds
  fdr.thresholds <- .GetFdrThresholds(
    obs.probs=observed.post.probs,
    permuted.probs=permuted.post.probs,
    target.fdrs=target.fdrs,
    n.cuts.order=n.cuts.order
    )
  
  # List of information to return
  fdr.info <- list(
    "target.fdrs"=target.fdrs,
    "fdr.info"=fdr.thresholds,
    "n.permute"=n.permute,
    "obs.probs"=observed.post.probs,
    "permuted.probs"=permuted.post.probs)
  
  # Diagnostic plot of sensitivity to addtional permutations
  if (diagnostic.plot.info) {
    fdr.info$vars.selected.uptoperm.i <- NULL
    for (i in 1:n.permute) {
      thresholds.i <- .GetFdrThresholds(
        obs.probs=observed.post.probs,
        permuted.probs=permuted.post.probs[,c(1:i)],
        target.fdrs=target.fdrs,
        n.cuts.order=n.cuts.order
      )
      for (fdr in target.fdrs) {
        thresh <- thresholds.i[paste(fdr),"PostProbThreshold"]
        thresholds.i[paste(fdr),"PostProbThreshold"] <- sum(observed.post.probs>=thresh)
      }
      fdr.info$vars.selected.uptoperm.i <- cbind(fdr.info$vars.selected.uptoperm.i, thresholds.i[,"PostProbThreshold"])      
    }    
  }
  
  # Return info
  for (fdr in target.fdrs) {
    fdr.info[[paste(fdr)]] <- names(
      which(observed.post.probs>=fdr.thresholds[paste(fdr),"PostProbThreshold"])
    )
  }
  
  return(fdr.info)
}

