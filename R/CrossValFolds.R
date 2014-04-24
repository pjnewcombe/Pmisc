#' Writes a data file in the format required for Java MCMC program
#' @export
#' @title Partitions data for K-fold cross validation. Returns a list of row numbers to
#' exclude in each of the K analyses (and use as validation data).
#' @name CrossValFolds
#' @param n Number of subjects in complete dataset to partition - use if do not care about
#' balanced folds (leave balance.var set to NULL)
#' @param balance.var A vectory containing a binary 0/1 outcome variable, which you wish the
#' folds to be balanced with respsect to (in which case leave n=NULL)
#' @param K number of folds to partition subjects into. Default is 10.
#' @param folds.seed Random number seed for shuffling up subjects between folds for
#' alternative partitions (default seed is 1)
#' @return A list with K elements. The kth element is a vector of rows to exclude from
#' the kth fold analysis, and use as validation data
#' @author Paul Newcombe
CrossValFolds <- function(
  n=NULL,
  balance.var=NULL,
  K=10,
  folds.seed=1) {
  if (!is.null(n)) {
    ids <- c(1:n)
  }
  if (!is.null(balance.var)) {
    n <- length(balance.var)
    all.ids <- c(1:n)
    ids <- all.ids[balance.var==0]
    ids2 <- all.ids[balance.var==1]
  }
  set.seed(folds.seed)
  
  # Randomise ids and determine partitions
  ids <- sample(x=ids, size=length(ids), replace=FALSE)
  divides <- ceiling(seq(from=1, to=(length(ids)+1), length.out=K+1))
  if (!is.null(ids2)) {
    ids2 <- sample(x=ids2, size=length(ids2), replace=FALSE)
    divides2 <- ceiling(seq(from=1, to=(length(ids2)+1), length.out=K+1))    
  }
  
  # Split partitioned IDs in a list
  folds.list <- list()
  for (k in 1:K) {
    folds.list[[k]] <- ids[divides[k]:(divides[k+1]-1)]
    # Append other IDs if going for a balanced case/control partition
    if (!is.null(ids2)) {
      folds.list[[k]] <- c(
        folds.list[[k]],
        ids2[divides2[k]:(divides2[k+1]-1)]
      )
    }
  }
  
  # Return folds list
  return(folds.list)
}
