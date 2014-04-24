\name{CrossValFolds}
\alias{CrossValFolds}
\title{Partitions data for K-fold cross validation. Returns a list of row numbers to
exclude in each of the K analyses (and use as validation data).}
\usage{
  CrossValFolds(n = NULL, balance.var = NULL, K = 10,
    folds.seed = 1)
}
\arguments{
  \item{n}{Number of subjects in complete dataset to
  partition - use if do not care about balanced folds
  (leave balance.var set to NULL)}

  \item{balance.var}{A vectory containing a binary 0/1
  outcome variable, which you wish the folds to be balanced
  with respsect to (in which case leave n=NULL)}

  \item{K}{number of folds to partition subjects into.
  Default is 10.}

  \item{folds.seed}{Random number seed for shuffling up
  subjects between folds for alternative partitions
  (default seed is 1)}
}
\value{
  A list with K elements. The kth element is a vector of
  rows to exclude from the kth fold analysis, and use as
  validation data
}
\description{
  Writes a data file in the format required for Java MCMC
  program
}
\author{
  Paul Newcombe
}

