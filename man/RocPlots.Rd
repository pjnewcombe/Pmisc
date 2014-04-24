\name{RocPlots}
\alias{RocPlots}
\title{Takes a list of objects returned by PredictivePerformance.}
\usage{
  RocPlots(cvperfs, main.title = NULL, fpr.stop = 1,
    tpr.stop = 1, lwd = 2, roc.cols = NULL,
    include.legend = TRUE,
    include.percentage.diffs = FALSE,
    avg.type = "threshold")
}
\arguments{
  \item{cvperfs}{A named list of objects from the
  PredictivePerformance function}

  \item{main}{Main plot title (default NULL)}

  \item{fpr.stop}{An optional maximum false positive rate
  (x-axis) to calculate truncated ROC curves}

  \item{lwd}{Line width of ROC curves (default 1)}

  \item{roc.cols}{Optional named vector of colours to use
  for the different ROC curves}
}
\description{
  Given a list of predictive scores, and a list of
  corresponding "true" outcomes, this function calculates
  various predictive performance measures. Note that this
  function can be used in the context of multiple
  imputation (if the argument n.mi.chains is used), in
  which case the performance measures are calculated within
  each chain, then averaged.
}
\author{
  Paul Newcombe
}

