\name{PredictivePerformance}
\alias{PredictivePerformance}
\title{Calculates various predictive performance measures.}
\usage{
  PredictivePerformance(predictions, outcomes)
}
\arguments{
  \item{predictions}{A list of predictions for each cross
  validation fold. If multiple imputation was used this
  should be a list of lists, the first level corresponding
  to multiple imputation chains, and the second level to
  cross validation folds. I.e. predictions[[1]][[2]] would
  contain predictions from the 1st multiple imputation
  chain, for patients in the 2nd cross-validation fold.}

  \item{outcomes}{A list of "true" binary outcomes for each
  cross validation fold. Note the patients ordering must be
  identical to that in predictions above.}
}
\value{
  A list containing various measures of predictive
  performance: rocr - the ROCR base object roc - ROCR
  object containing ROC curve information ppvTpr - ROCR
  object containing positive predictive value vs Trupe
  positive rate information auc - The average ROC auc over
  all folds (and, if relevant, MI chains) obs.risk.qs.e -
  Average observed risk in the predicted risk quantiles
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

