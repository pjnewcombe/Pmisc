#' Calculates prior probabability of causality for a particular variable, when a Poisson prior is used for model space
#'
#' @title Prior probability of a particular variable
#' @name ModelSpaceSpecProb
#' @param V total number of variables
#' @param mu model space mean
#' @return prior probability for a single variable
#' @author Paul Newcombe
ModelSpaceSpecProb <- function(V, mu) {
	priorProb <- 0
	### Normalising constant for truncated Poisson
	poiDenom <- 0
	for (v in 0:V) {	
		poiDenom <- poiDenom + (mu^v)*exp(-mu)/gamma(v+1)
	}
	## m - 1 places to fill (saince 1st place taken by SNP in question), from M-1 SNPs (SNP in question, that is in 1st place, is not an option)
	for (v in 1:V) {	
		vProb <- (mu^v)*exp(-mu)/(gamma(v+1)*poiDenom)
		priorProb <- priorProb + vProb*choose( (V-1) , (v-1) )/choose( V , v )		
	}
	return(priorProb)
}

#' Calculates prior probababilities for x, and >=x causal variables, when a truncated Poisson prior is used for model space
#'
#' @title prior probability for x causal variables
#' @name ModelSpaceProbs
#' @param V total number of variables
#' @param mu model space mean
#' @return prior matrix whereby 1st row is prior probabilities for =x causal variables, and 2nd row for >=x variables (x = 1,...V)
#' @author Paul Newcombe
ModelSpaceProbs <- function(V, mu) {
	# Normalising constant for truncated Poisson
	poiDenom <- 0
	for (v in 0:V) {	
		poiDenom <- poiDenom + (mu^v)*exp(-mu)/gamma(v+1)
	}
	
	# =x
	numPresPriorProb <- c(1:(V+1))
	for (v in 1:V) {
		# Prior prob of  >= m SNPs for m = 1,..M
		numPresPriorProb[v] <- (mu^v)*exp(-mu)/(gamma(v+1)*poiDenom)
	}
	numPresPriorProb[V+1] <- exp(-mu)/(gamma(1)*poiDenom)
	
	# >=x
	geqPriorProb <- c(1:(V+1))
	for (v in 1:V) {
		geqPriorProb[v] <- sum(numPresPriorProb[v:V]) 
	}	
	geqPriorProb[V+1] <- sum(numPresPriorProb[0:V])
	
	# combining
	return.mat <- rbind(numPresPriorProb,geqPriorProb)
	colnames(return.mat) <- paste(c(c(1:V),0),sep="")
	rownames(return.mat) <- c("=",">=")
	
	return(return.mat)
}

#' Calculates a Bayes Facotor, give a prior and posterior probabiltiies
#'
#' @title Bayes Factor
#' @name BayesFactor
#' @param prior.prob prior probability
#' @param post.prob posterior probability
#' @return Bayes Factor
#' @author Paul Newcombe
BayesFactor <- function(prior.prob, post.prob) {
	priorOdds <- prior.prob/(1-prior.prob)
	postOdds <- post.prob/(1-post.prob)
	bayesFactor <- postOdds/priorOdds
	return(bayesFactor)
}
