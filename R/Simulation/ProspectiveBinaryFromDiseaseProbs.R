##### Simulates case and controls given 

SimulateBinaryPhenotype <- function(
	V,	# num variables
	nComb, # no. unique genotypes
	combs, # nComb x V matrix of unique combs. V + 1 column is the risk from each combination
	logOrComb = NULL, # 1 x nComb vector of combination log-ORs with respect to baseline (i.e. one of these is 0)
	prospective = 0, # 1 if prospective study combination probabilties are population level, 0 if are for controls
	fq, # 1 x nComb vector of combination frequencies (either in general pop if prospective = 1, or in controls)
	probDisease = NULL, # if prospective = 1, this is probability of disease for each combination (worked out from baseline prevalence and ORs)
	nCont, # number of controls to simulate
	nCase, # number of cases to simulate
	longForm # returns data in long form, rather than short form
) {

	n <- c(nCont,nCase)
	## Calculate genotype - specific disease probs
	ccFqs <- matrix(0,2,nComb) 					# Control/Case combination probs
	ccDraws <- matrix(0,nComb,2) 				# Control/Case combination draws
	colnames(ccDraws) <- c("d0","d1")

	#########################################################
	### GENERATE CASE AND CONTROL COMBINATION FREQUENCIES ###
	#########################################################

	if (prospective == 1) {
		# If population level frequencies are provided, works out case and control probs according to baseline prevalence - requires genotype diseaseProbs
		for (c in 1:nComb) {
			probDiseaseVec <- c((1-probDisease[c] ), probDisease[c])
			for (d in 1:2) {
				ccFqs[d,c] <- fq[c] * probDiseaseVec[d]
			}
		}
	} else {	
		# applies Seaman's formula to relate control frequencies to case frequencies- requires genotype logOrCombs
		denom <- sum(fq*exp(logOrComb))
		for (c in 1:nComb) {
			ccFqs[1,c] <- fq[c]			# control fqs are given 
			ccFqs[2,c] <- fq[c]*exp(logOrComb[c])/denom			# control fqs are given 	
		}		
	}


	## Normalise case and control frequencies (only actually necessary for cases if Seaman's formula used), and simulate data
	for (d in 1:2) {
		ccFqs[d,] <- ccFqs[d,]/sum(ccFqs[d,])	
		ccDraws[,paste("d",(d-1),sep="")] <- rmultinom(1, n[d], ccFqs[d,])
	}

	combs <- cbind(combs,ccDraws)

	############################################
	##### EXPAND TO LONG FORM ??? - YES!!! #####
	############################################

	if (longForm == 1) {
		data <- matrix(0,sum(n),V)							# A row per individual
		d <- c(rep(0,nCont),rep(1,nCase))					# This vector will contains the disease statuses
	
		row <- 1
		combsCont <- combs[combs[,"d0"]>0, ]	# Only combinations with observed controls are selected
		nCombCont <- nrow(combsCont)
		for (c in 1:nCombCont) {
			for (i in 1:combsCont[c,"d0"]) {
				for (v in 1:V) {
					data[row,v] <- combsCont[c,v]				
				}
				row <- row + 1
			}	
		}
		combsCase <- combs[combs[,"d1"]>0, ]	# Only combinations with observed controls are selected
		nCombCase <- nrow(combsCase)
		for (c in 1:nCombCase) {
			for (i in 1:combsCase[c,"d1"]) {
				for (v in 1:V) {
					data[row,v] <- combsCase[c,v]				
				}
				row <- row + 1
			}	
		}	
	
		combs <- cbind(data,d)								# set combs as long data	
	
	}
	colnames(combs) <- c(paste("V",c(1:V),sep=""),"Disease")	
	return(combs)
}