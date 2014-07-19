##### Simulates case and controls given 
# Given distribution in controls, figures out frequencyies of combs in cases, then multinomial simila
# make use of marker names on control.population.unique.combs

RetroBinMultiSim <- function(
	control.population.unique.combs, # nComb x V matrix of unique control.population.unique.combs. V + 1 column is the risk from each combination
	logor.combs, # 1 x nComb vector of combination log-ORs with respect to baseline (i.e. one of these is 0)
	control.population.comb.fqs, # 1 x nComb vector of combination frequencies (either in general pop if prospective = 1, or in controls)
	n.cont, # number of controls to simulate
	n.case, # number of cases to simulate
	long.form # returns data in long form, rather than short form
) {

	nComb <- nrow(control.population.unique.combs)
	V <- ncol(control.population.unique.combs)
	variable.names <- colnames(control.population.unique.combs)	
	n <- c(n.cont,n.case)
	## Calculate genotype - specific disease probs
	ccFqs <- matrix(0,2,nComb) 					# Control/Case combination probs
	ccDraws <- matrix(0,nComb,2) 				# Control/Case combination draws
	colnames(ccDraws) <- c("d0","d1")

	#########################################################
	### GENERATE CASE AND CONTROL COMBINATION FREQUENCIES ###
	#########################################################

	# applies Seaman's formula to relate control frequencies to case frequencies- requires genotype logor.combss
	denom <- sum(control.population.comb.fqs*exp(logor.combs))
	for (c in 1:nComb) {
		ccFqs[1,c] <- control.population.comb.fqs[c]			# control control.population.comb.fqs are given 
		ccFqs[2,c] <- control.population.comb.fqs[c]*exp(logor.combs[c])/denom			# control control.population.comb.fqs are given 	
	}		
	## Normalise case and control frequencies (only actually necessary for cases if Seaman's formula used), and simulate data
	for (d in 1:2) {
		ccFqs[d,] <- ccFqs[d,]/sum(ccFqs[d,])	
		ccDraws[,paste("d",(d-1),sep="")] <- rmultinom(1, n[d], ccFqs[d,])	# Multinomially picks combs for cases and controls
	}
	control.population.unique.combs <- cbind(control.population.unique.combs,ccDraws)

	###############################
	##### EXPAND TO LONG FORM #####
	###############################

	if (long.form == 1) {
		data <- matrix(0,sum(n),V)							# A row per individual
		d <- c(rep(0,n.cont),rep(1,n.case))					# This vector will contains the disease statuses
	
		row <- 1
		combsCont <- control.population.unique.combs[control.population.unique.combs[,"d0"]>0, ]	# Only combinations with observed controls are selected
		nCombCont <- nrow(combsCont)
		for (c in 1:nCombCont) {
			for (i in 1:combsCont[c,"d0"]) {
				for (v in 1:V) {
					data[row,v] <- combsCont[c,v]				
				}
				row <- row + 1
			}	
		}
		combsCase <- control.population.unique.combs[control.population.unique.combs[,"d1"]>0, ]	# Only combinations with observed controls are selected
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
	colnames(combs) <- c(variable.names,"Disease")	
	return(combs)
}
