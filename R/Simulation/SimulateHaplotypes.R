#' Simulates genotypes for n people, given haplotypes hap
#'
#' @title Genotype simulation from haplotypes and haplotype frequencies
#' @name SimulateGenotypesFromHaplotypes
#' @param haps Haplotypes (rows are haplotypes, columns are binary markers)
#' @param hapfqs The frequencies of the haplotypes
#' @param n The number of individuals to simulate
#' @param d Disease status
#' @return Matrix of simulated genotypes
#' @author Paul Newcombe
SimulateGenotypesFromHaplotypes <- function(haps, hapfqs, n, d) {
  # Draw two sets of haplotypes (will be paired into people)
  drawn.hap1 <- rmultinom(n, 1, hapfqs)
  drawn.hap2 <- rmultinom(n, 1, hapfqs)
  # Turn haplotype pairs into genotypes
  genos <- t(drawn.hap1)%*%haps + t(drawn.hap2)%*%haps
  genos <- cbind(genos,d)
  colnames(genos) <- c(colnames(haps),"Disease")

  return(genos)
}

#' Simulates genotypes for cases and controls, based on an underlying haplotype distribution in controls, and SNP ORs
#'
#' @title Genotype simulation from cases and controls, based on control haplotypes and SNP ORs
#' @name SimulateCaseControlGenotypes_Retrospective
#' @param haps Haplotypes (rows are haplotypes, columns are binary markers)
#' @param control.hapfqs The underlying frequencies of the haplotypes in controls
#' @param log.snp.ors Vector of log-ORs for the SNPs
#' @param n.controls Number of controls to simulate
#' @param n.cases Number of cases to simulate
#' @return Matrix of simulated genotypes, including a disease vector
#' @author Paul Newcombe
SimulateCaseControlGenotypes_Retrospective <- function(haps, control.hapfqs, log.snp.ors, n.controls, n.cases) {
  # Numbers of haplotypes and markers
  H <- nrow(haps)
  M <- ncol(haps) 
  # Calculate haplotype ors
  log.hap.ors <- rep(0,H)
  for (h in 2:H) {
    for (m in 1:M) {
      log.hap.ors[h] <- log.hap.ors[h] + (haps[h,m]-haps[1,m])*log.snp.ors[m]
    }
  }
  # Calculate case haplotype frequencies
  case.hapfqs <- rep(0,H)
  denom <- sum(control.hapfqs*exp(log.hap.ors))
  for (h in 1:H) {
    case.hapfqs[h] <- control.hapfqs[h]*exp(log.hap.ors[h])/denom			# control control.haps.fqs are given 	
  }
  # Simulate data
  sim.data <- SimulateGenotypesFromHaplotypes(haps, control.hapfqs, n.controls, 0)
  sim.data <- rbind(sim.data, SimulateGenotypesFromHaplotypes(haps, case.hapfqs, n.cases, 1) )
  
  return(sim.data) 
}

##########################
### TEST DATA: ACEdata ###
##########################

library(hapsim)
data(ACEdata)
ACEdata.unique <- t(unique(t(ACEdata)))
colnames(ACEdata.unique) <- paste("SNP",c(1:ncol(ACEdata.unique)),sep="")
haps <-ACEdata.unique
control.hapfqs <- c(rep(1,nrow(ACEdata.unique)))/nrow(ACEdata.unique)
log.snp.ors <- rep(0,ncol(haps))
log.snp.ors[10] <- log(4)
data <- SimulateCaseControlGenotypes_Retrospective(haps=ACEdata.unique, control.hapfqs, log.snp.ors, 1000, 1000)

# ACEdata contains 22 haplotypes for 52 SNPs. Only 22 SNPs are unique
# Visualise LD
ldplot(ld.mat=haplodata(ACEdata.unique)$cor, ld.type="r")
# Allele fqs
1-allelefreqs(ACEdata.unique)$freqs
