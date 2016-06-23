#' Runs univariate and multivariate analyses for a given set of covariates
#' 
#' @export
#' @title Formats predictions into a ROCR `object' to plot using the RocPlots function.
#' @name UniMultivariateComparison
#' @param outcome Outcome variable
#' @param covariates Covariates to calculate univariate and multivariate effects for
#' @param confounders List of confounders to include in univariate and multivariate models
#' @param data Matrix or datafram containing the variables above.
#' @param pretty Whether to retrun a prettyified table (which contains strings so might be less useful).
#' @param model "logistic" or "linear"
#' 
#' @author Paul Newcombe

UniMultivariateComparison <- function(
  outcome,
  covariates,
  confounders=NULL,
  data,
  pretty=F,
  model="linear"
  ) {
  
  ##############################
  # --- Univariate results --- #
  ##############################
  
  uni.res <- list()
  for (v in covariates) {
    model.form <- paste(outcome,"~",v)
    if (!is.null(confounders)) {
      model.form <- paste(model.form,"+",paste(confounders, collapse="+"))
    }
    if (model=="logistic") {
      uni.res[[v]] <- glm(formula(model.form), as.data.frame(data), family=binomial)
    } else if (model=="linear") {
      uni.res[[v]] <- lm(formula(model.form), as.data.frame(data) )      
    }
    cat("Univariate regression for",v,"complete\n")
  }
  
  ################################
  # --- Multivariate results --- #
  ################################
  
  model.form <- paste(outcome,"~",paste(covariates, collapse="+"))
  if (!is.null(confounders)) {
    model.form <- paste(model.form,"+",paste(confounders, collapse="+"))
  }
  if (model=="logistic") {
    multi.res <- glm(formula(model.form), as.data.frame(data), family=binomial)
  } else if (model == "linear") {
    multi.res <- lm(formula(model.form), as.data.frame(data) )    
  } 
  cat("Multivariate regression complete\n")
  
  ########################
  # -- Results table --- #
  ########################
  .ExtractRes <- function(res, v) {
    if (length(v)==1) {
      rows <- t(as.matrix(summary(res)$coefficients[v,]))
    } else {
      rows <- summary(res)$coefficients[v,]      
    }
    res.tab.row <- cbind(
      rows[,1],
      rows[,1]-1.9641*rows[,2],
      rows[,1]+1.9641*rows[,2],
      rows[,4]
    )
    if (model=="logistic") {
      res.tab.row[,1:3] <- exp(res.tab.row[,1:3])
    }
    return(res.tab.row)
  }

  cols <- c(
    "Uni_Est","Uni_Lower","Uni_Upper","Uni_p",
    "Multi_Est","Multi_Lower","Multi_Upper","Multi_p")
  res.tab <- matrix(NA, length(covariates), length(cols), dimnames=list(covariates, cols))
  for (v in covariates) {
    res.tab[v,grep("Uni",cols)] <- .ExtractRes(uni.res[[v]],v)
  }
  res.tab[covariates,grep("Multi",cols)] <- .ExtractRes(multi.res,covariates)
  
  if (pretty) {
    cols.pretty <- c(
      "Uni_Est","Uni_95CI","Uni_p",
      "Multi_Est","Multi_95CI","Multi_p")
    res.tab.pretty <- matrix(NA, length(covariates), length(cols.pretty), dimnames=list(covariates, cols.pretty))
    res.tab.pretty[,"Uni_Est"] <- sprintf("%.2f",  res.tab[,"Uni_Est"])
    res.tab.pretty[,"Uni_95CI"] <-
      paste("(", sprintf("%.2f",  res.tab[,"Uni_Lower"]), ",_" ,
            sprintf("%.2f",  res.tab[,"Uni_Upper"]),")", sep=""
      )
    res.tab.pretty[,"Uni_p"] <- signif(res.tab[,"Uni_p"],3)
    res.tab.pretty[,"Multi_Est"] <- sprintf("%.2f",  res.tab[,"Multi_Est"])
    res.tab.pretty[,"Multi_95CI"] <-
      paste("(", sprintf("%.2f",  res.tab[,"Multi_Lower"]), ",_" ,
            sprintf("%.2f",  res.tab[,"Multi_Upper"]),")", sep=""
      )
    res.tab.pretty[,"Multi_p"] <- signif(res.tab[,"Multi_p"],3)
    res.tab <- res.tab.pretty
  }
  
  return(res.tab)
}
