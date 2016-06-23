#' This function refreshes a package:
#' - Updates the description and package.rd files
#' - Runs roxygen and re-generates documentation
#' - Reinstalls
#' - Updates local git repository if a username is given
#' - Uploads changes to git if a commit.message is given
#' 
#' @export
#' @title Regenerate and reinstall a package.
#' @name RefreshPackage
#' @param package.name The name of the package
#' @param package.location Directory all R packages are stored in.
#' @param github.username Optional Github account username. If provided all files will
#' be added.
#' @param commit.message Optional github commit can be performed by providing a commit
#' message. Default is NULL.
#' be added.
#' @param version Version number which today's date will be appended to. Default is 0.1
#' 
RefreshPackage <- function(
  package.name,
  package.location="/Users/pauln/Dropbox/Work Projects/R Packages",
  github.username="pjnewcombe",
  commit.message=NULL,
  version = 0.1
  ) {
  
  ### --- Load roxygen and set working directory
  library(roxygen2)
  setwd(file.path(package.location,package.name))
  
  cat("\n--------------------------------------------------")
  cat("\n--- Updating DESCRIPTION and -package.rd files ---")
  cat("\n--------------------------------------------------\n")
  WriteDescription(package.name, package.location, version)
#  WritePackageRd(package.name, package.location, version)
    
  cat("\n--------------------------")
  cat("\n--- Running Roxygen... ---")
  cat("\n--------------------------\n")
  roxygenise(package.dir=file.path(package.location,package.name),clean=TRUE)
  
  cat("\n------------------------")
  cat("\n--- Re-installing... ---")
  cat("\n------------------------\n")
  system(command=paste("R CMD INSTALL '",file.path(package.location,package.name),"'",sep=""))
  
  if (!is.null(github.username)) {
    cat("\n--------------------------")
    cat("\n--- Updating Github... ---")
    cat("\n--------------------------\n")
    try(system("git add DESCRIPTION"), silent=T)
    try(system("git add NAMESPACE"), silent=T)
    try(system("git add -A R"), silent=T)
    try(system("git add -A man"), silent=T)
    try(system("git add -A Examples"), silent=T)
    try(system("git add -A inst"), silent=T)
  }
  if (!is.null(commit.message)) {
    cat("\n------------------------------------------------")
    cat("\n--- Making a commit and syncing to Github... ---")
    cat("\n------------------------------------------------")
    system( paste("git commit -m '",commit.message,"'",sep="") )    
    system("git push -u origin master")
  }
  
  cat("\n-------------")
  cat("\n--- Done. ---")
  cat("\n-------------")
}
