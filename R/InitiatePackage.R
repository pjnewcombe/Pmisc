#' This function sets up a package (initated in github and RStudio as described below) as follows:
#' - Copies in basic function templates (with roxygen comments).
#' - Copies in `TextDescription.txt', `TextOverview.txt' and `TextTitle.txt' templates to the root.
#' - Creates descritpion and -package.rd files using the Pmisc functions.
#' - Runs roxygen to generate function documentation.
#' - Sets up github if a username is provided:
#'    - Creates github README.rd file
#'    - Adds the github repository url with 'git remote add origin'
#'    - Performs the first commit.
#'- Generates a `REFRESH' file tailored with the package name, and places
#' in the package root directory.
#' To setup a blank project in github and RStudio, to run this function on:
#' 1) Go to your github account and create a new repository with
#' the same name as the package you want to initiate. NOTE: Must
#' be empty.
#' 2) Open RStudio.
#' 3) File -> New Project -> New Directory -> R Package
#'    Check `Create a git repository for this project.'
#' 
#' @export
#' @title Initiate package with roxygen templates, github link and refresh file.
#' @name InitiatePackage
#' @param package.name The name of the package
#' @param package.location Directory all R packages are stored in.
#' @param github.username Github account username. Set to NULL to skip linking to a
#' github repository of the same name.
#' @param version Version number which today's date will be appended to. Default is 0.1
#' 
InitiatePackage <- function(
  package.name,
  package.location="/Users/pauln/Dropbox/Work Projects/R Packages",
  github.username="pjnewcombe",
  version = 0.1,
  reconnection = FALSE
  ) {
  
  ### --- Load roxygen and set working directory
  library(roxygen2)
  setwd(file.path(package.location,package.name))
  
  ### --- Check this is a new package by looking for the read-and-deleta and the default file created by RStudio.
  read.and.delete.file <- file.path(package.location, package.name, "Read-and-delete-me")
  auto.created.file <- file.path(package.location, package.name, "R", paste(package.name,"R",sep="."))
  if ( !file.exists(read.and.delete.file) | !file.exists(auto.created.file) ) {
    stop("This package is not newly created by RStudio. This function should only be applied with new
         packages, otherwise it may write over work.")
  }
  
  ### --- Delete these files (especially important to delete the latter since, being empty, it prevents installation).
  try(system( paste("rm '",
                    file.path(package.location, package.name, "Read-and-delete-me"),
                    "'",sep="") ), silent=T)
  try(system( paste("rm '",
                    file.path(package.location, package.name, "R", paste(package.name,"R",sep=".")),
                    "'",sep="") ), silent=T)
    
  cat("\n-----------------------------------------------")
  cat("\n--- Copying over example function templates ---")
  cat("\n-----------------------------------------------\n")  
  pmisc.root <- path.package("Pmisc")
  system( paste(
    "cp '",pmisc.root,"/PackageInitiation/Roxygen/'* '",
    file.path(package.location,package.name,"R"),"'",
    sep="") )
  system( paste(
    "cp -R '",pmisc.root,"/PackageInitiation/Examples' '",
    file.path(package.location,package.name),"'",
    sep="") )
    
  cat("\n--------------------------------------------------")
  cat("\n--- Initiate DESCRIPTION and -package.rd files ---")
  cat("\n--------------------------------------------------\n")  
  system( paste(
    "cp '",pmisc.root,"/PackageInitiation/REFRESH/TextTitle.txt' '",
    file.path(package.location,package.name),"'",
    sep="") )
  system( paste(
    "cp '",pmisc.root,"/PackageInitiation/REFRESH/TextDescription.txt' '",
    file.path(package.location,package.name),"'",
    sep="") )
  system( paste(
    "cp '",pmisc.root,"/PackageInitiation/REFRESH/TextOverview.txt' '",
    file.path(package.location,package.name),"'",
    sep="") )
  WriteDescription(package.name, package.location, version)
  WritePackageRd(package.name, package.location, version)
  
  cat("\n--------------------------")
  cat("\n--- Running Roxygen... ---")
  cat("\n--------------------------\n")
  roxygenise(package.dir=file.path(package.location,package.name))
  
  cat("\n------------------------")
  cat("\n--- Re-installing... ---")
  cat("\n------------------------\n")
  system(command=paste("R CMD INSTALL '",file.path(package.location,package.name),"'",sep=""))
  
  cat("\n-----------------------------------------------")
  cat("\n--- Setting up github ---")
  cat("\n-----------------------------------------------\n")  
  if (!is.null(github.username)) {
    LinkToGithubRepository(
      package.name=package.name,
      package.location=package.location,
      github.username=github.username,
      reconnection=reconnection)
  }  
  
  cat("\n----------------------------------")
  cat("\n--- Generating REFRESH file... ---")
  cat("\n----------------------------------\n")
  system( paste(
    "cp '",pmisc.root,"/PackageInitiation/REFRESH/REFRESH.R' '",
    file.path(package.location,package.name),"/REFRESH_",package.name,".R'",
    sep="") )
  fConn <- file(paste("REFRESH_",package.name,".R",sep=""), 'r+') 
  Lines <- readLines(fConn) 
  writeLines(paste("package.name <- '",package.name,"'",sep=""), con = fConn) 
  writeLines(paste("package.location <- '",package.location,"'",sep=""), con = fConn) 
  writeLines(paste("github.username <-'",github.username,"'",sep=""), con = fConn)
  writeLines(Lines, con = fConn) 
  close(fConn)
  
  cat("\n-------------")
  cat("\n--- Done. ---")
  cat("\n-------------")  
}
