#' This function is to be run after creating a pckage as following:
#' 
#' 1) Go to your github account and create a new repository with
#' the same name as the package you want to initiate
#' 2) Open RStudio
#' 3) File -> New Project -> New Directory -> R Package
#'    Check `Create a git repository for this project.'
#' 
#' This function will:
#' - Copy in a couple of basic function templates (with roxygen comments).
#' - Copy over `TextDescription.txt', `TextOverview.txt' and `TextTitle.txt' templates.
#' - Create descritpion and -package.rd files using the Pmisc functions.
#' - Run roxygen to generate documentation.
#' - Create a github README.rd file, initiate the github connection and
#' perform the first commit.
#' - Generate a `REFRESH' file tailored with the package name, and place
#' in the package root directory.
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
  
  library(roxygen2)
  setwd(file.path(package.location,package.name))
  ### --- Delete file created by RStudio - prevents installation
  try(system( paste("rm '",
                    file.path(package.location, package.name, "R", paste(package.name,"R",sep=".")),
                    "'",sep="") ), silent=T)
  
  
  ###########################################
  ### --- Copy over example templates --- ###
  ###########################################
  
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
  
  
  ############################
  ### --- Setup Github --- ###
  ############################

  if (!is.null(github.username)) {
    LinkToGithubRepository(package.name,package.location,github.username,reconnection)
  }  
  
  #########################################
  ### --- Generate a REFRESH.R file --- ###
  #########################################
  
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
