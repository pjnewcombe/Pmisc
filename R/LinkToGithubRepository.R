#' This function links a package to a github repository of the same name. Test Mod
#' 
#' @export
#' @title Link to a github repository of the same name.
#' @name LinkToGithubRepository
#' @param package.name The name of the package
#' @param package.location Directory all R packages are stored in.
#' @param github.username Github account username. Set to NULL to skip linking to a
#' github repository of the same name.
#' 
LinkToGithubRepository <- function(
  package.name,
  package.location="/Users/pauln/Dropbox/Work Projects/R Packages",
  github.username="pjnewcombe",
  reconnection = FALSE
  ) {
  
  setwd(file.path(package.location,package.name))
  
  ####################################
  ### --- Setup link to Github --- ###
  ####################################
  
  system(paste("git remote add origin https://github.com/",github.username,"/",package.name,".git",sep=""))
  if (reconnection) {
    system("git pull -u origin master")    
  } else {
    ### --- Make git readme file
    description.text <- scan(file.path(package.location,package.name,"TextDescription.txt"), what="character", sep="\t")
    write(file="README.md", package.name)
    write(file="README.md", paste(c(rep("=",nchar(package.name)),"\n"), collapse=""), append=T)
    write(file="README.md", paste(description.text,"\n"), append=T)
    
    ### --- Make first git commit of the github readme file
    system("git add README.md")
    system("git add DESCRIPTION")
    system("git add NAMESPACE")
    system("git commit -m 'First commit.' ")  
  }
  system("git push -u origin master")
}
