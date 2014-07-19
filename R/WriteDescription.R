#' Automates writing of the description file for packages. The version number has today's date
#' appended. Takes descriptive information from the files TextTitle.txt, TextDescription.txt,
#' and TextOverview.txt which are stored in the root of the package folder.
#' 
#' @export
#' @title Automates writing of the description file
#' @name WriteDescription
#' @param package.name The name of the package
#' @param package.location Directory all R packages are stored in.
#' @param version Version number which today's date will be appended to. Default is 0.1
#' 
WriteDescription <- function(
  package.name,
  package.location="/Users/pauln/Dropbox/Work Projects/R Packages",
  version = 0.1
  ) {
  ### --- Read in info
  title <- scan(file.path(package.location,package.name,"TextTitle.txt"), what="character", sep="\t")
  description <- scan(file.path(package.location,package.name,"TextDescription.txt"), what="character", sep="\t")
  overview <- scan(file.path(package.location,package.name,"TextOverview.txt"), what="character", sep="\t")
  
  # Write info to the file `DESCRIPTION', using base R's write.dcf
  write.dcf( list(
    Package = package.name, Type = "Package",
    Title = title,
    Version = paste(version,format(Sys.time(), "%d-%m-%Y"),sep="-"),
    Date=format(Sys.time(), "%d-%m-%Y"),
    Author = "Paul J Newcombe <paul.newcombe@mrc-bsu.cam.ac.uk>",
    Maintainer = "Paul J Newcombe <paul.newcombe@mrc-bsu.cam.ac.uk>",
    Description = description,
    License = "GPL (>=2)"),
    file = file.path(package.location,package.name,"DESCRIPTION")
    )
}
