#' Automates writing of the -package.Rd file, which contains basic info about the
#' package. The version number has today's date appended. Takes descriptive 
#' information from the files TextTitle.txt, TextDescription.txt, and
#' TextOverview.txt which are stored in the root of the package folder.
#' 
#' @export
#' @title Automates writing of the package overview Rd file.
#' @name WritePackageRd
#' @param package.name The name of the package
#' @param package.location Directory all R packages are stored in.
#' @param version Version number which today's date will be appended to. Default is 0.1
#' 
WritePackageRd <- function(
  package.name,
  package.location="/Users/pauln/Dropbox/Work Projects/R Packages",
  version = 0.1
  ) {
  
  ### --- Read in info
  title <- scan(file.path(package.location,package.name,"TextTitle.txt"), what="character", sep="\t")
  description <- scan(file.path(package.location,package.name,"TextDescription.txt"), what="character", sep="\t")
  overview <- scan(file.path(package.location,package.name,"TextOverview.txt"), what="character", sep="\t")
  
  rd.file <- paste(
    file.path(package.location,package.name),
    "/man/",package.name,"-package.Rd",sep="")
  
  ### --- Basic info
  write(paste("\\name{",package.name,"-package}",sep=""), rd.file)
  write(paste("\\alias{",package.name,"-package}",sep=""), rd.file, append=T)
  write(paste("\\alias{",package.name,"}",sep=""), rd.file, append=T)
  write("\\docType{package}", rd.file, append=T)
  write(paste("\\title{",title,"}",sep=""), rd.file, append=T)
  write(paste("\\description{",description,"}",sep=""), rd.file, append=T)
  
  ### --- Details
  write("\\details{", rd.file, append=T)
  write("\\tabular{ll}{", rd.file, append=T)
  write(paste("  Package: \\tab ",package.name,"\\cr",sep=""), rd.file, append=T)
  write("  Type: \\tab Package\\cr", rd.file, append=T)
  write(paste("  Version: \\tab ",paste(version,format(Sys.time(), "%d-%m-%Y"),sep="-"),"\\cr",sep=""), rd.file, append=T)
  write("  License: \\tab GPL (>=2)\\cr }", rd.file, append=T)
  write(overview, rd.file, append=T)
  write("}", rd.file, append=T)
  
  ### --- Author
  write("\\author{Paul Newcombe \\email{paul.newcombe@mrc-bsu.cam.ac.uk}}", rd.file, append=T)
}
