#' Automates writing of the -package.Rd file, which contains basic info about the
#' package. The version number has today's date appended. NOTE: The working 
#' directory must be set to the package directory before this
#' function is invoked.
#' 
#' @export
#' @title Automates writing of the package overview Rd file.
#' @name WritePackageRd
#' @param package.name The name of the package
#' @param title Text for the title field
#' @param description Text for the description field; if none is given the title is used.
#' @param description Overview of the package, most important functions etc. If none is
#' given the description is used.
#' @param version Version number which today's date will be appended to. Default is 0.1
#' 
WritePackageRd <- function(
  package.name,
  title = NULL,
  description = title,
  overview = description,
  version = 0.1
  ) {
  
  rd.file <- paste("man/",package.name,"-package.Rd",sep="")
  
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
