#' Automates writing of the description file for packages. The version number has today's date
#' appended. NOTE: The working directory must be set to the package directory before this
#' function is invoked.
#' 
#' @export
#' @title Automates writing of the description file
#' @name WriteDescription
#' @param package.name The name of the package
#' @param title Text for the title field
#' @param description Text for the description field; if none is given the title is used.
#' @param version Version number which today's date will be appended to. Default is 0.1
#' 
WriteDescription <- function(
  package.name,
  title = NULL,
  description = title,
  version = 0.1
  ) {
  # Write info to current working directory, in the file `DESCRIPTION',
  # using base R's command write.dcf
  write.dcf( list(
    Package = package.name, Type = "Package",
    Title = title,
    Version = paste(version,format(Sys.time(), "%d-%m-%Y"),sep="-"),
    Date=format(Sys.time(), "%d-%m-%Y"),
    Author = "Paul J Newcombe <paul.newcombe@mrc-bsu.cam.ac.uk>",
    Maintainer = "Paul J Newcombe <paul.newcombe@mrc-bsu.cam.ac.uk>",
    Description = description,
    License = "GPL (>=2)"),
    file = "DESCRIPTION")  
}
