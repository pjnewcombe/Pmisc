library(roxygen2)
package.name <- 'Pmisc'
package.location <- '/Users/paul/Dropbox/Work Projects/R Packages'

####################################
# --- Write `DESCRIPTION' file --- #
####################################
write.dcf( 
  x = list(
    Package = package.name, 
    Type = "Package",
    Title = "A collection of miscellaneous functions.",
    Version = paste("0.1",format(Sys.time(), "%d-%m-%Y"),sep="-"),
    Date=format(Sys.time(), "%d-%m-%Y"),
    Author = "Paul J Newcombe <paul.newcombe@mrc-bsu.cam.ac.uk>",
    Maintainer = "Paul J Newcombe <paul.newcombe@mrc-bsu.cam.ac.uk>",
    Description = "A collection of miscellaneous functions.",
    License = "GPL (>=2)"),
  file = file.path(package.location,package.name,"DESCRIPTION"),
  append = FALSE
)

#####################################################
# --- Run Roxygen to generate NAMESPACE and Rds --- #
#####################################################
roxygenise(package.dir=file.path(package.location,package.name),clean=TRUE)

######################
# --- Re-install --- #
######################
system(command=paste("R CMD INSTALL '",file.path(package.location,package.name),"'",sep=""))
