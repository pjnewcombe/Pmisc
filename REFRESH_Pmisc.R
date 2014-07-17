### --- Libraries, name and path
library(Pmisc)
library(roxygen2)
package.name <- "Pmisc"
package.dir <- paste("~/Dropbox/Work Projects/R Packages/",package.name,sep="")
setwd(package.dir)

### -- Re-write description and main doc files with updated date-version number
description.text <- "All my miscellaneous functions."
WriteDescription(package.name, title=description.text)
WritePackageRd(package.name, title=description.text)

### --- Use Roxygenise to generate .RD files from my comments, and re-install
roxygenise(package.dir=package.dir)
system(command=paste("R CMD INSTALL '",package.dir,"'",sep=""))
