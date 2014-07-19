\name{WritePackageRd}
\alias{WritePackageRd}
\title{Automates writing of the package overview Rd file.}
\usage{
  WritePackageRd(package.name,
    package.location = "/Users/pauln/Dropbox/Work Projects/R Packages",
    version = 0.1)
}
\arguments{
  \item{package.name}{The name of the package}

  \item{package.location}{Directory all R packages are
  stored in.}

  \item{version}{Version number which today's date will be
  appended to. Default is 0.1}
}
\description{
  Automates writing of the -package.Rd file, which contains
  basic info about the package. The version number has
  today's date appended. Takes descriptive information from
  the files TextTitle.txt, TextDescription.txt, and
  TextOverview.txt which are stored in the root of the
  package folder.
}

