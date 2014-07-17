\name{WritePackageRd}
\alias{WritePackageRd}
\title{Automates writing of the package overview Rd file.}
\usage{
  WritePackageRd(package.name, title = NULL,
    description = title, overview = description,
    version = 0.1)
}
\arguments{
  \item{package.name}{The name of the package}

  \item{title}{Text for the title field}

  \item{description}{Text for the description field; if
  none is given the title is used.}

  \item{description}{Overview of the package, most
  important functions etc. If none is given the description
  is used.}

  \item{version}{Version number which today's date will be
  appended to. Default is 0.1}
}
\description{
  Automates writing of the -package.Rd file, which contains
  basic info about the package. The version number has
  today's date appended. NOTE: The working directory must
  be set to the package directory before this function is
  invoked.
}

