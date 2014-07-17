\name{WriteDescription}
\alias{WriteDescription}
\title{Automates writing of the description file}
\usage{
  WriteDescription(package.name, title = NULL,
    description = title, version = 0.1)
}
\arguments{
  \item{package.name}{The name of the package}

  \item{title}{Text for the title field}

  \item{description}{Text for the description field; if
  none is given the title is used.}

  \item{version}{Version number which today's date will be
  appended to. Default is 0.1}
}
\description{
  Automates writing of the description file for packages.
  The version number has today's date appended. NOTE: The
  working directory must be set to the package directory
  before this function is invoked.
}

