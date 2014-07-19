\name{InitiatePackage}
\alias{InitiatePackage}
\title{Initiate package with roxygen templates, github link and refresh file.}
\usage{
  InitiatePackage(package.name,
    package.location = "/Users/pauln/Dropbox/Work Projects/R Packages",
    github.username = "pjnewcombe", version = 0.1,
    reconnection = FALSE)
}
\arguments{
  \item{package.name}{The name of the package}

  \item{package.location}{Directory all R packages are
  stored in.}

  \item{github.username}{Github account username. Set to
  NULL to skip linking to a github repository of the same
  name.}

  \item{version}{Version number which today's date will be
  appended to. Default is 0.1}
}
\description{
  This function is to be run after creating a pckage as
  following:
}
\details{
  1) Go to your github account and create a new repository
  with the same name as the package you want to initiate 2)
  Open RStudio 3) File -> New Project -> New Directory -> R
  Package Check `Create a git repository for this project.'

  This function will: - Copy in a couple of basic function
  templates (with roxygen comments). - Copy over
  `TextDescription.txt', `TextOverview.txt' and
  `TextTitle.txt' templates. - Create descritpion and
  -package.rd files using the Pmisc functions. - Run
  roxygen to generate documentation. - Create a github
  README.rd file, initiate the github connection and
  perform the first commit. - Generate a `REFRESH' file
  tailored with the package name, and place in the package
  root directory.
}

