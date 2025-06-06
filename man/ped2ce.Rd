% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buildComponent.R
\name{ped2ce}
\alias{ped2ce}
\title{Take a pedigree and turn it into an extended environmental relatedness matrix}
\usage{
ped2ce(ped, ...)
}
\arguments{
\item{ped}{a pedigree dataset.  Needs ID, momID, and dadID columns}

\item{...}{additional arguments to be passed to \code{\link{ped2com}}}
}
\description{
Take a pedigree and turn it into an extended environmental relatedness matrix
}
\details{
The algorithms and methodologies used in this function are further discussed and exemplified in the vignette titled "examplePedigreeFunctions". For more advanced scenarios and detailed explanations, consult this vignette.
}
