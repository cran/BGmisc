% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convertPedigree.R
\name{ped2mit}
\alias{ped2mit}
\alias{ped2mt}
\title{Take a pedigree and turn it into a mitochondrial relatedness matrix}
\usage{
ped2mit(
  ped,
  max.gen = Inf,
  sparse = FALSE,
  verbose = FALSE,
  gc = FALSE,
  flatten.diag = FALSE
)
}
\arguments{
\item{ped}{a pedigree dataset.  Needs ID, momID, and dadID columns}

\item{max.gen}{the maximum number of generations to compute
(e.g., only up to 4th degree relatives).  The default of Inf uses as many
generations as there are in the data.}

\item{sparse}{logical.  If TRUE, use and return sparse matrices from Matrix package}

\item{verbose}{logical.  If TRUE, print progress through stages of algorithm}

\item{gc}{logical. If TRUE, do frequent garbage collection via \code{\link{gc}} to save memory}

\item{flatten.diag}{logical. If TRUE, overwrite the diagonal of the final relatedness matrix with ones}
}
\description{
Take a pedigree and turn it into a mitochondrial relatedness matrix
}
\details{
The algorithms and methodologies used in this function are further discussed and exemplified in the vignette titled "examplePedigreeFunctions".
}
