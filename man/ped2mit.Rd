% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convertPedigree.R
\name{ped2mit}
\alias{ped2mit}
\alias{ped2mt}
\title{Take a pedigree and turn it into a mitochondrial relatedness matrix}
\usage{
ped2mit(
  ped,
  max.gen = 25,
  sparse = FALSE,
  verbose = FALSE,
  gc = FALSE,
  flatten.diag = FALSE,
  standardize.colnames = TRUE,
  tcross.alt.crossprod = FALSE,
  tcross.alt.star = FALSE
)
}
\arguments{
\item{ped}{a pedigree dataset.  Needs ID, momID, and dadID columns}

\item{max.gen}{the maximum number of generations to compute
(e.g., only up to 4th degree relatives).  The default is 25. However it can be set to infinity.
 `Inf` uses as many generations as there are in the data.}

\item{sparse}{logical.  If TRUE, use and return sparse matrices from Matrix package}

\item{verbose}{logical.  If TRUE, print progress through stages of algorithm}

\item{gc}{logical. If TRUE, do frequent garbage collection via \code{\link{gc}} to save memory}

\item{flatten.diag}{logical. If TRUE, overwrite the diagonal of the final relatedness matrix with ones}

\item{standardize.colnames}{logical. If TRUE, standardize the column names of the pedigree dataset}

\item{tcross.alt.crossprod}{logical. If TRUE, use alternative method of using Crossprod function for computing the transpose}

\item{tcross.alt.star}{logical. If TRUE, use alternative method of using \%\*\% for computing the transpose}
}
\description{
Take a pedigree and turn it into a mitochondrial relatedness matrix
}
\details{
The algorithms and methodologies used in this function are further discussed and exemplified in the vignette titled "examplePedigreeFunctions". For more advanced scenarios and detailed explanations, consult this vignette.
}
