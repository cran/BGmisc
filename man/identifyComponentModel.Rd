% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/identification.R
\name{identifyComponentModel}
\alias{identifyComponentModel}
\title{Determine if a variance components model is identified}
\usage{
identifyComponentModel(..., silent = FALSE)
}
\arguments{
\item{...}{Comma-separated relatedness component matrices.}

\item{silent}{logical. Whether to print messages about identification.}
}
\description{
Determine if a variance components model is identified
}
\details{
Returns of list of length 2. The first element is a single logical value:
TRUE if the model is identified, FALSE otherwise. The second list element
is the vector of non-identified parameters.  For instance, a model might
have 5 components with 3 of them identified and 2 of them not.  The second
list element will give the names of the components that are not
simultaneously identified.
}
\examples{

identifyComponentModel(A=list(matrix(1, 2, 2)), C=list(matrix(1, 2, 2)), E= diag(1, 2))


}
