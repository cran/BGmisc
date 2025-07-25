% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkParents.R
\name{checkParentIDs}
\alias{checkParentIDs}
\title{Validates and Optionally Repairs Parent IDs in a Pedigree Dataframe}
\usage{
checkParentIDs(
  ped,
  verbose = FALSE,
  repair = FALSE,
  repairsex = repair,
  addphantoms = repair,
  parentswithoutrow = repair,
  famID = "famID",
  personID = "ID",
  momID = "momID",
  dadID = "dadID"
)
}
\arguments{
\item{ped}{A dataframe representing the pedigree data with columns 'ID', 'dadID', and 'momID'.}

\item{verbose}{A logical flag indicating whether to print progress and validation messages to the console.}

\item{repair}{A logical flag indicating whether to attempt repairs on missing parent IDs.}

\item{repairsex}{A logical flag indicating whether to attempt repairs on sex of the parents}

\item{addphantoms}{A logical flag indicating whether to add phantom parents for missing parent IDs.}

\item{parentswithoutrow}{A logical flag indicating whether to add parents without a row in the pedigree.}

\item{famID}{Character. Column name for family IDs.}

\item{personID}{Character. Column name for individual IDs.}

\item{momID}{Character. Column name for maternal IDs.}

\item{dadID}{Character. Column name for paternal IDs.}
}
\value{
Depending on the value of `repair`, either a list containing validation results or a repaired dataframe is returned.
}
\description{
This function takes a pedigree object and performs two main tasks:
1. Checks for the validity of parent IDs, specifically looking for instances where only one parent ID is missing.
2. Optionally repairs the missing parent IDs based on a specified logic.
}
\examples{
\dontrun{
ped <- data.frame(ID = 1:4, dadID = c(NA, 1, 1, 2), momID = c(NA, NA, 2, 2))
checkParentIDs(ped, verbose = TRUE, repair = FALSE)
}
}
