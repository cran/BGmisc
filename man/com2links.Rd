% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeLinks.R
\name{com2links}
\alias{com2links}
\title{Convert Sparse Relationship Matrices to Kinship Links}
\usage{
com2links(
  rel_pairs_file = "dataRelatedPairs.csv",
  ad_ped_matrix = NULL,
  mit_ped_matrix = mt_ped_matrix,
  mt_ped_matrix = NULL,
  cn_ped_matrix = NULL,
  write_buffer_size = 1000,
  update_rate = 1000,
  gc = TRUE,
  writetodisk = TRUE,
  verbose = FALSE,
  legacy = FALSE,
  outcome_name = "data",
  drop_upper_triangular = TRUE,
  include_all_links_1ped = FALSE,
  ...
)
}
\arguments{
\item{rel_pairs_file}{File path to write related pairs to (CSV format).}

\item{ad_ped_matrix}{Matrix of additive genetic relatedness coefficients.}

\item{mit_ped_matrix}{Matrix of mitochondrial relatedness coefficients. Alias: \code{mt_ped_matrix}.}

\item{mt_ped_matrix}{Matrix of mitochondrial relatedness coefficients.}

\item{cn_ped_matrix}{Matrix of common nuclear relatedness coefficients.}

\item{write_buffer_size}{Number of related pairs to write to disk at a time.}

\item{update_rate}{Numeric. Frequency (in iterations) at which progress messages are printed.}

\item{gc}{Logical. If TRUE, performs garbage collection via \code{\link{gc}} to free memory.}

\item{writetodisk}{Logical. If TRUE, writes the related pairs to disk; if FALSE, returns a data frame.}

\item{verbose}{Logical. If TRUE, prints progress messages.}

\item{legacy}{Logical. If TRUE, uses the legacy branch of the function.}

\item{outcome_name}{Character string representing the outcome name (used in file naming).}

\item{drop_upper_triangular}{Logical. If TRUE, drops the upper triangular portion of the matrix.}

\item{include_all_links_1ped}{Logical. If TRUE, includes all links in the output. (Default is true when only one ped is provided)}

\item{...}{Additional arguments to be passed to \code{\link{com2links}}}
}
\value{
A data frame of related pairs if \code{writetodisk} is FALSE; otherwise, writes the results to disk.
}
\description{
This function processes one or more sparse relationship components (additive, mitochondrial,
and common nuclear) and converts them into kinship link pairs. The resulting related pairs are
either returned as a data frame or written to disk in CSV format.
}
