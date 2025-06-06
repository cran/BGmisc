% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimateCIs.R
\name{calculateCIs}
\alias{calculateCIs}
\title{Calculate Confidence Intervals for Correlation Coefficients}
\usage{
calculateCIs(
  tbl,
  rho_var,
  se_var,
  doubleentered = FALSE,
  method = "raykov",
  adjust_base = 1,
  design_effect_m = NULL,
  design_effect_rho = NULL,
  design_effect_m_col = NULL,
  design_effect_rho_col = NULL,
  conf_level = 0.95
)
}
\arguments{
\item{tbl}{A data frame or tibble containing the correlation coefficient and standard error variables.}

\item{rho_var}{The name of the column in \code{tbl} that contains the correlation coefficients.}

\item{se_var}{The name of the column in \code{tbl} that contains the standard errors.}

\item{doubleentered}{Logical. If \code{TRUE}, the function assumes that the correlation coefficients are double-entered, which adjusts the standard errors accordingly. Default is \code{FALSE}.}

\item{method}{The method to use for calculating the confidence intervals. Options are "raykov", "fisherz", "doubleenteredconserv", or "doubleentered".}

\item{adjust_base}{A numeric value to adjust the standard errors. Default is 1.}

\item{design_effect_m}{A numeric value for the design effect related to the mean. Default is \code{NULL}.}

\item{design_effect_rho}{A numeric value for the design effect related to the correlation. Default is \code{NULL}.}

\item{design_effect_m_col}{A character string specifying the column name for the design effect related to the mean. Default is \code{NULL}.}

\item{design_effect_rho_col}{A character string specifying the column name for the design effect related to the correlation. Default is \code{NULL}.}

\item{conf_level}{The confidence level for the intervals. Default is 0.95.}
}
\value{
A modified version of \code{tbl} with additional columns for the confidence intervals and related statistics.
Everything uses adjusted standard errors, including confidence intervals, z-tests, and p-values.
}
\description{
This function calculates confidence intervals for correlation coefficients using different methods.
}
\examples{
tbl <- data.frame(rho = c(0.5, 0.7, 0.3), se = c(0.1, 0.2, 0.05))
calculateCIs(tbl, rho_var = "rho", se_var = "se", method = "raykov")

}
