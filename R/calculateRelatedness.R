#' Calculate Relatedness Coefficient
#'
#' @description
#' This function calculates the relatedness coefficient between two individuals based on their shared ancestry, as described by Wright (1922).
#'
#' @details
#' The relatedness coefficient between two people (b & c) is defined in relation to their common ancestors:
#' \eqn{r_{bc} = \sum \left(\frac{1}{2}\right)^{n+n'+1} (1+f_a)}
#'
#' @param generations Number of generations back of common ancestors the pair share.
#' @param path Traditional method to count common ancestry, which is twice the number of generations removed from common ancestors. If not provided, it is calculated as 2*generations.
#' @param full Logical. Indicates if the kin share both parents at the common ancestor's generation. Default is TRUE.
#' @param maternal Logical. Indicates if the maternal lineage should be considered in the calculation.
#' @param empirical Logical. Adjusts the coefficient based on empirical data, using the total number of nucleotides and other parameters.
#' @param segregating Logical. Adjusts for segregating genes.
#' @param total_a Numeric. Represents the total size of the autosomal genome in terms of nucleotides, used in empirical adjustment. Default is 6800*1000000.
#' @param total_m Numeric. Represents the total size of the mitochondrial genome in terms of nucleotides, used in empirical adjustment. Default is 16500.
#' @param weight_a Numeric. Represents the weight of phenotypic influence from additive genetic variance, used in empirical adjustment.
#' @param weight_m Numeric. Represents the weight of phenotypic influence from mitochondrial effects, used in empirical adjustment.
#' @param denom_m Logical. Indicates if `total_m` and `weight_m` should be included in the denominator of the empirical adjustment calculation.
#' @param ... Further named arguments that may be passed to another function.
#'
#' @return
#' Relatedness Coefficient (`coef`): A measure of the genetic relationship between two individuals.
#'
#' @examples
#' \dontrun{
#' # For full siblings, the relatedness coefficient is expected to be 0.5:
#' calculateRelatedness(generations = 1, full = TRUE)
#' # For half siblings, the relatedness coefficient is expected to be 0.25:
#' calculateRelatedness(generations = 1, full = FALSE)
#' }
#' @export
#'
calculateRelatedness <- function(
    generations = 2, path = NULL, full = TRUE, maternal = FALSE,
    empirical = FALSE, segregating = TRUE,
    total_a = 6800 * 1000000, total_m = 16500,
    weight_a = 1, weight_m = 1, denom_m = FALSE, ...) {
  # If path is not provided, it is calculated as twice the number of generations
  if (is.null(path)) {
    path <- generations * 2
  }

  # Calculate the coefficient based on the path
  coef <- .5^path

  # If full siblings, the coefficient is doubled
  if (full == TRUE) {
    coef <- coef * 2
  }
  # If not considering segregating genes, adjust the coefficient
  if (segregating == FALSE) {
    coef <- coef * .01 + .99
  }

  # If empirical adjustment is needed
  if (empirical == TRUE) {
    denom_emp <- denom_m * total_m * weight_m + total_a * weight_a
    if (denom_emp == 0) stop("Denominator in empirical adjustment is zero.")

    coef <- (coef * total_a * weight_a + maternal * total_m * weight_m) / denom_emp
  }
  return(coef)
}


#' @rdname calculateRelatedness
#' @export
related_coef <- function(...) {
  warning("The 'related_coef' function is deprecated. Please use 'calculateRelatedness' instead.")
  calculateRelatedness(...)
}

#' Infer Relatedness Coefficient
#'
#' @description
#'
#' This function infers the relatedness coefficient between two groups based on
#' the observed correlation between their additive genetic variance and shared environmental variance.

#' @details
#' The function uses the ACE (Additive genetic, Common environmental, and Unique environmental) model to infer the relatedness between two individuals or groups.
#' By considering the observed correlation (`obsR`), the proportion of variance attributable to additive genetic variance (`aceA`), and the proportion of shared environmental variance (`aceC`), it calculates the relatedness coefficient.
#'
#' @param obsR Numeric. Observed correlation between the two groups. Must be between -1 and 1.
#' @param aceA Numeric. Proportion of variance attributable to additive genetic variance. Must be between 0 and 1. Default is 0.9.
#' @param aceC Numeric. Proportion of variance attributable to shared environmental variance. Must be between 0 and 1. Default is 0.
#' @param sharedC Numeric. Proportion of shared environment shared between the two individuals. Must be between 0 (no shared environment) and 1 (completely shared environment). Default is 0.
#' @param ... Further named arguments that may be passed to another function.
#'
#' @return
#' Numeric. The calculated relatedness coefficient (`est_r`).
#'
#' @examples
#' \dontrun{
#' # Infer the relatedness coefficient:
#' inferRelatedness(obsR = 0.5, aceA = 0.9, aceC = 0, sharedC = 0)
#' }
#' @export
inferRelatedness <- function(obsR, aceA = .9, aceC = 0, sharedC = 0) {
  if (aceA > 1 || aceA < 0 || aceC > 1 || aceC < 0) {
    stop("aceA and aceC must be proportions between 0 and 1")
  }
  calc_r <- (obsR - sharedC * aceC) / aceA
  return(calc_r)
}

#' @rdname inferRelatedness
#' @keywords internal
relatedness <- function(...) {
  warning("The 'relatedness' function is deprecated. Please use 'inferRelatedness' instead.")
  inferRelatedness(...)
}

#' Falconer's Formula
#'
#' @description
#' Use Falconer's formula to solve for H using the observed correlations for two groups of any two levels of relatednesses.
#'
#' @details
#' This generalization of Falconer's formula provides a method to calculate heritability by using the observed correlations for two groups of any two relatednesses.
#' This function solves for H using the formula:
#' \deqn{H^2 = \frac{obsR1 - obsR2}{r1 - r2}}
#' where r1 and r2 are the relatedness coefficients for the first and second group, respectively, and obsR1 and obsR2 are the observed correlations.
#'
#'
#' @param r1 Relatedness coefficient of the first group.
#' @param r2 Relatedness coefficient of the second group.
#' @param obsR1 Observed correlation between members of the first group.
#' @param obsR2 Observed correlation between members of the second group.
#'
#' @return Heritability estimates (`heritability_estimates`).

calculateH <- function(r1, r2, obsR1, obsR2) {
  # Check for equal relatedness coefficients to avoid division by zero
  if (any(r1 - r2 == 0)) {
    stop("Relatedness coefficients r1 and r2 must not be equal for any pair.")
  }
  if (any(abs(obsR1) > 1) || any(abs(obsR2) > 1)) {
    warning("The observed correlations should be between -1 and 1.")
  }

  if (any(obsR1 * obsR2 < 0)) {
    warning("The correlations should not have opposite signs.")
  }

  if (any(obsR1 < 0 & obsR2 < 0)) {
    message("Your scale might be reverse coded because you have negative correlations. Please check your data. ")
  }

  # Calculate heritability estimates (H^2) for all pairs
  heritability_estimates <- (obsR1 - obsR2) / (r1 - r2)

  # Check for unrealistic heritability estimates and warn the user
  if (any(heritability_estimates < 0)) {
    warning("Some calculated heritability values are negative, which may indicate assumption violations or questions about directionality.")
  }

  if (any(heritability_estimates > 1)) {
    warning("Some calculated heritability values are greater than 1, which may suggest overestimation or errors in the observed correlations or relatedness coefficients.")
  }
  return(heritability_estimates)
}
