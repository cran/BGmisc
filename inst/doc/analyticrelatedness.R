## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup--------------------------------------------------------------------
library(BGmisc)

## -----------------------------------------------------------------------------
# Example usage:
# For full siblings, the relatedness coefficient is expected to be 0.5:
calculateRelatedness(generations = 1, full = TRUE)
# For half siblings, the relatedness coefficient is expected to be 0.25:
calculateRelatedness(generations = 1, full = FALSE)

## -----------------------------------------------------------------------------
# Example usage:
# Infer the relatedness coefficient:
inferRelatedness(obsR = 0.5, aceA = 0.9, aceC = 0, sharedC = 0)

