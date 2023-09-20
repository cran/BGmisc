## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup, include=FALSE-----------------------------------------------------
library(BGmisc)
require(EasyMx)
require(OpenMx)

## -----------------------------------------------------------------------------
library(BGmisc)
library(EasyMx)
library(OpenMx)

## -----------------------------------------------------------------------------
comp2vech(list(
  matrix(c(1, .5, .5, 1), 2, 2),
  matrix(1, 2, 2)
))

## -----------------------------------------------------------------------------
identifyComponentModel(
  A = list(matrix(1, 2, 2)),
  C = list(matrix(1, 2, 2)),
  E = diag(1, 2)
)

## -----------------------------------------------------------------------------
identifyComponentModel(
  A = list(matrix(c(1, .5, .5, 1), 2, 2), matrix(1, 2, 2)),
  C = list(matrix(1, 2, 2), matrix(1, 2, 2)),
  E = diag(1, 4)
)

## -----------------------------------------------------------------------------
require(dplyr)
# require(purrr)

data(twinData, package = "OpenMx")
selVars <- c("ht1", "ht2")

mzdzData <- subset(
  twinData, zyg %in% c(1, 3),
  c(selVars, "zyg")
)

mzdzData$RCoef <- c(1, NA, .5)[mzdzData$zyg]


mzData <- mzdzData %>% filter(zyg == 1)

## -----------------------------------------------------------------------------
run1 <- emxTwinModel(
  model = "Cholesky",
  relatedness = "RCoef",
  data = mzData,
  use = selVars,
  run = TRUE, name = "TwCh"
)

summary(run1)

## -----------------------------------------------------------------------------
run2 <- emxTwinModel(
  model = "Cholesky",
  relatedness = "RCoef",
  data = mzdzData,
  use = selVars,
  run = TRUE, name = "TwCh"
)

summary(run2)

