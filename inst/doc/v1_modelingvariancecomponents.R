## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup, include=FALSE-----------------------------------------------------
library(BGmisc)
if (!requireNamespace("EasyMx", quietly = TRUE)) {
  message("Please install EasyMx to run the model fitting examples.")
} else {
  require(EasyMx)
}
if (!requireNamespace("OpenMx", quietly = TRUE)) {
  message("Please install OpenMx to run the model fitting examples.")
} else {
  require(OpenMx)
}

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

## ----include=FALSE------------------------------------------------------------
if (!requireNamespace("OpenMx", quietly = TRUE)) {
  # if OpenMx isn't available
  n_subjects <- 500
  require(dplyr)

  df_summary_data <- data.frame(
    age_mean = 34.45,
    age_sd = 10.12345,
    ht1_mean = 1.662,
    ht1_sd = 0.0896277,
    ht2_mean = 1.694,
    ht2_sd = 0.09903498
  )
  set.seed(12345)
  twinData <- data.frame(
    ht1 = rnorm(n_subjects, mean = df_summary_data$ht1_mean, sd = df_summary_data$ht1_sd),
    zyg = rep(c(1, 3), each = n_subjects / 2)
  )
  twinData$ht2 <- twinData$ht1 * ifelse(twinData$zyg == 1, 1, 0.5) +
    rnorm(n_subjects, mean = 0, sd = df_summary_data$ht2_sd) + .1 * rnorm(n_subjects, mean = df_summary_data$ht2_mean, sd = df_summary_data$ht2_sd)
  twinData$ht2[twinData$zyg == 3] <- twinData$ht2[twinData$zyg == 3] + .5 * rnorm(sum(twinData$zyg == 3), mean = df_summary_data$ht2_mean, sd = df_summary_data$ht2_sd)
} else {
  data(twinData, package = "OpenMx")
}

## -----------------------------------------------------------------------------
require(dplyr)


selVars <- c("ht1", "ht2")

mzdzData <- subset(
  twinData, zyg %in% c(1, 3),
  c(selVars, "zyg")
)

mzdzData$RCoef <- c(1, NA, .5)[mzdzData$zyg]


mzData <- mzdzData %>% filter(zyg == 1)

## -----------------------------------------------------------------------------
if (!requireNamespace("EasyMx", quietly = TRUE)) {
  print("Please install EasyMx to run the model fitting examples.")
} else {
  run1 <- emxTwinModel(
    model = "Cholesky",
    relatedness = "RCoef",
    data = mzData,
    use = selVars,
    run = TRUE, name = "TwCh"
  )

  summary(run1)
}

## -----------------------------------------------------------------------------
if (!requireNamespace("EasyMx", quietly = TRUE)) {
  print("Please install EasyMx to run the model fitting examples.")
} else {
  run2 <- emxTwinModel(
    model = "Cholesky",
    relatedness = "RCoef",
    data = mzdzData,
    use = selVars,
    run = TRUE, name = "TwCh"
  )

  summary(run2)
}

