## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup--------------------------------------------------------------------
library(BGmisc)
data(hazard)

## -----------------------------------------------------------------------------
ds <- ped2fam(hazard, famID = "newFamID")
table(ds$FamID, ds$newFamID)

## -----------------------------------------------------------------------------
add <- ped2add(hazard)

## -----------------------------------------------------------------------------
add[1:7, 1:7]

## -----------------------------------------------------------------------------
table(add)

## -----------------------------------------------------------------------------
add_list <- lapply(
  unique(hazard$FamID),
  function(d) {
    tmp <- hazard[hazard$FamID %in% d, ]
    ped2add(tmp)
  }
)

