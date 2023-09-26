## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup--------------------------------------------------------------------
library(BGmisc)
data(hazard)

## ---- echo=FALSE, results='hide', out.width='50%', fig.cap="Hazard Pedigree"----
capture.output(plotPedigree(hazard, code_male = 0))

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

## -----------------------------------------------------------------------------
mit <- ped2mit(hazard)
mit[1:7, 1:7]
table(mit)

## -----------------------------------------------------------------------------
commonNuclear <- ped2cn(hazard)
commonNuclear [1:7, 1:7]

table(commonNuclear)

## -----------------------------------------------------------------------------
extendedFamilyEnvironment <- ped2ce(hazard)
extendedFamilyEnvironment[1:7, 1:7]
table(extendedFamilyEnvironment)

