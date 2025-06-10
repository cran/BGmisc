## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup--------------------------------------------------------------------
library(BGmisc)
data(potter)

## ----echo=FALSE, results='hide', out.width='50%', fig.cap="Potter Family Pedigree"----
library(ggpedigree)
ggpedigree(potter, code_male = 1, verbose = TRUE)

## -----------------------------------------------------------------------------
df_potter <- potter
names(df_potter)[names(df_potter) == "famID"] <- "oldfam"

ds <- ped2fam(df_potter, famID = "famID", personID = "personID")

table(ds$famID, ds$oldfam)

## -----------------------------------------------------------------------------
add <- ped2add(potter, sparse = FALSE)

## -----------------------------------------------------------------------------
add[1:7, 1:7]

## -----------------------------------------------------------------------------
table(add)

## -----------------------------------------------------------------------------
add_list <- lapply(
  unique(potter$famID),
  function(d) {
    tmp <- potter[potter$famID %in% d, ]
    ped2add(tmp, sparse = FALSE)
  }
)

## -----------------------------------------------------------------------------
mit <- ped2mit(potter, sparse = FALSE)
mit[1:7, 1:7]
table(mit)

## -----------------------------------------------------------------------------
commonNuclear <- ped2cn(potter, sparse = FALSE)
commonNuclear[1:7, 1:7]

table(commonNuclear)

## -----------------------------------------------------------------------------
extendedFamilyEnvironment <- ped2ce(potter, sparse = FALSE)
extendedFamilyEnvironment[1:7, 1:7]
table(extendedFamilyEnvironment)

## ----echo=FALSE, results='hide', out.width='50%', fig.cap="Potter Subset Pedigree"----
library(ggpedigree)

names(potter)[names(potter) == "oldfam"] <- "famID"
subset_rows <- c(1:8, 11:36)
subset_potter <- potter[subset_rows, ]

subset_potter$dadID[subset_potter$dadID %in% c(9, 10)] <- NA
subset_potter$momID[subset_potter$momID %in% c(9, 10)] <- NA

ggpedigree(subset_potter, code_male = 1, verbose = TRUE)

## -----------------------------------------------------------------------------
subset_rows <- c(1:5, 31:36)
subset_potter <- potter[subset_rows, ]

## ----echo=FALSE, results='hide', out.width='50%'------------------------------
ggpedigree(subset_potter, code_male = 1, verbose = TRUE)

