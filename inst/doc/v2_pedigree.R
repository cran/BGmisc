## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## -----------------------------------------------------------------------------
## Loading Required Libraries
library(BGmisc)
library(ggpedigree)
set.seed(5)
df_ped <- simulatePedigree(
  kpc = 4,
  Ngen = 4,
  sexR = .5,
  marR = .7
)
summary(df_ped)

## -----------------------------------------------------------------------------
df_ped[21, ]

## -----------------------------------------------------------------------------
summarizeFamilies(df_ped, famID = "fam")$family_summary

## ----warning=FALSE, message=FALSE,fig.width=8, fig.height=6-------------------
library(ggpedigree)

df_ped_recoded <- recodeSex(df_ped, code_male = "M", recode_male = 1, recode_female = 0)

ggpedigree::ggpedigree(df_ped_recoded,
  personID = "ID",
  code_male = 1
)

## ----fig.width=8, fig.height=6------------------------------------------------
set.seed(8)
# Simulate a family with 3 generations
df_ped_3 <- simulatePedigree(Ngen = 3)

# Simulate a family with 4 generations
df_ped_4 <- simulatePedigree(Ngen = 4)

## ----echo=FALSE,fig.width=8, fig.height=6-------------------------------------
library(ggplot2)

df_ped_3$famID <- 1
df_ped_3$fam <- NULL
df_ped_3$ID <- df_ped_3$ID / 100
df_ped_3$dadID <- df_ped_3$dadID / 100
df_ped_3$momID <- df_ped_3$momID / 100
df_ped_3$spID <- df_ped_3$spID / 100
df_ped_4$famID <- 2
df_ped_4$fam <- NULL

df_ped_all <- rbind(df_ped_3, df_ped_4)
df_ped_all <- recodeSex(df_ped_all,
  code_male = "M",
  recode_male = 1,
  recode_female = 0
)

ggpedigree::ggpedigree(df_ped_all,
  personID = "ID",
  famID = "famID",
  config = list(
    label_method = "geom_text",
    label_text_size = 1
  ),
  code_male = 1
) +
  facet_wrap(~famID, scales = "free")

