## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)
library(tidyverse)

## -----------------------------------------------------------------------------
library(BGmisc)

data(hazard)

df <- hazard # this is the data that we will use for the example

## ----echo=FALSE---------------------------------------------------------------
plotPedigree(df)

## -----------------------------------------------------------------------------
ped_add_partial_complete <- ped2com(df,
  isChild_method = "partialparent",
  component = "additive",
  adjacency_method = "direct"
)
ped_add_classic_complete <- ped2com(df,
  isChild_method = "classic",
  component = "additive", adjacency_method = "direct"
)

## -----------------------------------------------------------------------------
library(corrplot)


corrplot(as.matrix(ped_add_classic_complete),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Additive component - Classic method"
)

corrplot(as.matrix(ped_add_partial_complete),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Additive component - Partial parent method"
)

## -----------------------------------------------------------------------------
corrplot(as.matrix(ped_add_classic_complete - ped_add_partial_complete),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE
)

## -----------------------------------------------------------------------------
df$momID[df$ID == 4] <- NA

## -----------------------------------------------------------------------------
ped_add_partial_mom <- ped_add_partial <- ped2com(df,
  isChild_method = "partialparent",
  component = "additive",
  adjacency_method = "direct"
)

ped_add_classic_mom <- ped_add_classic <- ped2com(df,
  isChild_method = "classic",
  component = "additive", adjacency_method = "direct"
)

## -----------------------------------------------------------------------------
corrplot(as.matrix(ped_add_classic),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Classic (mother removed)"
)

corrplot(as.matrix(ped_add_partial),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Partial (mother removed)"
)

## -----------------------------------------------------------------------------
sqrt(mean((ped_add_classic - ped_add_partial)^2))

## -----------------------------------------------------------------------------
corrplot(as.matrix(ped_add_classic_complete - ped_add_classic),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE
)

sqrt(mean((ped_add_classic_complete - ped_add_classic)^2))

## -----------------------------------------------------------------------------
corrplot(as.matrix(ped_add_classic_complete - ped_add_partial),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE
)

sqrt(mean((ped_add_classic_complete - ped_add_partial)^2))

## -----------------------------------------------------------------------------
data(hazard)

df <- hazard # this is the data that we will use for the example


df$dadID[df$ID == 4] <- NA
# add
ped_add_partial_dad <- ped_add_partial <- ped2com(df,
  isChild_method = "partialparent",
  component = "additive",
  adjacency_method = "direct"
)

ped_add_classic_dad <- ped_add_classic <- ped2com(df,
  isChild_method = "classic",
  component = "additive", adjacency_method = "direct"
)

## -----------------------------------------------------------------------------
corrplot(as.matrix(ped_add_classic_dad),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Classic (father removed)"
)

corrplot(as.matrix(ped_add_partial_dad),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Partial (father removed)"
)

## -----------------------------------------------------------------------------
corrplot(as.matrix(ped_add_classic_complete - ped_add_classic),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE
)

sqrt(mean((ped_add_classic_complete - ped_add_classic)^2))

## -----------------------------------------------------------------------------
corrplot(as.matrix(ped_add_classic_complete - ped_add_partial),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE
)

sqrt(mean((ped_add_classic_complete - ped_add_partial)^2))

## -----------------------------------------------------------------------------
data("inbreeding")

df <- inbreeding

FamIDs <- unique(df$FamID)

## -----------------------------------------------------------------------------
inbreeding_list <- list()
results <- data.frame(
  FamIDs = FamIDs,
  RMSE_partial_dad = rep(NA, length(FamIDs)),
  RMSE_partial_mom = rep(NA, length(FamIDs)),
  RMSE_classic_dad = rep(NA, length(FamIDs)),
  RMSE_classic_mom = rep(NA, length(FamIDs)),
  max_R_classic_dad = rep(NA, length(FamIDs)),
  max_R_partial_dad = rep(NA, length(FamIDs)),
  max_R_classic_mom = rep(NA, length(FamIDs)),
  max_R_partial_mom = rep(NA, length(FamIDs)),
  max_R_classic = rep(NA, length(FamIDs))
)

## -----------------------------------------------------------------------------
for (i in 1:length(FamIDs)) {
  # make three versions to filter down
  df_fam_dad <- df_fam_mom <- df_fam <- df[df$FamID == FamIDs[i], ]

  results$RMSE_partial_mom[i] <- sqrt(mean((ped_add_classic_complete - ped_add_partial_mom)^2))


  ped_add_partial_complete <- ped2com(df_fam,
    isChild_method = "partialparent",
    component = "additive",
    adjacency_method = "direct"
  )

  ped_add_classic_complete <- ped2com(df_fam,
    isChild_method = "classic",
    component = "additive",
    adjacency_method = "direct"
  )


  # select first ID with a mom and dad
  momid_to_cut <- df_fam$ID[!is.na(df_fam$momID)] %>% head(1)
  dadid_to_cut <- df_fam$ID[!is.na(df_fam$dadID)] %>% head(1)

  df_fam_dad$dadID[df_fam$ID == dadid_to_cut] <- NA

  df_fam_mom$momID[df_fam$ID == momid_to_cut] <- NA

  ped_add_partial_dad <- ped2com(df_fam_dad,
    isChild_method = "partialparent",
    component = "additive",
    adjacency_method = "direct"
  )
  ped_add_classic_dad <- ped2com(df_fam_dad,
    isChild_method = "classic",
    component = "additive", adjacency_method = "direct"
  )

  results$RMSE_partial_dad[i] <- sqrt(mean((ped_add_classic_complete - ped_add_partial_dad)^2))
  results$RMSE_classic_dad[i] <- sqrt(mean((ped_add_classic_complete - ped_add_classic_dad)^2))
  results$max_R_classic_dad[i] <- max(as.matrix(ped_add_classic_dad))
  results$max_R_partial_dad[i] <- max(as.matrix(ped_add_partial_dad))


  ped_add_partial_mom <- ped2com(df_fam_mom,
    isChild_method = "partialparent",
    component = "additive",
    adjacency_method = "direct"
  )

  ped_add_classic_mom <- ped2com(df_fam_mom,
    isChild_method = "classic",
    component = "additive", adjacency_method = "direct"
  )

  results$RMSE_partial_mom[i] <- sqrt(mean((ped_add_classic_complete - ped_add_partial_mom)^2))
  results$RMSE_classic_mom[i] <- sqrt(mean((ped_add_classic_complete - ped_add_classic_mom)^2))
  results$max_R_classic_mom[i] <- max(as.matrix(ped_add_classic_mom))
  results$max_R_partial_mom[i] <- max(as.matrix(ped_add_partial_mom))
  results$max_R_classic[i] <- max(as.matrix(ped_add_classic_complete))

  inbreeding_list[[i]] <- list(
    df_fam = df_fam,
    ped_add_partial_complete = ped_add_partial_complete,
    ped_add_classic_complete = ped_add_classic_complete,
    ped_add_partial_dad = ped_add_partial_dad,
    ped_add_classic_dad = ped_add_classic_dad,
    ped_add_partial_mom = ped_add_partial_mom,
    ped_add_classic_mom = ped_add_classic_mom
  )
}

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
plotPedigree(inbreeding_list[[1]]$df_fam,
  verbose = FALSE
)

## -----------------------------------------------------------------------------
# pull the first family from the list
fam1 <- inbreeding_list[[1]]

corrplot(as.matrix(fam1$ped_add_classic_complete),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Classic - Complete"
)

corrplot(as.matrix(fam1$ped_add_classic_mom),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Classic - Mom Missing"
)

corrplot(as.matrix(fam1$ped_add_partial_mom),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Partial - Mom Missing"
)

corrplot(as.matrix(fam1$ped_add_classic_dad),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Classic - Dad Missing"
)

corrplot(as.matrix(fam1$ped_add_partial_dad),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Partial - Dad Missing"
)

## -----------------------------------------------------------------------------
corrplot(as.matrix(fam1$ped_add_classic_complete - fam1$ped_add_classic_mom),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Classic Mom Diff from Complete"
)

corrplot(as.matrix(fam1$ped_add_classic_complete - fam1$ped_add_partial_mom),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Partial Mom Diff from Complete"
)

corrplot(as.matrix(fam1$ped_add_classic_complete - fam1$ped_add_classic_dad),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Classic Dad Diff from Complete"
)

corrplot(as.matrix(fam1$ped_add_classic_complete - fam1$ped_add_partial_dad),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Partial Dad Diff from Complete"
)

## -----------------------------------------------------------------------------
results <- results %>%
  as.data.frame() %>%
  mutate(
    RMSE_diff_dad = RMSE_classic_dad - RMSE_partial_dad,
    RMSE_diff_mom = RMSE_classic_mom - RMSE_partial_mom
  )

## -----------------------------------------------------------------------------
results %>%
  select(RMSE_diff_mom, RMSE_diff_dad) %>%
  summary()

## -----------------------------------------------------------------------------
mean(results$RMSE_diff_mom > 0, na.rm = TRUE)
mean(results$RMSE_diff_dad > 0, na.rm = TRUE)

## -----------------------------------------------------------------------------
results %>%
  as.data.frame() %>%
  select(
    -FamIDs, -RMSE_diff_mom, -RMSE_diff_dad, -max_R_classic_dad,
    -max_R_partial_dad, -max_R_classic_mom, -max_R_partial_mom, -max_R_classic
  ) %>%
  summary()

