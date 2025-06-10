## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
#options(rmarkdown.html_vignette.check_title = FALSE)

## -----------------------------------------------------------------------------
library(BGmisc)
# Example usage:
# For full siblings, the relatedness coefficient is expected to be 0.5:
calculateRelatedness(generations = 1, full = TRUE)
# For half siblings, the relatedness coefficient is expected to be 0.25:
calculateRelatedness(generations = 1, full = FALSE)

## -----------------------------------------------------------------------------
# Example usage:
# Infer the relatedness coefficient:
inferRelatedness(obsR = 0.5, aceA = 0.9, aceC = 0, sharedC = 0)

## -----------------------------------------------------------------------------
# Now assume shared environment is fully shared:
inferRelatedness(obsR = 0.5, aceA = 0.45, aceC = 0.45, sharedC = 1)

## -----------------------------------------------------------------------------
library(BGmisc)
library(ggpedigree)
data(hazard)

df <- hazard |> dplyr::rename(personID = ID) # this is the data that we will use for the example

# Plot the pedigree to visualize relationships

ggpedigree(df, config = list(
  personID = "personID",
  momID = "momID",
  dadID = "dadID",
  famID = "famID",
  code_male = 0
))

## -----------------------------------------------------------------------------
ped_add_partial_complete <- ped2com(df,
  isChild_method = "partialparent",
  component = "additive",
  adjacency_method = "direct",
  sparse = FALSE
)
ped_add_classic_complete <- ped2com(df,
  isChild_method = "classic",
  component = "additive", adjacency_method = "direct",
  sparse = FALSE
)

## -----------------------------------------------------------------------------
library(ggpedigree)
ggRelatednessMatrix(as.matrix(ped_add_classic_complete),
  config =
    list(title = "Additive component - Classic method")
)

ggRelatednessMatrix(as.matrix(ped_add_partial_complete),
  config =
    list(title = "Additive component - Partial parent method")
)

## ----warning=FALSE------------------------------------------------------------
library(corrplot)
corrplot((as.matrix(ped_add_classic_complete) - as.matrix(ped_add_partial_complete)),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, order = "hclust",
  tl.pos = "l", tl.col = "black", tl.srt = 5, tl.cex = 0.2,
  col = COL1("Reds", 100), mar = c(0, 0, 2, 0)
)

## -----------------------------------------------------------------------------
df$momID[df$ID == 4] <- NA

## -----------------------------------------------------------------------------
ped_add_partial_mom <- ped_add_partial <- ped2com(df,
  isChild_method = "partialparent",
  component = "additive",
  adjacency_method = "direct",
  sparse = FALSE
)

ped_add_classic_mom <- ped_add_classic <- ped2com(df,
  isChild_method = "classic",
  component = "additive", adjacency_method = "direct",
  sparse = FALSE
)

## -----------------------------------------------------------------------------
corrplot(as.matrix(ped_add_classic),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Classic (mother removed)",
  order = "hclust",
  tl.pos = "l", tl.col = "black", tl.srt = 5, tl.cex = 0.2,
  col = COL1("Reds", 100), mar = c(0, 0, 2, 0)
)
corrplot(as.matrix(ped_add_partial),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Partial (mother removed)",
  order = "hclust",
  tl.pos = "l", tl.col = "black", tl.srt = 5, tl.cex = 0.2,
  col = COL1("Reds", 100), mar = c(0, 0, 2, 0)
)

## -----------------------------------------------------------------------------
sqrt(mean((as.matrix(ped_add_classic) - as.matrix(ped_add_partial))^2))

## -----------------------------------------------------------------------------
corrplot(as.matrix(ped_add_classic_complete) - as.matrix(ped_add_classic),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE,
  order = "hclust",
  tl.pos = "l", tl.col = "black", tl.srt = 5, tl.cex = 0.2,
  col = COL1("Reds", 100), mar = c(0, 0, 2, 0)
)

sqrt(mean((ped_add_classic_complete - ped_add_classic)^2))

## -----------------------------------------------------------------------------
corrplot(as.matrix(ped_add_classic_complete - ped_add_partial),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE,
  order = "hclust",
  tl.pos = "l", tl.col = "black", tl.srt = 5, tl.cex = 0.2,
  col = COL1("Reds", 100), mar = c(0, 0, 2, 0)
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
  adjacency_method = "direct",
  sparse = FALSE
)

ped_add_classic_dad <- ped_add_classic <- ped2com(df,
  isChild_method = "classic",
  component = "additive", adjacency_method = "direct",
  sparse = FALSE
)

## -----------------------------------------------------------------------------
corrplot(as.matrix(ped_add_classic_dad),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Classic (father removed)",
  order = "hclust",
  tl.pos = "l", tl.col = "black", tl.srt = 5, tl.cex = 0.2,
  col = COL1("Reds", 100), mar = c(0, 0, 2, 0)
)

corrplot(as.matrix(ped_add_partial_dad),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Partial (father removed)",
  order = "hclust",
  tl.pos = "l", tl.col = "black", tl.srt = 5, tl.cex = 0.2,
  col = COL1("Reds", 100), mar = c(0, 0, 2, 0)
)

## -----------------------------------------------------------------------------
corrplot(as.matrix(ped_add_classic_complete - ped_add_classic),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE,
  order = "hclust",
  tl.pos = "l", tl.col = "black", tl.srt = 5, tl.cex = 0.2,
  col = COL1("Reds", 100), mar = c(0, 0, 2, 0)
)

sqrt(mean((ped_add_classic_complete - ped_add_classic)^2))

## -----------------------------------------------------------------------------
corrplot(as.matrix(ped_add_classic_complete - ped_add_partial),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE,
  order = "hclust",
  tl.pos = "l", tl.col = "black", tl.srt = 5, tl.cex = 0.2,
  col = COL1("Reds", 100), mar = c(0, 0, 2, 0)
)

sqrt(mean((ped_add_classic_complete - ped_add_partial)^2))

## -----------------------------------------------------------------------------
data("inbreeding")

df <- inbreeding

famIDs <- unique(df$famID)

## -----------------------------------------------------------------------------
inbreeding_list <- list()
results <- data.frame(
  famIDs = famIDs,
  RMSE_partial_dad = rep(NA, length(famIDs)),
  RMSE_partial_mom = rep(NA, length(famIDs)),
  RMSE_classic_dad = rep(NA, length(famIDs)),
  RMSE_classic_mom = rep(NA, length(famIDs)),
  max_R_classic_dad = rep(NA, length(famIDs)),
  max_R_partial_dad = rep(NA, length(famIDs)),
  max_R_classic_mom = rep(NA, length(famIDs)),
  max_R_partial_mom = rep(NA, length(famIDs)),
  max_R_classic = rep(NA, length(famIDs))
)

## -----------------------------------------------------------------------------
for (i in 1:length(famIDs)) {
  # make three versions to filter down
  df_fam_dad <- df_fam_mom <- df_fam <- df[df$famID == famIDs[i], ]

  results$RMSE_partial_mom[i] <- sqrt(mean((ped_add_classic_complete - ped_add_partial_mom)^2))


  ped_add_partial_complete <- ped2com(df_fam,
    isChild_method = "partialparent",
    component = "additive",
    adjacency_method = "direct",
    sparse = FALSE
  )

  ped_add_classic_complete <- ped2com(df_fam,
    isChild_method = "classic",
    component = "additive",
    adjacency_method = "direct",
    sparse = FALSE
  )


  # select first ID with a mom and dad
  momid_to_cut <- head(df_fam$ID[!is.na(df_fam$momID)], 1)
  dadid_to_cut <- head(df_fam$ID[!is.na(df_fam$dadID)], 1)

  df_fam_dad$dadID[df_fam$ID == dadid_to_cut] <- NA

  df_fam_mom$momID[df_fam$ID == momid_to_cut] <- NA

  ped_add_partial_dad <- ped2com(df_fam_dad,
    isChild_method = "partialparent",
    component = "additive",
    adjacency_method = "direct",
    sparse = FALSE
  )
  ped_add_classic_dad <- ped2com(df_fam_dad,
    isChild_method = "classic",
    component = "additive", adjacency_method = "direct",
    sparse = FALSE
  )

  results$RMSE_partial_dad[i] <- sqrt(mean((ped_add_classic_complete - ped_add_partial_dad)^2))
  results$RMSE_classic_dad[i] <- sqrt(mean((ped_add_classic_complete - ped_add_classic_dad)^2))
  results$max_R_classic_dad[i] <- max(as.matrix(ped_add_classic_dad))
  results$max_R_partial_dad[i] <- max(as.matrix(ped_add_partial_dad))


  ped_add_partial_mom <- ped2com(df_fam_mom,
    isChild_method = "partialparent",
    component = "additive",
    adjacency_method = "direct",
    sparse = FALSE
  )

  ped_add_classic_mom <- ped2com(df_fam_mom,
    isChild_method = "classic",
    component = "additive", adjacency_method = "direct",
    sparse = FALSE
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
library(ggpedigree)
df <- inbreeding_list[[1]]$df_fam

ggpedigree(df,
  verbose = FALSE, personID = "ID",
  config = list(
    code_male = 0
  )
)

## -----------------------------------------------------------------------------
# pull the first family from the list
fam1 <- inbreeding_list[[1]]

corrplot(as.matrix(fam1$ped_add_classic_complete),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Classic - Complete",
  order = "hclust",
  tl.pos = "l", tl.col = "black", tl.srt = 5, tl.cex = 0.2,
  col = COL1("Reds", 100), mar = c(0, 0, 2, 0)
)

corrplot(as.matrix(fam1$ped_add_classic_mom),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Classic - Mom Missing",
  order = "hclust",
  tl.pos = "l", tl.col = "black", tl.srt = 5, tl.cex = 0.2,
  col = COL1("Reds", 100), mar = c(0, 0, 2, 0)
)

corrplot(as.matrix(fam1$ped_add_partial_mom),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Partial - Mom Missing",
  order = "hclust",
  tl.pos = "l", tl.col = "black", tl.srt = 5, tl.cex = 0.2,
  col = COL1("Reds", 100), mar = c(0, 0, 2, 0)
)

corrplot(as.matrix(fam1$ped_add_classic_dad),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Classic - Dad Missing",
  order = "hclust",
  tl.pos = "l", tl.col = "black", tl.srt = 5, tl.cex = 0.2,
  col = COL1("Reds", 100), mar = c(0, 0, 2, 0)
)

corrplot(as.matrix(fam1$ped_add_partial_dad),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Partial - Dad Missing",
  order = "hclust",
  tl.pos = "l", tl.col = "black", tl.srt = 5, tl.cex = 0.2,
  col = COL1("Reds", 100), mar = c(0, 0, 2, 0)
)

## -----------------------------------------------------------------------------
corrplot(as.matrix(fam1$ped_add_classic_complete - fam1$ped_add_classic_mom),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Classic Mom Diff from Complete",
  order = "hclust",
  tl.pos = "l", tl.col = "black", tl.srt = 5, tl.cex = 0.2,
  col = COL1("Reds", 100), mar = c(0, 0, 2, 0)
)

corrplot(as.matrix(fam1$ped_add_classic_complete - fam1$ped_add_partial_mom),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Partial Mom Diff from Complete",
  order = "hclust",
  tl.pos = "l", tl.col = "black", tl.srt = 5, tl.cex = 0.2,
  col = COL1("Reds", 100), mar = c(0, 0, 2, 0)
)

corrplot(as.matrix(fam1$ped_add_classic_complete - fam1$ped_add_classic_dad),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Classic Dad Diff from Complete",
  order = "hclust",
  tl.pos = "l", tl.col = "black", tl.srt = 5, tl.cex = 0.2,
  col = COL1("Reds", 100), mar = c(0, 0, 2, 0)
)

corrplot(as.matrix(fam1$ped_add_classic_complete - fam1$ped_add_partial_dad),
  method = "color", type = "lower", col.lim = c(0, 1),
  is.corr = FALSE, title = "Partial Dad Diff from Complete",
  order = "hclust",
  tl.pos = "l", tl.col = "black", tl.srt = 5, tl.cex = 0.2,
  col = COL1("Reds", 100), mar = c(0, 0, 2, 0)
)

## -----------------------------------------------------------------------------
results <- as.data.frame(results)

results$RMSE_diff_dad <- results$RMSE_classic_dad - results$RMSE_partial_dad
results$RMSE_diff_mom <- results$RMSE_classic_mom - results$RMSE_partial_mom

## -----------------------------------------------------------------------------
summary(dplyr::select(results, RMSE_diff_mom, RMSE_diff_dad))

## -----------------------------------------------------------------------------
mean(results$RMSE_diff_mom > 0, na.rm = TRUE)
mean(results$RMSE_diff_dad > 0, na.rm = TRUE)

## -----------------------------------------------------------------------------
results |>
  as.data.frame() |>
  dplyr::select(
    -famIDs, -RMSE_diff_mom, -RMSE_diff_dad, -max_R_classic_dad,
    -max_R_partial_dad, -max_R_classic_mom, -max_R_partial_mom, -max_R_classic
  ) |>
  summary()

