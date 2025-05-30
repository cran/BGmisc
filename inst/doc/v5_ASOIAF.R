## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
library(BGmisc)
library(tidyverse)
library(ggpedigree)
data(ASOIAF)

## -----------------------------------------------------------------------------
head(ASOIAF)

## -----------------------------------------------------------------------------
df_got <- checkSex(ASOIAF,
  code_male = 1,
  code_female = 0,
  verbose = FALSE, repair = TRUE
)

## -----------------------------------------------------------------------------
add <- ped2com(df_got,
  isChild_method = "partialparent",
  component = "additive",
  adjacency_method = "direct",
  sparse = TRUE
)

mt <- ped2com(df_got,
  isChild_method = "partialparent",
  component = "mitochondrial",
  adjacency_method = "direct",
  sparse = TRUE
)

cn <- ped2cn(df_got,
  isChild_method = "partialparent",
  adjacency_method = "indexed",
  sparse = TRUE
)

## -----------------------------------------------------------------------------
df_links <- com2links(
  writetodisk = FALSE,
  ad_ped_matrix = add, cn_ped_matrix = cn, mit_ped_matrix = mt,
  drop_upper_triangular = TRUE
) # %>%
#  filter(ID1 != ID2)

## -----------------------------------------------------------------------------
# Find the IDs of Jon Snow and Daenerys Targaryen

jon_id <- df_got %>%
  filter(name == "Jon Snow") %>%
  pull(ID)

dany_id <- df_got %>%
  filter(name == "Daenerys Targaryen") %>%
  pull(ID)

## -----------------------------------------------------------------------------
jon_dany_row <- df_links %>%
  filter(ID1 == jon_id | ID2 == jon_id) %>%
  filter(ID1 %in% dany_id | ID2 %in% dany_id)

jon_dany_row

## -----------------------------------------------------------------------------
df_repaired <- checkParentIDs(df_got,
  addphantoms = TRUE,
  repair = TRUE,
  parentswithoutrow = FALSE,
  repairsex = FALSE
) %>% mutate(
  famID = 1,
  affected = case_when(
    ID %in% c(jon_id, dany_id, "365") ~ 1,
    TRUE ~ 0
  )
)

## ----message=FALSE, warning=FALSE---------------------------------------------
plotPedigree(df_repaired, affected = df_repaired$affected, verbose = FALSE)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(ggpedigree)
plt <- ggPedigree(df_repaired,
  status_col = "affected",
  personID = "ID",
  config = list(
    status_unaffected_lab = 0,
    sex_color = TRUE,
    code_male = "M",
    status_affected_lab = 1,
    affected_shape = 4,
    ped_width = 14,
    include_tooltips = TRUE,
    label_nudge_y = -.25,
    include_labels = TRUE,
    label_method = "geom_text",
    segment_self_color = "purple",
    tooltip_cols = c("name")
  )
)

plt +
  theme(legend.position = "none") +
  labs(title = "ASOIAF Pedigree: Jon Snow and Daenerys Targaryen")

