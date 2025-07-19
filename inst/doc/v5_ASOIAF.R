## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------
library(BGmisc)
library(tidyverse)
library(ggpedigree)
data(ASOIAF, package = "ggpedigree")

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
  filter(ID1 %in% dany_id | ID2 %in% dany_id) %>% # round to nearest 4th decimal
  mutate(across(c(addRel, mitRel, cnuRel), ~ round(.x, 4)))

jon_dany_row

## -----------------------------------------------------------------------------
rhaenyra_id <- df_got %>%
  filter(name == "Rhaenyra Targaryen") %>%
  pull(ID)
daemon_id <- df_got %>%
  filter(name == "Daemon Targaryen") %>%
  pull(ID)

rhaenyra_daemon_row <- df_links %>%
  filter(ID1 == rhaenyra_id | ID2 == rhaenyra_id) %>%
  filter(ID1 %in% daemon_id | ID2 %in% daemon_id) %>% # round to 4th decimal
  mutate(across(c(addRel, mitRel, cnuRel), ~ round(.x, 4)))

rhaenyra_daemon_row

## -----------------------------------------------------------------------------
df_repaired <- checkParentIDs(df_got, # %>% filter(famID == 1),
  addphantoms = TRUE,
  repair = TRUE,
  parentswithoutrow = FALSE,
  repairsex = FALSE
) %>% mutate(
  # famID = 1,
  affected = case_when(
    ID %in% c(jon_id, dany_id, 339) ~ TRUE,
    TRUE ~ FALSE
  )
)

## -----------------------------------------------------------------------------
checkIDs <- checkIDs(df_repaired, verbose = TRUE)

# checkIDs

## -----------------------------------------------------------------------------
# Check for unique IDs and parent-child relationships
checkPedigreeNetwork <- checkPedigreeNetwork(df_repaired,
  personID = "ID",
  momID = "momID",
  dadID = "dadID",
  verbose = TRUE
)

checkPedigreeNetwork

## ----eval=FALSE, fig.height=8, fig.width=10, message=FALSE, warning=FALSE, include=FALSE----
# plotPedigree(
#   df_repaired %>% mutate(
#     famID = 1
#   ),
#   affected = df_repaired$affected,
#   verbose = FALSE
# )

## ----message=FALSE, warning=FALSE, fig.width=10, fig.height=8-----------------
library(ggpedigree)

df_repaired_renamed <- df_repaired %>% rename(
  personID = ID
)
plt <- ggpedigree(df_repaired_renamed,
  overlay_column = "affected",
  personID = "personID",
  interactive = FALSE,
  config = list(
    overlay_include = TRUE,
    point_size = .75,
    code_male = "M",
    ped_width = 17,
    label_nudge_y = -.25,
    include_labels = TRUE,
    label_method = "geom_text",
    # segment_self_color = "purple",
    sex_color_include = FALSE,
    focal_fill_personID = 353, # 339, # 353,
    focal_fill_include = TRUE,
    tooltip_columns = c("personID", "name", "focal_fill"),
    focal_fill_force_zero = TRUE,
    focal_fill_mid_color = "orange",
    focal_fill_low_color = "#9F2A63FF",
    focal_fill_legend_title = "Relatedness to \nAegon Targaryen",
    focal_fill_na_value = "black",
    value_rounding_digits = 4
  )
)

plt

# reduce file size for CRAN
# if (interactive()) {
# If running interactively, use plotly::partial_bundle
# to reduce file size for CRAN
#  plotly::partial_bundle(plt)
# } else {
#  plotly::partial_bundle(plt, local = TRUE)
# }

## ----eval=FALSE, include=FALSE------------------------------------------------
# df_repaired %>%
#   filter(!is.na(name)) %>%
#   arrange(ID) %>%
#   knitr::kable(caption = "Key Characters in ASOIAF Pedigree")

