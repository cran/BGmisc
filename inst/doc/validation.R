## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)
library(tidyverse)

## ----checkIDs-----------------------------------------------------------------
library(BGmisc)

# Load our example dataset
df <- ped2fam(potter, famID = "newFamID", personID = "personID")

# Check for ID issues
checkIDs(df, repair = FALSE)

## ----datamade-----------------------------------------------------------------
# Create our problematic dataset
df_duplicates <- df
# Sibling ID conflict
df_duplicates$personID[df_duplicates$name == "Vernon Dursley"] <-
  df_duplicates$personID[df_duplicates$name == "Marjorie Dursley"]
# Duplicate entry
df_duplicates <- rbind(
  df_duplicates,
  df_duplicates[df_duplicates$name == "Dudley Dursley", ]
)

## -----------------------------------------------------------------------------
library(tidyverse)

summarizeFamilies(df_duplicates,
  famID = "newFamID",
  personID = "personID"
)$family_summary %>%
  glimpse()

## -----------------------------------------------------------------------------
# Identify duplicates
result <- checkIDs(df_duplicates)
print(result)

## -----------------------------------------------------------------------------
# Let's examine the problematic entries
df_duplicates %>%
  filter(personID %in% result$non_unique_ids) %>%
  arrange(personID)

## -----------------------------------------------------------------------------
df_repair <- checkIDs(df, repair = TRUE)

df_repair %>%
  filter(ID %in% result$non_unique_ids) %>%
  arrange(ID)

result <- checkIDs(df_repair)

print(result)

## ----within-------------------------------------------------------------------
# Create a sample dataset with within-person duplicate parent IDs

df_within <- ped2fam(potter, famID = "newFamID", personID = "personID")

df_within$momID[df_within$name == "Vernon Dursley"] <- df_within$personID[df_within$name == "Vernon Dursley"]

# Check for within-row duplicates
result <- checkIDs(df_within, repair = FALSE)
print(result)

## -----------------------------------------------------------------------------
# Find the problematic entry

df_within[df_within$momID %in% result$is_own_mother_ids, ]

## -----------------------------------------------------------------------------
# Validate sex coding

results <- checkSex(potter,
  code_male = 1,
  code_female = 0,
  verbose = TRUE, repair = FALSE
)
print(results)

## -----------------------------------------------------------------------------
# Repair sex coding
df_fix <- checkSex(potter,
  code_male = 1,
  code_female = 0,
  verbose = TRUE, repair = TRUE
)
print(df_fix)

## ----eval = FALSE-------------------------------------------------------------
# # note, is broken right now
# # Load necessary libraries and datasets
# library(tidyverse)
# library(BGmisc)
# set.seed(123)
# # Create a sample dataset similar to the one used in Mason's approach
# sample_data <- data.frame(
#   ID = 1:10,
#   name = c("Person1", "Person2", "Person3", "Person4", "Person5", "Person6", "Person7", "Person8", "Person9", "Person10"),
#   dadID = c(NA, NA, 1, 1, 3, 3, 5, 5, 7, 7),
#   momID = c(NA, NA, 2, 2, 4, 4, 6, 6, 7, 8),
#   sex = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
#   byr = runif(10, 1900, 2000),
#   dyr = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
# )
# 
# 
# 
# summarizePedigrees(sample_data)
# 
# 
# # Clean the sample dataset
# cleaned_data <- sample_data %>%
#   janitor::remove_empty(c("rows", "cols")) %>%
#   mutate(
#     sex_factor = as.factor(case_when(sex == 1 ~ "male", sex == 0 ~ "female"))
#   )
# 
# # Check for duplicate IDs
# temp_check <- checkIDs(cleaned_data, verbose = TRUE, repair = FALSE)
# all_duplicated_ids <- cbind(temp_check$non_unique_ids, temp_check$duplicated_parents_ids)
# 
# cleaned_data <- cleaned_data %>%
#   mutate(
#     duplicated = case_when(ID %in% temp_check$non_unique_ids ~ 1, TRUE ~ 0),
#     duplicated_parent = case_when(dadID %in% all_duplicated_ids | momID %in% all_duplicated_ids ~ 1, TRUE ~ 0),
#     duplicated_source_ID = case_when(ID %in% all_duplicated_ids ~ ID, dadID %in% all_duplicated_ids ~ dadID, momID %in% all_duplicated_ids ~ momID, TRUE ~ NA_integer_),
#     alteredlinks = 0
#   )
# 
# # Display and manually correct specific errors
# cleaned_data %>%
#   filter(duplicated == 1 | duplicated_parent == 1) %>%
#   arrange(duplicated_source_ID, ID) %>%
#   print(n = Inf)
# 
# # Perform specific corrections
# cleaned_data <- cleaned_data %>%
#   mutate(
#     alteredlinks = case_when(ID == 9 ~ 1, TRUE ~ alteredlinks),
#     ID = case_when(ID == 7 & round(byr, digits = 0) == 2020 ~ ID + 1e6, TRUE ~ ID)
#   )
# 
# # Final check for remaining duplicates
# final_check <- checkIDs(cleaned_data, verbose = TRUE, repair = FALSE)
# print(final_check)

