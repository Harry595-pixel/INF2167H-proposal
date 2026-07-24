#If not already done, install these packages in your console using
#install.packages()

library(tidyverse)
library(dplyr)
library(janitor)
library(WorldFlora)

#create dataset with only relevant variables from raw_data
tree_data <- data |>
  select(STRUCTID, WARD, BOTANICAL_NAME, COMMON_NAME, DBH_TRUNK)

#reformat variable names
tree_data_clean <- tree_data |>
  clean_names()

#create new column for genus name and move to right of botanical_name
tree_data_clean <- tree_data_clean |>
  mutate(genus_name = word(botanical_name, 1))

tree_data_clean <- tree_data_clean |>
  relocate(genus_name, .after = botanical_name)

#create new column for region based on ward and move to right of ward
tree_data_clean <- tree_data_clean |>
  mutate(
    region = case_when(
      ward %in% c("01", "02", "03", "04", "05", "07") ~ "Etobicoke York",
      ward %in% c("06", "08", "15", "16", "17", "18") ~ "North York",
      ward %in% c("09", "10", "11", "12", "13", "14", "19") ~ "Toronto/East York",
      ward %in% c("20", "21", "22", "23", "24", "25") ~ "Scarborough",
      TRUE ~ "Unknown"
    ))

tree_data_clean <- tree_data_clean |>
  relocate(region, .after = ward)

#create new column for family_name based on botanical_name
#step 1: load WFO backbone to variable
#if first time running script, you will need to run download line:
#WFO.download()

# 1. Open the file picker (select 'classification.csv')
wfo_file_path <- file.choose() 

# 2. Read the text file directly (avoids WFO.remember silent failures)
message("Loading WFO backbone, please wait ~10 seconds...")
WFO.data <- read.delim(
  wfo_file_path, 
  sep = "\t", 
  header = TRUE, 
  quote = "", 
  encoding = "UTF-8", 
  stringsAsFactors = FALSE
)

nrow(WFO.data)

#step 2: Extract unique names from botanical_names
unique_trees <- tree_data_clean |>
  select(botanical_name) |>
  distinct() |>
  filter(!is.na(botanical_name) & botanical_name != "") |>
  as.data.frame()

#step 3: Pre-clean unique name list
cleaned_names <- WFO.prepare(
  spec.data = unique_trees,
  spec.full = "botanical_name"
  )

#step 4: Run WFO.match only on the unique names
matched_results <- WFO.match(
  spec.data = cleaned_names,
  WFO.data = WFO.data,
  spec.name = 
)

best_unique <- WFO.one(matched_results)

#step 5: Join family name to tree_data_clean dataset
tree_data_clean <- tree_data_clean |>
  left_join(
    best_unique |>
      select(spec.name, family),
    by = c("botanical_name" = "spec.name")
  ) |>
  rename(family_name = family)

unique(tree_data_clean$botanical_name)
view(best_unique)