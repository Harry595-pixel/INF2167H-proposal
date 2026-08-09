#If not already done, install these packages in your console using
#install.packages()

library(tidyverse)
library(dplyr)
library(janitor)
library(WorldFlora)

#create dataset with only relevant variables from raw_data
tree_data <- data |>
  select(STRUCTID, WARD, BOTANICAL_NAME, DBH_TRUNK)

#reformat variable names
tree_data_clean <- tree_data |>
  clean_names()

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

#create new columns for species, genus, and family based on botanical_name
#step 1: load WFO backbone to variable
#if first time running script, you will need to run download line:
#WFO.download()

#Open the file picker (select 'classification.csv')
wfo_file_path <- file.choose() 

#Read the text file directly (avoids WFO.remember silent failures)
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

#step 4: Run WFO.match only on unique tree names
matched_results <- WFO.match(
  spec.data = cleaned_names,
  WFO.data = WFO.data,
  spec.name = 
)

best_unique <- WFO.one(matched_results)

#step 5: Join species, genus, and family name to tree_data_clean dataset
taxonomy_extract <- best_unique |>
  select(
    botanical_name,
    species_name = spec.name,
    family_name = family,
    genus_name = genus
  )

tree_data_clean <- tree_data_clean|>
  left_join(
    taxonomy_extract,
    by = "botanical_name"
  )

tree_data_clean <- tree_data_clean |>
  relocate(species_name, genus_name, family_name, .after = botanical_name)

#Checking missing data in taxonomy categorization
missing_data <- tree_data_clean |>
  filter(is.na(species_name) | is.na(genus_name) | is.na(family_name)) |>
  count(botanical_name, sort = TRUE)
  
missing_data

#Fill in missing data for species, genus, and family
#Assume that botanical_names with count n < 1000 are negligible considering the size of the dataset
# step 1: Create lookup table for botanical_names with count n > 1000
lookup_table <- tibble(
  botanical_name = c(
    "Acer x freemanii (A. rubrum x saccharinum) 'Autumn Blaze'",
    "Quercus alba",
    "Acer x freemanii (A. rubrum x saccharinum) 'Armstrong'",
    "Magnolia x soulngeana (M. denudata x liliiflora)",
    "Acer x freemanii (A. rubrum x saccharinum) 'Marmo'",
    "Acer x freemanii (A. rubrum x saccharinum) 'Celebration'",
    "Acer x freemanii (A. rubrum x saccharinum) 'Sienna Glen'",
    "Acer x freemanii (A. rubrum x saccharinum)"
  ),
  species_lookup = c(
    "Acer x freemanii", "Quercus alba", "Acer x freemanii", "Magnolia x soulangeana",
    "Acer x freemanii", "Acer x freemanii", "Acer x freemanii", "Acer x freemanii"
  ),
  genus_lookup = c(
    "Acer", "Quercus", "Acer", "Magnolia", 
    "Acer", "Acer", "Acer", "Acer"
  ),
  family_lookup = c(
    "Sapindaceae", "Fagaceae", "Sapindaceae", "Magnoliaceae", 
    "Sapindaceae", "Sapindaceae", "Sapindaceae", "Sapindaceae"
  )
)

# 2. Join and populate missing taxonomy
tree_data_clean <- tree_data_clean |>
  left_join(lookup_table, by = "botanical_name") |>
  mutate(
    species_name = coalesce(species_name, species_lookup),
    genus_name = coalesce(genus_name, genus_lookup),
    family_name = coalesce(family_name, family_lookup)
  ) |>
  select(-ends_with("_lookup"))

#Check for missing data again
missing_data_2 <- tree_data_clean |>
  filter(is.na(species_name) | is.na(genus_name) | is.na(family_name)) |>
  count(botanical_name, sort = TRUE)

#Summary of missing data
tree_data_clean |>
  summarize(across(everything(), ~ sum(is.na(.))))

#Exclude missing data
tree_data_clean <- tree_data_clean |>
  filter(
    !is.na(species_name), 
    !is.na(genus_name), 
    !is.na(family_name), 
    !is.na(ward), 
    dbh_trunk > 0
  )

view(tree_data_clean)

view(unique_trees)

saveRDS(tree_data_clean, file = here("models", "tree_data_clean.rds"))