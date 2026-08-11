library(tidyverse)
library(tinytable)
library(here)

#Trees per ward
ward_counts <- tree_data_clean |>
  count(ward, name = "total_trees_ward")

#Calculate 10-20-30 rule benchmarks per ward
# A. Species Threshold Check (> 10%)
species_summary <- tree_data_clean |>
  count(ward, species_name, name = "species_count") |>
  left_join(ward_counts, by = "ward") |>
  mutate(species_prop = species_count / total_trees_ward) |>
  group_by(ward) |>
  summarize(
    max_species = max(species_prop),
    dominant_species = species_name[which.max(species_prop)]
  )

species_summary

# B. Genus Threshold Check (> 20%)
genus_summary <- tree_data_clean |>
  count(ward, genus_name, name = "genus_count") |>
  left_join(ward_counts, by = "ward") |>
  mutate(genus_prop = genus_count / total_trees_ward) |>
  group_by(ward) |>
  summarize(
    max_genus = max(genus_prop),
    dominant_genus = genus_name[which.max(genus_prop)]
  )

genus_summary

# C. Family Threshold Check (> 30%)
family_summary <- tree_data_clean |>
  count(ward, family_name, name = "family_count") |>
  left_join(ward_counts, by = "ward") |>
  mutate(family_prop = family_count / total_trees_ward) |>
  group_by(ward) |>
  summarize(
    max_family = max(family_prop),
    dominant_family = family_name[which.max(family_prop)]
  )

family_summary

# D. DBH Summary Statistics per Ward
dbh_summary <- tree_data_clean |>
  group_by(ward) |>
  summarize(
    mean_dbh = mean(dbh_trunk, na.rm = TRUE),
    median_dbh = median(dbh_trunk, na.rm = TRUE),
    sd_dbh = sd(dbh_trunk, na.rm = TRUE)
  )

dbh_summary

#Join ward-level metrics and overall rule compliance flag
ward_metrics <- ward_counts |>
  left_join(species_summary, by = "ward") |>
  left_join(genus_summary, by = "ward") |>
  left_join(family_summary, by = "ward") |>
  left_join(dbh_summary, by = "ward")

#Format ward numbers
ward_metrics <- ward_metrics[-nrow(ward_metrics), ]

ward_metrics <- ward_metrics |>
  mutate(ward = as.numeric(unlist(ward)))

#Add ward name variable
ward_metrics <- ward_metrics |>
  left_join(
    wards |>
      select(`Ward Number`, `Ward Name`), 
    by = c("ward" = "Ward Number")
  ) |>
  relocate(`Ward Name`, .after = ward)

#Format ward name  
ward_metrics <- ward_metrics |>
  rename(ward_name = `Ward Name`)

view(ward_metrics)

#Create table for presentation and report
ward_table <- tt(head(ward_metrics, 25)) |> 
  style_tt(i = 1, bg = "lightgray")

# Save the finished table object to disk
saveRDS(ward_table, file = here("models", "ward_table.rds"))