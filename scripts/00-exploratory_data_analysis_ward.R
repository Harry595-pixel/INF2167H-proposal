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
    max_species_prop = max(species_prop),
    dominant_species = species_name[which.max(species_prop)],
    species_violation = if_else(max_species_prop > 0.10, 1, 0) # Exceeds 10%
  )

species_summary

# B. Genus Threshold Check (> 20%)
genus_summary <- tree_data_clean |>
  count(ward, genus_name, name = "genus_count") |>
  left_join(ward_counts, by = "ward") |>
  mutate(genus_prop = genus_count / total_trees_ward) |>
  group_by(ward) |>
  summarize(
    max_genus_prop = max(genus_prop),
    dominant_genus = genus_name[which.max(genus_prop)],
    genus_violation = if_else(max_genus_prop > 0.20, 1, 0) # Exceeds 20%
  )

genus_summary

# C. Family Threshold Check (> 30%)
family_summary <- tree_data_clean |>
  count(ward, family_name, name = "family_count") |>
  left_join(ward_counts, by = "ward") |>
  mutate(family_prop = family_count / total_trees_ward) |>
  group_by(ward) |>
  summarize(
    max_family_prop = max(family_prop),
    dominant_family = family_name[which.max(family_prop)],
    family_violation = if_else(max_family_prop > 0.30, 1, 0) # Exceeds 30%
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
  left_join(dbh_summary, by = "ward") |>
  mutate(
    # Overall 10-20-30 Violation Indicator (1 if any rule is broken, 0 otherwise)
    any_rule_violation = if_else(
      species_violation == 1 | genus_violation == 1 | family_violation == 1, 
      1, 
      0
    )
  )

#Add region variable
ward_metrics <- ward_metrics |>
  left_join(
    tree_data_clean |>
      distinct(ward, region), 
    by = "ward"
  ) |>
  relocate(region, .after = ward)

view(ward_metrics)

#Create table for presentation and report
ward_table <- tt(head(ward_metrics, 10)) |> 
  style_tt(i = 1, bg = "lightgray")

# Save the finished table object to disk
saveRDS(ward_table, file = here("models", "ward_table.rds"))