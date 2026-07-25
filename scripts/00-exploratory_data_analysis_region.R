library(tidyverse)

#Trees per region
region_counts <- tree_data_clean |>
  count(region, name = "total_trees_region")

# Calculate 10-20-30 Rule Benchmarks per Region
# A. Species Threshold Check (> 10%)
species_summary <- tree_data_clean |>
  count(region, species_name, name = "species_count") |>
  left_join(region_counts, by = "region") |>
  mutate(species_prop = species_count / total_trees_region) |>
  group_by(region) |>
  summarize(
    max_species_prop = max(species_prop),
    dominant_species = species_name[which.max(species_prop)],
    species_violation = if_else(max_species_prop > 0.10, 1, 0) # Exceeds 10%
  )

# B. Genus Threshold Check (> 20%)
genus_summary <- tree_data_clean |>
  count(region, genus_name, name = "genus_count") |>
  left_join(region_counts, by = "region") |>
  mutate(genus_prop = genus_count / total_trees_region) |>
  group_by(region) |>
  summarize(
    max_genus_prop = max(genus_prop),
    dominant_genus = genus_name[which.max(genus_prop)],
    genus_violation = if_else(max_genus_prop > 0.20, 1, 0) # Exceeds 20%
  )

# C. Family Threshold Check (> 30%)
family_summary <- tree_data_clean |>
  count(region, family_name, name = "family_count") |>
  left_join(region_counts, by = "region") |>
  mutate(family_prop = family_count / total_trees_region) |>
  group_by(region) |>
  summarize(
    max_family_prop = max(family_prop),
    dominant_family = family_name[which.max(family_prop)],
    family_violation = if_else(max_family_prop > 0.30, 1, 0) # Exceeds 30%
  )

# D. DBH Summary Statistics per Region
dbh_summary <- tree_data_clean |>
  group_by(region) |>
  summarize(
    mean_dbh = mean(dbh_trunk, na.rm = TRUE),
    median_dbh = median(dbh_trunk, na.rm = TRUE),
    sd_dbh = sd(dbh_trunk, na.rm = TRUE)
  )

#Join Region-Level Metrics & Overall Rule Compliance Flag
region_metrics <- region_counts |>
  left_join(species_summary, by = "region") |>
  left_join(genus_summary, by = "region") |>
  left_join(family_summary, by = "region") |>
  left_join(dbh_summary, by = "region") |>
  mutate(
    # Overall 10-20-30 Violation Indicator (1 if ANY rule is broken, 0 otherwise)
    any_rule_violation = if_else(
      species_violation == 1 | genus_violation == 1 | family_violation == 1, 
      1, 
      0
    )
  )

view(region_metrics)