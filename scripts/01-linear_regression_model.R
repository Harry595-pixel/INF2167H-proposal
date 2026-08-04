library(tidyverse)
library(broom)
library(here)

#Prepare Ward-Level Continuous Metrics
#Calculate Shannon Diversity Index (H') per ward
ward_shannon <- tree_data_clean |>
  count(ward, species_name) |>
  group_by(ward) |>
  mutate(p = n / sum(n)) |>
  summarize(shannon_index = -sum(p * log(p)))

#Rescale proportions to percentages (0-100%) and calculate excess violations
ward_metrics_refined <- ward_metrics |>
  left_join(ward_shannon, by = "ward") |>
  mutate(
    #Rescale to percentages for readable beta coefficients
    species_pct = max_species_prop * 100,
    genus_pct   = max_genus_prop * 100,
    family_pct  = max_family_prop * 100,
    
    #Degree of Violation (Excess percentage above 10-20-30 cutoffs)
    species_excess = pmax(species_pct - 10, 0),
    genus_excess   = pmax(genus_pct - 20, 0),
    family_excess  = pmax(family_pct - 30, 0)
  )

#Model 1: Multi-Taxa Continuous Concentrations
#Predict Ward Mean DBH using continuous species, genus, and family percentages
model_1_concentrations <- lm(
  mean_dbh ~ species_pct + genus_pct + family_pct, 
  data = ward_metrics_refined
)

summary(model_1_concentrations)
tidy(model_1_concentrations, conf.int = TRUE)

#Model 2: Degree of 10-20-30 Rule Violation (Excess Concentration)
#Predict Ward Mean DBH based on how far wards exceed the 10-20-30 cutoffs
model_2_excess <- lm(
  mean_dbh ~ species_excess + genus_excess + family_excess, 
  data = ward_metrics_refined
)

summary(model_2_excess)
tidy(model_2_excess, conf.int = TRUE)

#Model 3: Overall Taxonomic Diversity (Shannon Index)
#Predict Ward Mean DBH using continuous species evenness/richness
model_3_shannon <- lm(
  mean_dbh ~ shannon_index, 
  data = ward_metrics_refined
)

summary(model_3_shannon)
tidy(model_3_shannon, conf.int = TRUE)

view(ward_metrics_refined)

# Save model object to disk
saveRDS(model_1_concentrations, file = here("models", "model_1.rds"))