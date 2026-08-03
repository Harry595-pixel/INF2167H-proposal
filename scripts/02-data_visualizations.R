library(tidyverse)
library(broom)

#Plot 1: Coefficient Estimate Plot
model_1_tidy <- tidy(model_1_concentrations, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>
  mutate(
    term_clean = c("Species Concentration", "Genus Concentration", "Family Concentration")
  )

ggplot(model_1_tidy, aes(x = estimate, y = reorder(term_clean, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "forestgreen", size = 0.8) +
  geom_point(size = 3.5, color = "darkgreen") +
  labs(
    title = "Model 1 Regression Coefficients (Predicting Mean DBH)",
    subtitle = "Positive genus effect (aging monocultures) vs. negative species effect (young mass plantings)",
    x = "Effect on Mean Ward Trunk Diameter (cm per 1% increase)",
    y = NULL
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

#Plot 2: Ward Genus Dominance vs. Mean Trunk Size by Region
ggplot(ward_metrics_refined, aes(x = genus_pct, y = mean_dbh)) +
  # Points color-coded by region
  geom_point(aes(color = region), size = 3.5, alpha = 0.85) +
  # Single linear regression line for the overall model trend
  geom_smooth(method = "lm", color = "grey30", se = TRUE, linetype = "dashed") +
  scale_color_brewer(palette = "Set2", name = "Region") +
  labs(
    title = "Ward Genus Concentration vs. Average Trunk Diameter",
    subtitle = "Model 1: Testing if low-diversity wards rely on larger, aging trees",
    x = "Top Genus Concentration (%)",
    y = "Mean Ward Trunk Diameter (cm)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",              # Options: "bottom", "right", "top"
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 13)
  )

#Plot 3: Size-Class Distribution by Genera
tree_data_clean |>
  group_by(genus_name) |>
  filter(n() > 10000) |>
  ungroup() |>
  ggplot(aes(x = dbh_trunk, fill = genus_name)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.6, color = "white") +
  facet_wrap(~ genus_name) +
  coord_cartesian(
    xlim = c(0, 100),
    ylim = c(0, 15000)
    ) +
  #Customize x-axis tick mark breaks for readability
  scale_x_continuous(breaks = seq(0, 100, by = 20)) +
  labs(
    title = "Trunk Diameter Distribution Across Toronto's Top Tree Genera",
    subtitle = "Identifies which dominant genera are skewed toward old/large DBH classes",
    x = "Trunk Diameter (DBH in cm)",
    y = "Tree Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none", 
    plot.title = element_text(face = "bold"),
    panel.spacing = unit(1.3, "lines")
  )


