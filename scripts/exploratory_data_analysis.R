library(tidyverse)

#Unique botanical names
unique(tree_data_clean$botanical_name)

#Unique genus names
unique(tree_data_clean$genus_name)

#Unique wards
unique(tree_data_clean$ward)

#Tree distribution by genus name
tree_data_clean |>
  group_by(genus_name) |>
  count() |>
  ungroup() |>
  mutate(percentage = round(n / sum(n) * 100, 2)) |>
  arrange(desc(n))

#Trees by ward
tree_data_clean |>
  group_by(ward) |>
  count() |>
  ungroup() |>
  mutate(percentage = round(n / sum(n) * 100, 2)) |>
  arrange(desc(n))

#Trees by region
tree_data_clean |>
  group_by(region) |>
  count() |>
  ungroup() |>
  mutate(percentage = round(n / sum(n) * 100, 2)) |>
  arrange(desc(n))