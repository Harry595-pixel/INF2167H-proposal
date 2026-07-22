#If not already done, install these packages in your console
#install.packages("tidyverse")

library(tidyverse)
library(janitor)

tree_data <- data %>%
  select(STRUCTID, WARD, BOTANICAL_NAME, COMMON_NAME, DBH_TRUNK)

tree_data_clean <- tree_data |>
  clean_names()

head(tree_data_clean)