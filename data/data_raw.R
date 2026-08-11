#If not already done, install these packages in your console
#install.packages("opendatatoronto")
#install.packages("tidyverse")
#install.packages("dplyr")

library(opendatatoronto)
library(tidyverse)
library(dplyr)

data <- list_package_resources("6ac4569e-fd37-4cbc-ac63-db3624c5f6a2") |>
  filter(str_detect(id, "b65cd31d-fabc-4222-83ef-8ddd11295d2b")) |>
  get_resource()

wards <- list_package_resources("6678e1a6-d25f-4dff-b2b7-aa8f042bc2eb") |>
  filter(str_detect(id, "ea4cc466-bd4d-40c6-a616-7abfa9d7398f")) |>
  get_resource()

view(wards)


head(data)