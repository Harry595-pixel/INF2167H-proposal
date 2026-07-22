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

head(data)