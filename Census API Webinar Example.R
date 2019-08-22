install.packages("tidycensus")
library(tidyverse)
library(tidycensus)
library(ggplot2)

census_api_key("360f925a95819df326a882de79efdd5d653516d2", overwrite = TRUE, install = TRUE)

acs17 <- load_variables(year = 2017, dataset = "acs5", cache = TRUE)

medval_pa <- get_acs(geography = "county", variables = "B25077_001", state = "PA", survey = "acs5")

agpa_val <- get_acs(geography = "tract", variables = "B25077_001", state = "PA", county = "003", option... survey = "acs5")

