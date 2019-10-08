library(tidycensus)
library(blscrapeR)
library(tidyverse)
library(usmap)

ten_county <- as.vector(fips('PA', county =  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')))

names(ten_county) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')

Census <- County_profile_Census_Pull(ten_county, Estimates_year = 2018, ACS_year = 2017, dataset = "acs5")

BLS <- UN_LF_County_Pull(ten_county, 2018)

