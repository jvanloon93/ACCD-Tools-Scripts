library(tidycensus)
library(tidyverse)
library(ggplot2)
library(usmap)

ten_county <- as.numeric(as.vector(fips('PA', county =  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland'))))

names(ten_county) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')

ten_county_name <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')

#census_api_key("3dd9ac070b801c90650f35f91996975bc6f84bdd", overwrite = TRUE, install = TRUE)

v17<- load_variables(2017, "acs1")

pop <- bind_rows(lapply(seq_along(ten_county_name), function(i) get_acs(geography = 'county', variables = "B01001_001", state = "PA", county = ten_county_name[i], geometry = FALSE)))
colnames(pop) <- c("GEOID", "County", "variable", "Total_Population", "Margin of Error" )
ggplot(pop, aes(x = County, y = Total_Population))+ geom_col() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

LaborForce <- bind_rows(lapply(seq_along(ten_county_name), function(i) get_acs(geography = 'county', variables = "B10058_002", state = "PA", county = ten_county_name[i], geometry = FALSE)))

pop2 <- bind_rows(lapply(seq_along(ten_county_name), function(i) get_acs(geography = 'county', variables = "B01003_001", state = "PA", county = ten_county_name[i], geometry = FALSE)))
colnames(pop2) <- c("GEOID", "County", "variable", "Total_Population", "Margin of Error" )
ggplot(pop2, aes(x = County, y = Total_Population))+ geom_col() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

v17concept <- v17 %>%
  count(concept)

