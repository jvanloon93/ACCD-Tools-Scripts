library(tidycensus)
library(blscrapeR)
library(tidyverse)
library(usmap)

ten_county <- as.vector(fips('PA', county =  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')))

names(ten_county) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')

#Census_Pull <- tibble(County_Name = as.character(),
                      #GEOID = as.numeric(), 
                      #Total_population = as.numeric(),
                      #Median_Household_income = as.numeric(),
                      #Persons_Below_Poverty_Line = as.numeric(),
                      #PoP_over_25 = as.numeric())

Census_Pull <- get_estimates(
  geography = "county", 
  product = "population",
  state = "PA",
  year = 2017) %>%
  mutate(year = as.character(2017)) %>% 
  rename(Total_Population = value) %>%
  filter(variable == "POP" & GEOID %in% c(ten_county)) %>%
  select(NAME, GEOID, year, Total_Population)


PoP_over_25_vars <- function() {
  
  PoP_over_25_endings <- c()
  
Male_var_endings <- 11:25 

for (i in 1:length(Male_var_endings)) {
  new <- as.character(paste0("B01001_0", Male_var_endings[i]))
  PoP_over_25_endings <- c(PoP_over_25_endings, new)
}

female_var_endings <- 35:49

for (i in 1:length(female_var_endings)) {
  new <- as.character(paste0("B01001_0", female_var_endings[i]))
  PoP_over_25_endings <- c(PoP_over_25_endings, new)
}
return(PoP_over_25_endings)

}

PoP_over_25_vars <- PoP_over_25_vars()

PoP_over_25 <- lapply(seq_along(PoP_over_25_vars), 
                                function(i) get_acs(
                                geography = "county",
                                state = "PA",
                                variables = PoP_over_25_vars[i], 
                                year = 2017, 
                                geometry = FALSE)) %>%
                bind_rows() %>%
                filter(GEOID %in% c(ten_county)) %>% 
                group_by(GEOID) %>% 
                summarise(Total_Pop_over_25 = sum(estimate)) %>%
                select(Total_Pop_over_25)

Median_Household_income <- get_acs(geography = "county", 
                                   state = "PA", 
                                   variables = "B19013_001", 
                                   year = 2017, 
                                   geometry = FALSE) %>%
                            filter(GEOID %in% c(ten_county)) %>%
                            rename(Median_Household_income = estimate) %>%
                            select(Median_Household_income)

Persons_below_poverty_line <- get_acs(geography = "county", 
                                 state = "PA", 
                                 variables = "B17013_001", 
                                 year = 2017, 
                                 geometry = FALSE) %>%
                          filter(GEOID %in% c(ten_county)) %>%
                          rename(Persons_below_poverty_line = estimate) %>%
                          select(Persons_below_poverty_line)
  
HS_Education_vars_over_25 <- c( "B15001_014", "B15001_015", "B15001_016", "B15001_017", "B15001_018", "B15001_022", "B15001_023", "B15001_024",
                                "B15001_025", "B15001_026", "B15001_030", "B15001_031", "B15001_032", "B15001_033", "B15001_034", "B15001_038",
                                "B15001_039", "B15001_040", "B15001_041", "B15001_042", "B15001_055", "B15001_056", "B15001_057", "B15001_058",
                                "B15001_059", "B15001_063", "B15001_064", "B15001_065", "B15001_066", "B15001_067", "B15001_071", "B15001_072",
                                "B15001_073", "B15001_074", "B15001_075", "B15001_079", "B15001_080", "B15001_081", "B15001_082", "B15001_083") 

HS_over_25 <- lapply(seq_along(HS_Education_vars_over_25), 
                                    function(i) get_acs(
                                      geography = "county",
                                      state = "PA",
                                      variables = HS_Education_vars_over_25[i], 
                                      year = 2017, 
                                      geometry = FALSE)) %>%
              bind_rows() %>%
              filter(GEOID %in% c(ten_county)) %>% 
              group_by(GEOID) %>% 
              summarise(HS_over_25 = sum(estimate)) %>%
              select(HS_over_25)


Bachelors_over_25_vars <-  c( "B15001_017", "B15001_018", "B15001_025", "B15001_026", "B15001_033", "B15001_034", "B15001_041", "B15001_042", 
                              "B15001_058", "B15001_059", "B15001_066", "B15001_067", "B15001_074", "B15001_075", "B15001_082", "B15001_083") 

Bachelors_over_25 <- lapply(seq_along(Bachelors_over_25_vars), 
                               function(i) get_acs(
                                 geography = "county",
                                 state = "PA",
                                 variables = Bachelors_over_25_vars[i], 
                                 year = 2017, 
                                 geometry = FALSE)) %>%
              bind_rows() %>%
              filter(GEOID %in% c(ten_county)) %>% 
              group_by(GEOID) %>% 
              summarise(Bachelors_over_25 = sum(estimate)) %>%
              select(Bachelors_over_25)


Census_Pull <- cbind(Census_Pull, Median_Household_income, Persons_below_poverty_line)

#select specfic columns, line population up and 

