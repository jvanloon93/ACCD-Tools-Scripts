library(tidycensus)
library(blscrapeR)
library(tidyverse)
library(usmap)


v <- load_variables(2018, "acs1")

County_profile_Census_Pull <- function(county_fips, Estimates_year, ACS_year, dataset){ 
  
Census_Pull <- get_estimates(
  geography = "county", 
  product = "population",
  state = "PA",
  year = Estimates_year) %>%
  mutate(year = as.character(Estimates_year)) %>% 
  rename(Total_Population = value) %>%
  filter(variable == "POP" & GEOID %in% c(county_fips)) %>%
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
                                year = ACS_year, 
                                survey = dataset,
                                geometry = FALSE)) %>%
                bind_rows() %>%
                filter(GEOID %in% c(county_fips)) %>% 
                group_by(GEOID) %>% 
                summarise(Total_Pop_over_25 = sum(estimate)) %>%
                select(Total_Pop_over_25)

Median_Household_income <- get_acs(geography = "county", 
                                   state = "PA", 
                                   variables = "B19013_001", 
                                   year = ACS_year,
                                   survey = dataset,
                                   geometry = FALSE) %>%
                            filter(GEOID %in% c(county_fips)) %>%
                            rename(Median_Household_income = estimate) %>%
                            select(Median_Household_income)

Persons_below_poverty_line <- get_acs(geography = "county", 
                                 state = "PA", 
                                 variables = "B17001_002", 
                                 year = ACS_year, 
                                 survey = dataset,
                                 geometry = FALSE) %>%
                          filter(GEOID %in% c(county_fips)) %>%
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
                                      year = ACS_year, 
                                      survey = dataset,
                                      geometry = FALSE)) %>%
              bind_rows() %>%
              filter(GEOID %in% c(county_fips)) %>% 
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
                                 year = ACS_year,
                                 survey = dataset,
                                 geometry = FALSE)) %>%
              bind_rows() %>%
              filter(GEOID %in% c(county_fips)) %>% 
              group_by(GEOID) %>% 
              summarise(Bachelors_over_25 = sum(estimate)) %>%
              select(Bachelors_over_25)


Census_Pull <- cbind(Census_Pull, PoP_over_25, HS_over_25, Bachelors_over_25)

Census_Pull <- Census_Pull %>%
  mutate(HS_percentage = HS_over_25 / Total_Pop_over_25, Bach_percentage = Bachelors_over_25/Total_Pop_over_25)

return(Census_Pull <- cbind(Census_Pull, Median_Household_income, Persons_below_poverty_line))

}

City_Census_Pull <- function(city_code, Estimates_year, ACS_year, dataset){ 
  
  Census_Pull <- get_estimates(
    geography = "place", 
    product = "population",
    state = "PA",
    year = Estimates_year) %>%
    mutate(year = as.character(Estimates_year)) %>% 
    rename(Total_Population = value) %>%
    filter(variable == "POP" & GEOID %in% c(city_code)) %>%
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
                          geography = "place",
                          state = "PA",
                          variables = PoP_over_25_vars[i], 
                          year = ACS_year, 
                          survey = dataset,
                          geometry = FALSE)) %>%
    bind_rows() %>%
    filter(GEOID %in% c(city_code)) %>% 
    group_by(GEOID) %>% 
    summarise(Total_Pop_over_25 = sum(estimate)) %>%
    select(Total_Pop_over_25)
  
  Median_Household_income <- get_acs(geography = "place", 
                                     state = "PA", 
                                     variables = "B19013_001", 
                                     year = ACS_year,
                                     survey = dataset,
                                     geometry = FALSE) %>%
    filter(GEOID %in% c(city_code)) %>%
    rename(Median_Household_income = estimate) %>%
    select(Median_Household_income)
  
  Persons_below_poverty_line <- get_acs(geography = "place", 
                                        state = "PA", 
                                        variables = "B17001_002", 
                                        year = ACS_year, 
                                        survey = dataset,
                                        geometry = FALSE) %>%
    filter(GEOID %in% c(city_code)) %>%
    rename(Persons_below_poverty_line = estimate) %>%
    select(Persons_below_poverty_line)
  
  HS_Education_vars_over_25 <- c( "B15001_014", "B15001_015", "B15001_016", "B15001_017", "B15001_018", "B15001_022", "B15001_023", "B15001_024",
                                  "B15001_025", "B15001_026", "B15001_030", "B15001_031", "B15001_032", "B15001_033", "B15001_034", "B15001_038",
                                  "B15001_039", "B15001_040", "B15001_041", "B15001_042", "B15001_055", "B15001_056", "B15001_057", "B15001_058",
                                  "B15001_059", "B15001_063", "B15001_064", "B15001_065", "B15001_066", "B15001_067", "B15001_071", "B15001_072",
                                  "B15001_073", "B15001_074", "B15001_075", "B15001_079", "B15001_080", "B15001_081", "B15001_082", "B15001_083") 
  
  HS_over_25 <- lapply(seq_along(HS_Education_vars_over_25), 
                       function(i) get_acs(
                         geography = "place",
                         state = "PA",
                         variables = HS_Education_vars_over_25[i], 
                         year = ACS_year, 
                         survey = dataset,
                         geometry = FALSE)) %>%
    bind_rows() %>%
    filter(GEOID %in% c(city_code)) %>% 
    group_by(GEOID) %>% 
    summarise(HS_over_25 = sum(estimate)) %>%
    select(HS_over_25)
  
  
  Bachelors_over_25_vars <-  c( "B15001_017", "B15001_018", "B15001_025", "B15001_026", "B15001_033", "B15001_034", "B15001_041", "B15001_042", 
                                "B15001_058", "B15001_059", "B15001_066", "B15001_067", "B15001_074", "B15001_075", "B15001_082", "B15001_083") 
  
  Bachelors_over_25 <- lapply(seq_along(Bachelors_over_25_vars), 
                              function(i) get_acs(
                                geography = "place",
                                state = "PA",
                                variables = Bachelors_over_25_vars[i], 
                                year = ACS_year,
                                survey = dataset,
                                geometry = FALSE)) %>%
    bind_rows() %>%
    filter(GEOID %in% c(city_code)) %>% 
    group_by(GEOID) %>% 
    summarise(Bachelors_over_25 = sum(estimate)) %>%
    select(Bachelors_over_25)
  
  
  Census_Pull <- cbind(Census_Pull, PoP_over_25, HS_over_25, Bachelors_over_25)
  
  Census_Pull <- Census_Pull %>%
    mutate(HS_percentage = HS_over_25 / Total_Pop_over_25, Bach_percentage = Bachelors_over_25/Total_Pop_over_25)
  
  return(Census_Pull <- cbind(Census_Pull, Median_Household_income, Persons_below_poverty_line))
  
}

#https://pitt.libguides.com/uscensus/pghcensustracts

SQH <- get_acs(geography = "tract", 
               variables = "B01001_001",
               state = "PA", 
               county = "Allegheny", 
               year = 2017, 
               survey = "acs5")

SQH <- separate(SQH, NAME, into = c("Tract", "County"), sep = ",")

SQH$Tract <- substr(SQH$Tract, 13, 18)

SQH$Tract <- as.integer(SQH$Tract)

SQH_var <- c(1401, 1402, 1403, 1408, 1413, 1414, 9803, 9805)

SQH <- SQH %>%
  filter(Tract %in% SQH_var)

sum(SQH$estimate)