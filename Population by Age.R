install.packages("reshape2")
library(tidycensus)
library(blscrapeR)
library(tidyverse)
library(usmap)
library(reshape2)


Pop_by_age <- function(year) {

v <- load_variables(year, "acs1")

Nation <- get_estimates(geography = "us", 
                        product = "characteristics", 
                        breakdown = "AGEGROUP",
                        breakdown_labels = TRUE, 
                        year = year)

PA <- get_estimates(geography = "state",
                    product = "characteristics",
                    breakdown = "AGEGROUP",
                    breakdown_labels = TRUE,
                    year = year
) %>% filter(GEOID == 42)

# MSA <- get_estimates(geography = "metropolitan statistical area/micropolitan statistical area",
#                     # state = "42",
#                      product = "characteristics",
#                      breakdown = "AGEGROUP",
#                      breakdown_labels = TRUE,
#                      year = year) 
#MSAs not available of the API... yet

Census_ten_county <- c("423", "425", "427", "4219", "4251", "4259", "4263", "4273", "42125", "42129")

Counties <-  get_estimates(geography = "county",
                         state = "42",
                         product = "characteristics",
                         breakdown = "AGEGROUP",
                         breakdown_labels = TRUE,
                         year = year) %>% filter(GEOID %in% Census_ten_county)

Counties <- dcast(Counties, NAME ~ AGEGROUP, value.var = "value")

Acs_Pop_Vars <- c("B01001_001", "B01001_002", "B01001_003", "B01001_004", "B01001_005", "B01001_006", "B01001_007", "B01001_008",
                  "B01001_009", "B01001_010", "B01001_011", "B01001_012", "B01001_013", "B01001_014", "B01001_015", "B01001_016",
                  "B01001_017", "B01001_018", "B01001_019", "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024",
                  "B01001_025", "B01001_026", "B01001_027", "B01001_026", "B01001_027", "B01001_028", "B01001_029", "B01001_030",
                  "B01001_031", "B01001_033", "B01001_034", "B01001_035", "B01001_036", "B01001_037", "B01001_038", "B01001_039",
                  "B01001_040", "B01001_041", "B01001_042", "B01001_043", "B01001_044", "B01001_045", "B01001_046", "B01001_047", 
                  "B01001_048", "B01001_049", "B01002_001")

City <- get_acs( geography = "place",
                 variables = Acs_Pop_Vars, 
                 year = year,
                 survey = "acs1") %>% filter(GEOID == 4261000)

City <- left_join(City, v, by = c("variable" = "name")) %>% 
  select(GEOID, NAME, label, estimate)

lst <- list(Nation, PA, Counties, City)

names(lst) <- c("Nation", "PA", "Counties", "City")

return(lst)

}

lst <- Pop_by_age(2018)

