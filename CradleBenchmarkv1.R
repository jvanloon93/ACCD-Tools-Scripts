library(tidycensus)
library(tidyverse)
library(ggplot2)
library(usmap)

v17 <- load_variables(2017, "acs5")

v17ACS1 <- load_variables(2017, "acs1")

write.csv(v17, file = "v17.csv")

MSA_Pop <- get_acs(
  geography = "metropolitan statistical area/micropolitan statistical area", 
  variables = "B01001_001", 
  year = 2017, 
  geometry = FALSE)


St.Louis_Pop <- MSA_Pop%>%
  subset(GEOID == 41180)

Pittsburgh_Pop <- MSA_Pop%>%
  subset(GEOID == 38300)

All_Ed_Total <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
                    variables = "B15001_001", 
                    year = 2017, 
                    geometry = FALSE)

St.Louis_Ed_Total<- All_Ed_Total %>%
  subset(GEOID == 41180) 

Pittsburgh_Ed_Total<- All_Ed_Total %>%
  subset(GEOID == 38300)

HS_Education_vars <- c( "B15001_006", "B15001_007", "B15001_008", "B15001_009", "B15001_010", "B15001_014", "B15001_015", "B15001_016", "B15001_017", 
                        "B15001_018", "B15001_022", "B15001_023", "B15001_024", "B15001_025", "B15001_026", "B15001_030", "B15001_031", "B15001_032",
                        "B15001_033", "B15001_034", "B15001_038", "B15001_039", "B15001_040", "B15001_041", "B15001_042", "B15001_047", "B15001_048",
                        "B15001_049", "B15001_050", "B15001_051", "B15001_055", "B15001_056", "B15001_057", "B15001_058", "B15001_059", "B15001_063",
                        "B15001_064", "B15001_065", "B15001_066", "B15001_067", "B15001_071", "B15001_072", "B15001_073", "B15001_074", "B15001_075",
                        "B15001_079", "B15001_080", "B15001_081", "B15001_082", "B15001_083") 

MSA_HS <- bind_rows(lapply(seq_along(HS_Education_vars), 
                           function(i) get_acs(
                             geography = "metropolitan statistical area/micropolitan statistical area", 
                             variables = HS_Education_vars[i], 
                             year = 2016, 
                             geometry = FALSE)
                           )
                    ) 

St.Louis_HS_ED  <- MSA_HS %>%
  subset(GEOID == 41180) 

Pittsburgh_HS_ED <- MSA_HS %>%
  subset(GEOID == 38300)

Assoc_and_Higher_Education_vars <- c( "B15001_008", "B15001_009", "B15001_010", "B15001_016", "B15001_017", "B15001_018", "B15001_024", "B15001_025", "B15001_026",
                        "B15001_032", "B15001_033", "B15001_034", "B15001_040", "B15001_041", "B15001_042", "B15001_049", "B15001_050", "B15001_051",
                        "B15001_057", "B15001_058", "B15001_059", "B15001_065", "B15001_066", "B15001_067", "B15001_073", "B15001_074", "B15001_075",
                        "B15001_081", "B15001_082", "B15001_083") 

MSA_HigherEd <- bind_rows(lapply(seq_along(Assoc_and_Higher_Education_vars), 
                           function(i) get_acs(
                             geography = "metropolitan statistical area/micropolitan statistical area", 
                             variables = Assoc_and_Higher_Education_vars[i], 
                             year = 2017, 
                             geometry = FALSE)
)
) 

St.Louis_HigherED  <- MSA_HigherEd %>%
  subset(GEOID == 41180) 

Pittsburgh_HigherED <- MSA_HigherEd %>%
  subset(GEOID == 38300)
            
Commute_time_Var <- c("B08013_001")            

All_Commute_Time <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
                            variables = Commute_time_Var, 
                            year = 2017, 
                            geometry = FALSE)

St.Louis_Commute <- All_Commute_Time%>%
  subset(GEOID == 41180) 

Pittsburgh_Commute <- All_Commute_Time %>%
  subset(GEOID == 38300)
