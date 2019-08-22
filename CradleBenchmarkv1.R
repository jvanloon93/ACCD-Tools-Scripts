library(tidycensus)
library(blscrapeR)
library(bea.R)
library(tidyverse)
library(ggplot2)
library(usmap)

v17 <- load_variables(2017, "acs5")


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
                             year = 2017, 
                             geometry = FALSE)))

St.Louis_HS_ED  <- MSA_HS %>%
  subset(GEOID == 41180) 

Pittsburgh_HS_ED <- MSA_HS %>%
  subset(GEOID == 38300)

Pittsburgh_HS_Percent <- sum(Pittsburgh_HS_ED$estimate) / sum(Pittsburgh_Ed_Total$estimate)

St.Louis_HS_Percent <- sum(St.Louis_HS_ED$estimate) / sum(St.Louis_Ed_Total$estimate)

Assoc_and_Higher_Education_vars <- c( "B15001_008", "B15001_009", "B15001_010", "B15001_016", "B15001_017", "B15001_018", "B15001_024", "B15001_025", "B15001_026",
                        "B15001_032", "B15001_033", "B15001_034", "B15001_040", "B15001_041", "B15001_042", "B15001_049", "B15001_050", "B15001_051",
                        "B15001_057", "B15001_058", "B15001_059", "B15001_065", "B15001_066", "B15001_067", "B15001_073", "B15001_074", "B15001_075",
                        "B15001_081", "B15001_082", "B15001_083") 

MSA_HigherEd <- bind_rows(lapply(seq_along(Assoc_and_Higher_Education_vars), 
                           function(i) get_acs(
                             geography = "metropolitan statistical area/micropolitan statistical area", 
                             variables = Assoc_and_Higher_Education_vars[i], 
                             year = 2017, 
                             geometry = FALSE)))
 

St.Louis_HigherED  <- MSA_HigherEd %>%
  subset(GEOID == 41180) 

Pittsburgh_HigherED <- MSA_HigherEd %>%
  subset(GEOID == 38300)

Pittsburgh_Higher_Ed_Percent <- sum(Pittsburgh_HigherED$estimate) / sum(Pittsburgh_Ed_Total$estimate)

St.Louis_Higher_Ed_Percent <- sum(St.Louis_HigherED$estimate) / sum(St.Louis_Ed_Total$estimate)


Total_18_44_vars <- c("B15001_003", "B15001_011", "B15001_019", "B15001_044", "B15001_052", "B15001_060")

HS_Education_18_to_44_vars <- c("B15001_006", "B15001_007", "B15001_008", "B15001_009", "B15001_010", "B15001_014", "B15001_015", "B15001_016", "B15001_017", 
                                "B15001_018", "B15001_022", "B15001_023", "B15001_024", "B15001_025", "B15001_026", "B15001_047", "B15001_048",
                                "B15001_049", "B15001_050", "B15001_051", "B15001_055", "B15001_056", "B15001_057", "B15001_058", "B15001_059", "B15001_063",
                                "B15001_064", "B15001_065", "B15001_066", "B15001_067")

Higher_Ed_18_to_44_vars <- c("B15001_008", "B15001_009", "B15001_010", "B15001_016", "B15001_017", "B15001_018", "B15001_024", "B15001_025", "B15001_026",
                                "B15001_049", "B15001_050", "B15001_057", "B15001_058", "B15001_059", "B15001_063",
                                "B15001_066", "B15001_067")

Total_18_44 <- bind_rows(lapply(seq_along(Total_18_44_vars), 
                                                function(i) get_acs(
                                                  geography = "metropolitan statistical area/micropolitan statistical area", 
                                                  variables = Total_18_44_vars[i], 
                                                  year = 2017, 
                                                  geometry = FALSE)))
St.Louis_18_to_44  <- Total_18_44 %>%
  subset(GEOID == 41180) 

Pittsburgh_18_to_44<- Total_18_44%>%
  subset(GEOID == 38300)

HS_Education_18_to_44 <- bind_rows(lapply(seq_along(HS_Education_18_to_44_vars), 
                                                function(i) get_acs(
                                                  geography = "metropolitan statistical area/micropolitan statistical area", 
                                                  variables = HS_Education_18_to_44_vars[i], 
                                                  year = 2017, 
                                                  geometry = FALSE)))

St.Louis_HS_ED_18_to_44  <- HS_Education_18_to_44 %>%
  subset(GEOID == 41180) 

Pittsburgh_HS_ED_18_to_44 <- HS_Education_18_to_44%>%
  subset(GEOID == 38300)

Pittsburgh_HS_Ed_18_to_44_Percent <- sum(Pittsburgh_HS_ED_18_to_44$estimate)/sum(Pittsburgh_18_to_44$estimate)

St.Louis_HS_ED_18_to_44_Percent <- sum(St.Louis_HS_ED_18_to_44$estimate)/sum(St.Louis_18_to_44$estimate)

Higher_Ed_18_to_44 <- bind_rows(lapply(seq_along(Higher_Ed_18_to_44_vars), 
                                                          function(i) get_acs(
                                                            geography = "metropolitan statistical area/micropolitan statistical area", 
                                                            variables = Higher_Ed_18_to_44_vars[i], 
                                                            year = 2017, 
                                                            geometry = FALSE)))

St.Louis_HigherED_18_to_44  <- Higher_Ed_18_to_44 %>%
  subset(GEOID == 41180) 

Pittsburgh_HigherED_18_to_44 <- Higher_Ed_18_to_44%>%
  subset(GEOID == 38300)

Pittsburgh_HigherED_18_to_44_Percent <- sum(Pittsburgh_HigherED_18_to_44$estimate) / sum(Pittsburgh_18_to_44$estimate)

St.Louis_HigherED_18_to_44_Percent <- sum(St.Louis_HigherED_18_to_44$estimate) / sum(St.Louis_18_to_44$estimate)


Pittsburgh_Pop$estimate

Pittsburgh_HS_Percent

Pittsburgh_Higher_Ed_Percent

Pittsburgh_HS_Ed_18_to_44_Percent

Pittsburgh_HigherED_18_to_44_Percent

St.Louis_Pop$estimate

St.Louis_HS_Percent

St.Louis_Higher_Ed_Percent

St.Louis_HS_ED_18_to_44_Percent

St.Louis_HigherED_18_to_44_Percent

MSA_Fips <- c("4238300", "2941180")
MSA_Fips_NoState <- c("3830", "4118")


LaborForce_MSA_Codes <- as.vector(sapply(seq_along(MSA_Fips), function(i) (paste('LAUMT', MSA_Fips[i],'00000006', sep = ""))))

Pittsburgh_LaborForce <- bls_api(c(LaborForce_MSA_Codes[1]))

St.Louis_LaborForce <- bls_api(c(LaborForce_MSA_Codes[2]))

PITLF18 <- Pittsburgh_LaborForce %>%
  filter(year == 2018) 

PITLF18 <- mean(PITLF18$value)

PITLF17 <- Pittsburgh_LaborForce %>%
  filter(year == 2017)

PITLF17 <- mean(PITLF17$value)

STLLF18 <- St.Louis_LaborForce %>%
  filter(year == 2018)

STLLF18 <- mean(STLLF18$value)

STLF17 <- St.Louis_LaborForce %>% 
  filter(year == 2017)

STLFL17 <- mean(STLF17$value)

Unemployment_MSA_codes <- as.vector(sapply(seq_along(MSA_Fips), function(i) (paste('LAUMT', MSA_Fips[i],'00000004', sep = ""))))

Pittsburgh_Unemployment <-bls_api(c(Unemployment_MSA_codes[1]))

St.Louis_Unemployment <- bls_api(c(Unemployment_MSA_codes[2]))

PITUN18 <- Pittsburgh_Unemployment %>%
  filter(year == 2018)

PITUN18 <- mean(PITUN18$value)

PITUN17 <- Pittsburgh_Unemployment %>%
  filter(year == 2017)

PITUN17 <- mean(PITUN17$value)

STLUN18 <- St.Louis_Unemployment %>%
  filter(year == 2018)

STLUN18 <- mean(STLUN18$value)

STLUN17 <- St.Louis_Unemployment %>%
  filter(year == 2017)

STLUN17 <- mean(STLUN17$value)

Employment_MSA_Codes <- as.vector(sapply(seq_along(MSA_Fips_NoState), function(i) (paste('ENUC', MSA_Fips_NoState[i],'10010', sep = ""))))

Total_Wage_MSA_Codes <- as.vector(sapply(seq_along(MSA_Fips_NoState), function(i) (paste('ENUC', MSA_Fips_NoState[i],'30010', sep = ""))))

Pittsburgh_Employment <- bls_api(c(Employment_MSA_Codes[1]))

Pittsburgh_Total_Wages <- bls_api(c(Total_Wage_MSA_Codes[1]))

St.Louis_Employment <- bls_api(c(Employment_MSA_Codes[2]))

St.Louis_Total_Wages <- bls_api(c(Total_Wage_MSA_Codes[2]))

EmploymentPTS_MSA_Codes <- as.vector(sapply(seq_along(MSA_Fips_NoState), function(i) (paste('ENUC', MSA_Fips_NoState[i],'10554', sep = ""))))

Total_WagPTSe_MSA_Codes <- as.vector(sapply(seq_along(MSA_Fips_NoState), function(i) (paste('ENUC', MSA_Fips_NoState[i],'30554', sep = ""))))

PITPTS_Employment <- bls_api(c(EmploymentPTS_MSA_Codes[1]))

PITPTS_Total_wages <- bls_api(c(Total_WagPTSe_MSA_Codes[1]))

STLPTS_Employment <- bls_api(c(EmploymentPTS_MSA_Codes[2]))

STLPTS_Total_Wages <- bls_api(c(Total_WagPTSe_MSA_Codes[2]))

PITLF18
PITLF17
(PITLF18-PITLF17)/PITLF17

STLLF18
STLFL17
(STLLF18- STLFL17)/STLFL17

#With CEW back online



CES_Endings <- c("0000000001", "1500000001", "3000000001", "4000000001", "5000000001", "5500000001", "6000000001", "6500000001", "7000000001", "8000000001", "9000000001")

CES_Names <- c("Total NonFarm", "Mining, Logging, and Construction", "Manfacturing", "Trade, Transportation, and Utilities", "Information", "Financial Activites", 
               "Professional and Business Services", "Education and Health Services", "Leisure and Hospitality", "Other Services", "Government")

PIT_CES_Codes <- as.vector(sapply(seq_along(CES_Endings), function(i) (paste('SMU', MSA_Fips[1], CES_Endings[i], sep = ""))))

names(PIT_CES_Codes) <- CES_Names

STL_CES_COdes <- as.vector(sapply(seq_along(CES_Endings), function(i) (paste('SMU', MSA_Fips[2], CES_Endings[i], sep = ""))))

names(STL_CES_COdes) <- CES_Names

PIT_NonFarm <- lapply(seq_along(PIT_CES_Codes), function(i) bls_api(PIT_CES_Codes[i]))

STL_NonFarm <- lapply(seq_along(STL_CES_COdes), function(i) bls_api(STL_CES_COdes[i])) 




MSA_Housing_As_Percent_of_income <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
                        variables = "B25092_001", 
                        year = 2017, 
                        geometry = FALSE)

St.Louis_Housing <- MSA_Housing_As_Percent_of_income%>%
  subset(GEOID == 41180) 

Pittsburgh_Housing<- MSA_Housing_As_Percent_of_income %>%
  subset(GEOID == 38300)

Pittsburgh_Housing$estimate

St.Louis_Housing$estimate

