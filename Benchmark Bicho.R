install.packages("XLConnect")
install.packages("XLConnectJars")
install.packages("rJava")
library(XLConnect)
library(XLConnectJars)
library(rJava)
library(tidycensus)
library(blscrapeR)
library(tidyverse)
library(usmap)

v17 <- load_variables(2017, "acs5")

book <- loadWorkbook("Benchmarking.xls", create = TRUE)

saveWorkbook(book)

ten_county <- as.vector(fips('PA', county =  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')))

MSA_Fips <- c("35620", "31080", "16980", "19100", "14460", "41860")
CESFIPS <- c("4238300", MSA_Fips_WSate)

CESFIPS_Names <- c("Pittsburgh, PA", "New York-Newark-Jersey City, NY-NJ-PA MSA",
                   "Los Angeles-Long Beach-Anaheim, CA MSA", "Chicago-Naperville-Elgin, IL-IN-WI MSA",
                   "Dallas-Fort Worth-Arlington, TX MSA", "Boston-Cambridge-Newton, MA-NH MSA",
                   "San Francisco-Oakland-Berkeley, CA MSA")

MSA_Fips_WSate <- c("3635620", "0631080", "1716980", "4819100", "2571650", "0641860")
#Boston not a MSA but NECTA 
MSA_Fips_NoState_noZero <- c("3562", "3108", "1698", "1910", "7165", "4186")


PA_Pop<- get_acs(
  geography = "county", 
  state = "PA",
  variables = "B01001_001", 
  year = 2017, 
  geometry = FALSE)

ten_County_pop <- PA_Pop %>%
  subset(GEOID %in% ten_county) %>%
 rename(Resident_Population_Estimate = estimate) 

MSA_Pop <- get_acs(
  geography = "metropolitan statistical area/micropolitan statistical area", 
  variables = "B01001_001", 
  year = 2017, 
  geometry = FALSE)

Bicho_Pop <- MSA_Pop %>%
  subset(GEOID %in% MSA_Fips) %>%
  rename(Resident_Population_Estimate = estimate)

Pop <- rbind(ten_County_pop, Bicho_Pop)

createSheet(book, "Population")
writeWorksheet(book, Pop, sheet = "Population")

All_Ed_Total_MSA <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
                    variables = "B15001_001", 
                    year = 2017, 
                    geometry = FALSE)

All_Ed_Total_MSA <- All_Ed_Total_MSA %>%
  subset(GEOID %in% MSA_Fips) %>%
  rename(All_Education = estimate)

All_Ed_Total_Ten_County <- get_acs(geography = "county",
                                   state = "PA",
                                   variables = "B15001_001", 
                                   year = 2017, 
                                   geometry = FALSE)

All_Ed_Total_Ten_County <- All_Ed_Total_Ten_County %>%
  subset(GEOID %in% ten_county)%>%
  rename(All_Education = estimate)


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

MSA_HS <- MSA_HS %>%
  subset(GEOID %in% MSA_Fips) %>%
  group_by(GEOID) %>%
  summarise(sum(estimate)) %>% 
  rename(High_School_Graduates_or_Higher = "sum(estimate)")

ten_county_HS <- bind_rows(lapply(seq_along(HS_Education_vars), 
                           function(i) get_acs(
                             geography = "county", 
                             state = "PA",
                             variables = HS_Education_vars[i], 
                             year = 2017, 
                             geometry = FALSE)))

ten_county_HS <- ten_county_HS %>%
  subset(GEOID %in% ten_county) %>%
  group_by(GEOID) %>%
  summarise(sum(estimate)) %>%
  rename(High_School_Graduates_or_Higher = "sum(estimate)")

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

MSA_HigherEd <- MSA_HigherEd %>%
  subset(GEOID %in% MSA_Fips) %>%
  group_by(GEOID) %>%
  summarise(sum(estimate)) %>%
  rename(Assoc_and_Higher = "sum(estimate)")

Ten_county_HigherEd <- bind_rows(lapply(seq_along(Assoc_and_Higher_Education_vars), 
                                 function(i) get_acs(
                                   geography = "county",
                                   state = "PA",
                                   variables = Assoc_and_Higher_Education_vars[i], 
                                   year = 2017, 
                                   geometry = FALSE)))

Ten_county_HigherEd <- Ten_county_HigherEd %>%
  subset(GEOID %in% ten_county) %>%
  group_by(GEOID) %>%
  summarise(sum(estimate)) %>%
  rename(Assoc_and_Higher = "sum(estimate)")

Ten_County_ed <- merge(All_Ed_Total_Ten_County, ten_county_HS, by = "GEOID")

Ten_County_ed <- merge(Ten_County_ed, Ten_county_HigherEd, by = "GEOID")

Ten_County_ed <- Ten_County_ed %>%
  mutate(HS_Percentage = High_School_Graduates_or_Higher / All_Education, 
         Assoc_and_Higher_Percentage =  Assoc_and_Higher / All_Education)

createSheet(book, "Ten County Ed All")
writeWorksheet(book, Ten_County_ed, sheet = "Ten County Ed All" )

Bicho_ed <- merge(All_Ed_Total_MSA, MSA_HS)

Bicho_ed <- merge(Bicho_ed, MSA_HigherEd)

Bicho_ed <- Bicho_ed %>%
  mutate(HS_Percentage = High_School_Graduates_or_Higher / All_Education, 
         Assoc_and_Higher_Percentage =  Assoc_and_Higher / All_Education)

createSheet(book, "MSA Ed All")
writeWorksheet(book, Bicho_ed, sheet = "MSA Ed All")

saveWorkbook(book)

Total_18_44_vars <- c("B15001_003", "B15001_011", "B15001_019", "B15001_044", "B15001_052", "B15001_060")

HS_Education_18_to_44_vars <- c("B15001_006", "B15001_007", "B15001_008", "B15001_009", "B15001_010", "B15001_014", "B15001_015", "B15001_016", "B15001_017", 
                                "B15001_018", "B15001_022", "B15001_023", "B15001_024", "B15001_025", "B15001_026", "B15001_047", "B15001_048",
                                "B15001_049", "B15001_050", "B15001_051", "B15001_055", "B15001_056", "B15001_057", "B15001_058", "B15001_059", "B15001_063",
                                "B15001_064", "B15001_065", "B15001_066", "B15001_067")

Higher_Ed_18_to_44_vars <- c("B15001_008", "B15001_009", "B15001_010", "B15001_016", "B15001_017", "B15001_018", "B15001_024", "B15001_025", "B15001_026",
                                "B15001_049", "B15001_050", "B15001_057", "B15001_058", "B15001_059", "B15001_063",
                                "B15001_066", "B15001_067")

Total_18_44_MSA <- bind_rows(lapply(seq_along(Total_18_44_vars), 
                                                function(i) get_acs(
                                                  geography = "metropolitan statistical area/micropolitan statistical area", 
                                                  variables = Total_18_44_vars[i], 
                                                  year = 2017, 
                                                  geometry = FALSE)))

Total_18_44_MSA <- Total_18_44_MSA %>%
  subset(GEOID %in% MSA_Fips) %>%
  group_by(GEOID) %>%
  summarise(sum(estimate)) %>%
  rename(Total_18_44 = "sum(estimate)")

Total_18_44_County <- bind_rows(lapply(seq_along(Total_18_44_vars), 
                                    function(i) get_acs(
                                      geography = "county",
                                      state = "PA",
                                      variables = Total_18_44_vars[i], 
                                      year = 2017, 
                                      geometry = FALSE)))

Total_18_44_County <- Total_18_44_County %>%
  subset(GEOID %in% ten_county) %>%
  group_by(GEOID) %>% 
  summarise(sum(estimate)) %>%
  rename(Total_18_44 = "sum(estimate)")

  

HS_Education_18_to_44_MSA <- bind_rows(lapply(seq_along(HS_Education_18_to_44_vars), 
                                                function(i) get_acs(
                                                  geography = "metropolitan statistical area/micropolitan statistical area", 
                                                  variables = HS_Education_18_to_44_vars[i], 
                                                  year = 2017, 
                                                  geometry = FALSE)))

HS_Education_18_to_44_MSA <- HS_Education_18_to_44_MSA %>%
  subset(GEOID %in% MSA_Fips) %>%
  group_by(GEOID) %>%
  summarise(sum(estimate)) %>%
  rename(HS_Education_18_to_44 = "sum(estimate)")


HS_Education_18_to_44_Ten_County <- bind_rows(lapply(seq_along(HS_Education_18_to_44_vars), 
                                              function(i) get_acs(
                                                geography = "county",
                                                state = "PA",
                                                variables = HS_Education_18_to_44_vars[i], 
                                                year = 2017, 
                                                geometry = FALSE)))

HS_Education_18_to_44_Ten_County <- HS_Education_18_to_44_Ten_County %>%
  subset(GEOID %in% ten_county) %>%
  group_by(GEOID) %>%
  summarise(sum(estimate)) %>%
  rename(HS_Education_18_to_44 = "sum(estimate)")


Higher_Ed_18_to_44_MSA <- bind_rows(lapply(seq_along(Higher_Ed_18_to_44_vars), 
                                                          function(i) get_acs(
                                                            geography = "metropolitan statistical area/micropolitan statistical area", 
                                                            variables = Higher_Ed_18_to_44_vars[i], 
                                                            year = 2017, 
                                                            geometry = FALSE)))

Higher_Ed_18_to_44_MSA <- Higher_Ed_18_to_44_MSA %>%
  subset(GEOID %in% MSA_Fips) %>%
  group_by(GEOID)%>%
  summarise(sum(estimate)) %>%
  rename(Higher_Ed_18_to_44 = "sum(estimate)")
  


Higher_Ed_18_to_44_Ten_County <- bind_rows(lapply(seq_along(Higher_Ed_18_to_44_vars), 
                                           function(i) get_acs(
                                             geography = "county",
                                             state = "PA",
                                             variables = Higher_Ed_18_to_44_vars[i], 
                                             year = 2017, 
                                             geometry = FALSE)))

Higher_Ed_18_to_44_Ten_County <- Higher_Ed_18_to_44_Ten_County %>%
  subset(GEOID %in% ten_county) %>%
  group_by(GEOID) %>%
  summarise(sum(estimate)) %>%
  rename(Higher_Ed_18_to_44 = "sum(estimate)" )

Ten_County_Names <- Ten_County_ed %>%
  select(GEOID, NAME)

Ten_County_ed_18_44 <- merge(Ten_County_Names, Total_18_44_County)
Ten_County_ed_18_44 <- merge(Ten_County_ed_18_44, HS_Education_18_to_44_Ten_County)
Ten_County_ed_18_44 <- merge(Ten_County_ed_18_44, Higher_Ed_18_to_44_Ten_County)

Ten_County_ed_18_44 <- Ten_County_ed_18_44 %>%
  mutate(HS_18_44_Percentage = HS_Education_18_to_44 / Total_18_44 , 
         Assoc_and_Higher_18_44 =  Higher_Ed_18_to_44 / Total_18_44)

Bicho_names <- Bicho_ed %>%
  select(GEOID, NAME)

Bicho_ed_18_44 <- merge(Bicho_names, Total_18_44_MSA)
Bicho_ed_18_44 <- merge(Bicho_ed_18_44, HS_Education_18_to_44_MSA)
Bicho_ed_18_44 <- merge(Bicho_ed_18_44, Higher_Ed_18_to_44_MSA)

Bicho_ed_18_44 <- Bicho_ed_18_44 %>%
  mutate(HS_18_44_Percentage = HS_Education_18_to_44 / Total_18_44 , 
         Assoc_and_Higher_18_44 =  Higher_Ed_18_to_44 / Total_18_44)

createSheet(book, "10-County 18 to 44")
writeWorksheet(book, Ten_County_ed_18_44, sheet = "10-County 18 to 44")

createSheet(book, "MSA Ed 18 to 44")
writeWorksheet(book, Bicho_ed_18_44, sheet = "MSA Ed 18 to 44")

saveWorkbook(book)



LaborForce_MSA_Codes <- as.vector(sapply(seq_along(MSA_Fips_WSate), function(i) (paste('LAUMT', MSA_Fips_WSate[i],'00000006', sep = ""))))

LaborForce_Ten_County_Codes <- as.vector(sapply(seq_along(ten_county), function(i) (paste('LAUCN', ten_county[i],'0000000006', sep = ""))))

MSA_LaborForce <- bind_rows(lapply(seq_along(LaborForce_MSA_Codes), function(i) bls_api(LaborForce_MSA_Codes[i])))

Ten_County_LaborForce <- bind_rows(lapply(seq_along(LaborForce_Ten_County_Codes), function(i) bls_api(LaborForce_Ten_County_Codes[i])))

MSA_LaborForce18 <- MSA_LaborForce %>%
  subset(year == 2018) %>%
  group_by(seriesID) %>%
  summarise(mean(value))

Unemployment_MSA_codes <- as.vector(sapply(seq_along(MSA_Fips), function(i) (paste('LAUMT', MSA_Fips[i],'00000004', sep = ""))))

Unemployment_ten_county_codes <- as.vector(sapply(seq_along(ten_county), function(i) (paste('LAUCN', ten_county[i],'0000000004', sep = ""))))

Employment_MSA_Codes <- as.vector(sapply(seq_along(MSA_Fips_NoState), function(i) (paste('ENUC', MSA_Fips_NoState[i],'10010', sep = ""))))

Employment_Ten_County_Codes <- as.vector(sapply(seq_along(ten_county), function(i) (paste('ENU', ten_county[i],'10010', sep = ""))))
  
Total_Wage_MSA_Codes <- as.vector(sapply(seq_along(MSA_Fips_NoState), function(i) (paste('ENUC', MSA_Fips_NoState[i],'30010', sep = ""))))

Total_Wage_Ten_County_Codes <- as.vector(sapply(seq_along(ten_county), function(i) (paste('ENU', ten_county[i],'30010', sep = ""))))

>>>>>>> ef49d648512001fbcae36e467e91f790756790eb

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

