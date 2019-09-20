library(XLConnect)
library(XLConnectJars)
library(rJava)
library(tidycensus)
library(blscrapeR)
library(tidyverse)
library(usmap)

v17 <- load_variables(2017, "acs5")

#define Data 
PA_Fips <- as.vector(fips('PA', county =  c('Lawrence', 'Butler', 'Armstrong', 'Indiana', 'Allegheny', 'Beaver', 'Westmoreland', 'Washington', 'Fayette', 'Greene',
                                            'Venango', 'Mercer', 'Erie', 'Crawford', 'Warren', 'McKean', 'Elk', 'Clearfield', 'Jefferson', 'Clarion', 'Cambria', 'Blair',
                                            'Bedford', 'Somerset')))

PA_Names <- c('Lawrence', 'Butler', 'Armstrong', 'Indiana', 'Allegheny', 'Beaver', 'Westmoreland', 'Washington', 'Fayette', 'Greene',
                    'Venango', 'Mercer', 'Erie', 'Crawford', 'Warren', 'McKean', 'Elk', 'Clearfield', 'Jefferson', 'Clarion', 'Cambria', 'Blair',
                    'Bedford', 'Somerset')

PA_counties <- data.frame(PA_Fips, PA_Names)

PA_counties <- PA_counties %>%
  mutate(State = "Pennsylvania") %>%
  rename(FIPS = PA_Fips, County = PA_Names)

WV_Fips <- as.vector(fips('WV', county = c('Monongalia', 'Marion', 'Wetzel', 'Preston', 'Ohio', 'Marshall', 'Brooke', 'Hancock', 'Tyler')))

WV_Names <- c('Monogolia', 'Marion', 'Wetzel', 'Preston', 'Ohio', 'Marshall', 'Brooke', 'Hancock', 'Tyler')

WV_counties <- data.frame(WV_Fips, WV_Names)

WV_counties <- WV_counties %>%
  mutate(State = "West_Virginia") %>%
  rename(FIPS = WV_Fips, County = WV_Names)

OH_Fips <- as.vector(fips('OH', county = c('Cuyahoga', 'Lake', 'Geauga', 'Ashtabula', 'Trumbull', 'Mahoning', 'Medina', 'Portage', 'Summit', 'Wayne', 
                                 'Stark', 'Columbiana', 'Holmes', 'Coshocton', 'Muskingum', 'Belmont', 'Tuscarawas', 'Carroll', 'Jefferson',
                                 'Harrison', 'Guernsey', 'Noble', 'Monroe', 'Washington', 'Morgan')))

OH_names <- c('Cuyahoga', 'Lake', 'Geauga', 'Ashtabula', 'Trumbull', 'Mahoning', 'Medina', 'Portage', 'Summit', 'Wayne', 
                    'Stark', 'Columbiana', 'Holmes', 'Coshocton', 'Muskingum', 'Belmont', 'Tuscarawas', 'Carroll', 'Jefferson',
                    'Harrison', 'Guernsey', 'Noble', 'Monroe', 'Washington', 'Morgan')

OH_counties <- data.frame(OH_Fips, OH_names)

OH_counties <- OH_counties %>%
  mutate(State = "Ohio") %>%
  rename(FIPS = OH_Fips, County = OH_names)

county_fips <- c(PA_Fips, WV_Fips, OH_Fips)

dfcounties <- rbind(PA_counties, WV_counties, OH_counties) 

dfcounties <- dfcounties %>%
  mutate(placeholder = 1:58)

Xwalk <- read.csv(url("https://www.nber.org/cbsa-msa-fips-ssa-county-crosswalk/2017/cbsatocountycrosswalk2017.csv"))
 
Xwalk <- Xwalk %>%
  rename(FIPS = fipscounty) %>%
  select(FIPS, cbsa, cbsaname)

dfMSAs <- merge(Xwalk, dfcounties)

dfMSAs <- dfMSAs %>%
  arrange(placeholder)

dft <- data.frame(MSAs, MSA_Names)

MSAs <- c("38300", "21500", "27780", "11020", "49660", "48540", "48260", "17460", "15940", "10420", "34060")

MSA_Names <- c("Pittsburgh, PA", 'Erie, PA', 'Johnstown, PA', "Altoona, PA", 'Youngstown-Warren-Boardman, OH-PA', "Wheeling, WV-OH", "Weirton-Steubenville, WV-OH", "Cleveland-Elyria, OH", "Canton-Massillon, OH", "Akron, OH", 'Morgantown, WV' )

MSA_less_johnstown <- MSAs[-3]

#set up excel target 

book <- loadWorkbook("Bluebird.xlsx", create = TRUE)

#Census

Pop <- get_estimates(geography = "county", product = "population", year = 2018)

Pop <- Pop %>%
  filter(GEOID %in% county_fips & variable == 'POP') %>%
  select(GEOID, value) %>%
  rename(FIPS = GEOID, Total_Population_Estimate = value)

Age_vars <- as.data.frame(v17[9:14,], v17[33:38,])

Pop_Age <- bind_rows(lapply(seq_along(Age_vars$name), 
                            function(i) get_acs(
                              geography = 'county', 
                              variables = Age_vars$name[i], 
                              year = 2017, 
                              geometry = FALSE)))

Pop_Age <- Pop_Age %>%
  filter(GEOID %in% county_fips) %>%
  group_by(GEOID) %>%
  summarise(sum(estimate)) %>%
  rename(FIPS = GEOID, Population_18_to_34 = "sum(estimate)") %>%
  select(FIPS, Population_18_to_34)

Ed_total_vars_25_65 <- as.data.frame(rbind(v17[8006,],v17[8014,], v17[8022,], v17[8047,], v17[8055,], v17[8063,]))

HS_Total_vars_25_65 <- as.data.frame(rbind(v17[8009:8013,], v17[8017:8021,], v17[8025:8029,], v17[8050:8054,], v17[8058:8062,], v17[8066:8070,]))

Bach_Total_Vars_25_65 <- as.data.frame(rbind(v17[8012:8013,], v17[8020:8021,], v17[8028:8029,], v17[8053:8054,], v17[8061:8062,], v17[8069:8070,]))


Ed_total_25_65 <- bind_rows(lapply(seq_along(Ed_total_vars_25_65$name), 
                             function(i) get_acs(
                               geography = 'county', 
                               variables = Ed_total_vars_25_65$name[i], 
                               year = 2017, 
                               geometry = FALSE)))

Ed_total_25_65 <- Ed_total_25_65 %>%
  filter(GEOID %in% county_fips) %>%
  group_by(GEOID) %>%
  summarise(sum(estimate)) %>%
  rename(FIPS = GEOID, Ed_total_25_65= "sum(estimate)") %>%
  select(FIPS, Ed_total_25_65)
  

HS_Total_25_65 <- bind_rows(lapply(seq_along(HS_Total_vars_25_65$name), 
                                     function(i) get_acs(
                                       geography = 'county', 
                                       variables = HS_Total_vars_25_65$name[i], 
                                       year = 2017, 
                                       geometry = FALSE)))
HS_Total_25_65 <- HS_Total_25_65 %>%
  filter(GEOID %in% county_fips) %>%
  group_by(GEOID) %>%
  summarise(sum(estimate)) %>%
  rename(FIPS = GEOID, HS_Total_25_65 = "sum(estimate)") %>%
  select(FIPS, HS_Total_25_65)



Bach_Total_25_65 <- bind_rows(lapply(seq_along(Bach_Total_Vars_25_65$name), 
                                     function(i) get_acs(
                                       geography = 'county', 
                                       variables = Bach_Total_Vars_25_65$name[i], 
                                       year = 2017, 
                                       geometry = FALSE)))

Bach_Total_25_65 <- Bach_Total_25_65 %>%
  filter(GEOID %in% county_fips) %>%
  group_by(GEOID) %>%
  summarise(sum(estimate)) %>%
  rename(FIPS = GEOID, Bach_Total_25_65 = "sum(estimate)") %>%
  select(FIPS, Bach_Total_25_65)

df <- merge(dfcounties, Pop)

df<- merge(df, Pop_Age)

df <- merge(df, Ed_total_25_65)

df <- merge(df, HS_Total_25_65)

df <- merge(df, Bach_Total_25_65)

df <- df %>%
  mutate(HS_Percentage = HS_Total_25_65/Ed_total_25_65, Bach_Percentage = Bach_Total_25_65/Ed_total_25_65) %>%
  arrange(placeholder)

Median_Household_income  <- get_acs(geography = "county", variables = "B19013_001", year = 2017)

Median_Household_income <- Median_Household_income %>%
  filter(GEOID %in% county_fips) %>%
  rename(Median_Household_Income = estimate, FIPS = GEOID) %>%
  select(FIPS, Median_Household_Income)


# Begin BLS Pull 

Unemployment_Codes <- as.vector(sapply(seq_along(county_fips), function(i) (paste('LAUCN',county_fips[i],'0000000004', sep = ""))))

Labor_Force_Codes <- as.vector(sapply(seq_along(county_fips), function(i) (paste('LAUCN',county_fips[i],'0000000006', sep = ""))))

Unemployment <- bind_rows(lapply(seq_along(Unemployment_Codes), 
                       function(i) bls_api(
                         Unemployment_Codes[i],
                         startyear = 2018,
                         endyear = 2018,
                         registrationKey = Sys.getenv("BLS_KEY")
                       )))

Unemployment <- Unemployment %>%
  filter(year == 2018) %>%
  group_by(seriesID) %>%
  summarise(mean(value)) %>% 
  rename(Annual_Unemployment = "mean(value)")

Labor_Force <- bind_rows(lapply(seq_along(Labor_Force_Codes), 
                                 function(i) bls_api(
                                   Labor_Force_Codes[i],
                                   startyear = 2018,
                                   endyear = 2018,
                                   registrationKey = Sys.getenv("BLS_KEY")
                                 )))

Labor_Force <- Labor_Force %>%
  filter(year == 2018) %>%
  group_by(seriesID) %>%
  summarise(mean(value)) %>% 
  rename(Annual_Laborforce = "mean(value)")

#series ID errors

seriesID <- Unemployment$seriesID

key <- as.numeric(sapply(seq_along(seriesID), function(i) substring(seriesID[i], 6, 10)))

UN_LF <- data.frame(key, Unemployment$Annual_Unemployment, Labor_Force$Annual_Laborforce)

UN_LF <- UN_LF %>%
  rename(FIPS = key)

#Gluetogether

df <- merge(df, UN_LF)

df <- df %>%
  mutate(Unemployment_Rate = Unemployment.Annual_Unemployment / Labor_Force.Annual_Laborforce)

df <- merge(df, Median_Household_income)

createSheet(book, "Demographics and Employment")

writeWorksheet(book, df, "Demographics and Employment")


# Workforce Compostion 

MSAs_CEW <- sapply(seq_along(MSAs), function(i) paste0("C", substring(MSAs[i], 1, 4)))

cew <- bind_rows(lapply(seq_along(MSAs_CEW), function(i) qcew_api(year = 2018, qtr = "a", slice = "area", sliceCode = MSAs_CEW[i])))

TC <- cew %>%
  filter(own_code == 0) %>%
  select(area_fips, annual_avg_emplvl) %>%
  rename(Total_Covered = annual_avg_emplvl)

N <- cew %>%
  filter(agglvl_code == 43 & own_code == 5) %>%
  select(area_fips,industry_code, annual_avg_emplvl)

TCN <- merge(N, TC)

TCN <- TCN %>%
  mutate(Industry_Percentage = annual_avg_emplvl/Total_Covered)

# Begin by MSA Pull 

OES_Warehouse_Manager_Codes<- as.vector(sapply(seq_along(MSA_less_johnstown), function(i) (paste('OEUM', "00", MSA_less_johnstown[i],'000000', '113071', '03', sep = ""))))

OES_Supervisor_Manager_Codes<- as.vector(sapply(seq_along(MSAs), function(i) (paste('OEUM', "00", MSAs[i],'000000','511011','03', sep = ""))))

OES_Warehouse_Worker_Codes<- as.vector(sapply(seq_along(MSAs), function(i) (paste('OEUM', "00", MSAs[i],'000000','537061', '03', sep = ""))))

OES_Forklift_Operator_Codes<- as.vector(sapply(seq_along(MSAs), function(i) (paste('OEUM', "00", MSAs[i],'000000','537051','03', sep = ""))))

OES_Warehouse_Manager <- bind_rows(lapply(seq_along(OES_Warehouse_Manager_Codes), function(i) bls_api(OES_Warehouse_Manager_Codes[i], registrationKey = Sys.getenv("BLS_Key"))))

OES_Warehouse_Manager <- OES_Warehouse_Manager %>%
  mutate(MSA = as.numeric(MSAs[-3]),  Occupation = "Warehouse Manager")

OES_Supervisor_Manager <- bind_rows(lapply(seq_along(OES_Supervisor_Manager_Codes), function(i) bls_api(OES_Supervisor_Manager_Codes[i], registrationKey = Sys.getenv("BLS_Key"))))

OES_Supervisor_Manager <- OES_Supervisor_Manager %>%
  mutate(MSA = as.numeric(MSAs), Occupation = "Supervisor Manager")

OES_Warehouse_Worker<- bind_rows(lapply(seq_along(OES_Warehouse_Worker_Codes), function(i) bls_api(OES_Warehouse_Worker_Codes[i], registrationKey = Sys.getenv("BLS_Key"))))

OES_Warehouse_Worker <- OES_Warehouse_Worker %>%
  mutate(MSA = as.numeric(MSAs), Occupation = "Warehouse Worker" )

OES_Forklift_Operator <-  bind_rows(lapply(seq_along(OES_Forklift_Operator_Codes), function(i) bls_api(OES_Forklift_Operator_Codes[i], registrationKey = Sys.getenv("BLS_Key"))))

OES_Forklift_Operator <- OES_Forklift_Operator %>%
  mutate(MSA = as.numeric(MSAs), Occupation = "Forklift Operator")

OES <- rbind(OES_Warehouse_Manager, OES_Warehouse_Worker, OES_Supervisor_Manager, OES_Forklift_Operator)

OES <- OES %>%
  rename(Hourly_Wage = value) 

OES <- select(OES, MSA, Hourly_Wage, Occupation)

#Blue Collar Percentage

All_Occ_Codes <- as.vector(sapply(seq_along(MSAs), function(i) (paste('OEUM', "00", MSAs[i],'000000', '000000', '01', sep = ""))))

CE_Occ_Codes <- as.vector(sapply(seq_along(MSAs), function(i) (paste('OEUM', "00", MSAs[i],'000000', '470000', '01', sep = ""))))

IMR__Occ_Codes <- as.vector(sapply(seq_along(MSAs), function(i) (paste('OEUM', "00", MSAs[i],'000000','490000','01', sep = ""))))

P_Occ_Codes <- as.vector(sapply(seq_along(MSAs), function(i) (paste('OEUM', "00", MSAs[i],'000000','510000', '01', sep = ""))))

TMM_Occ_Codes<- as.vector(sapply(seq_along(MSAs), function(i) (paste('OEUM', "00", MSAs[i],'000000','530000','01', sep = ""))))

All_Occ <- bind_rows(lapply(seq_along(All_Occ_Codes), function(i) bls_api(All_Occ_Codes[i], registrationKey = Sys.getenv("BLS_Key"))))

All_Occ <- All_Occ %>%
  mutate(as.numeric(MSAs))

CE_OCC <- bind_rows(lapply(seq_along(CE_Occ_Codes), function(i) bls_api(CE_Occ_Codes[i], registrationKey = Sys.getenv("BLS_Key"))))
CE_OCC <- CE_OCC %>% 
  mutate(as.numeric(MSAs))
  
IMR_OCC <- bind_rows(lapply(seq_along(IMR__Occ_Codes), function(i) bls_api(IMR__Occ_Codes[i], registrationKey = Sys.getenv("BLS_Key"))))
IMR_OCC <- IMR_OCC %>%
  mutate(as.numeric(MSAs))

P_Occ <- bind_rows(lapply(seq_along(P_Occ_Codes), function(i) bls_api(P_Occ_Codes[i], registrationKey = Sys.getenv("BLS_Key"))))
P_Occ <- P_Occ %>%
  mutate(as.numeric(MSAs))

TMM_OCC <- bind_rows(lapply(seq_along(TMM_Occ_Codes), function(i) bls_api(TMM_Occ_Codes[i], registrationKey = Sys.getenv("BLS_Key"))))
TMM_OCC <- TMM_OCC %>%
  mutate(as.numeric(MSAs))

BC_Occ <- rbind(CE_OCC, IMR_OCC, P_Occ, TMM_OCC)

BC_Occ <- BC_Occ %>%
  group_by(`as.numeric(MSAs)`)%>%
  summarise(sum(value)) %>% 
  rename(MSA = 'as.numeric(MSAs)', Blue_Collar_Workforce = "sum(value)")

#ran out of BLS server Calls 

dfMSAsdata <- merge(dfMSAs, OES)

dfMSAdata1 <- merge(dft, OES)

createSheet(book, "OES MSA Crosswalk")

createSheet(book, "Occupation data")

writeWorksheet(book, dfMSAs, "OES MSA Crosswalk")

writeWorksheet(book, dfMSAsdata, "Occupation data")






