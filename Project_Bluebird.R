library(XLConnect)
library(XLConnectJars)
library(rJava)
library(tidycensus)
library(blscrapeR)
library(tidyverse)
library(usmap)

v17 <- load_variables(2017, "acs5")


PA_Fips <- as.vector(fips('PA', county =  c('Lawrence', 'Butler', 'Armstrong', 'Indiana', 'Allegheny', 'Beaver', 'Westmoreland', 'Washington', 'Fayette', 'Greene',
                                            'Venango', 'Mercer', 'Erie', 'Crawford', 'Warren', 'McKean', 'Elk', 'Clearfield', 'Jefferson', 'Clarion', 'Cambria', 'Blair',
                                            'Bedford', 'Somerset')))

PA_Names <- c('Lawrence', 'Butler', 'Armstrong', 'Indiana', 'Allegheny', 'Beaver', 'Westmoreland', 'Washington', 'Fayette', 'Greene',
                    'Venango', 'Mercer', 'Erie', 'Crawford', 'Warren', 'McKean', 'Elk', 'Clearfield', 'Jefferson', 'Clarion', 'Cambria', 'Blair',
                    'Bedford', 'Somerset')

PA_MSAs <- data.frame(PA_Fips, PA_Names)

PA_MSAs <- PA_MSAs %>%
  mutate(State = "Pennsylvania") %>%
  rename(FIPS = PA_Fips, County = PA_Names)

WV_Fips <- as.vector(fips('WV', county = c('Monongalia', 'Marion', 'Wetzel', 'Preston', 'Ohio', 'Marshall', 'Brooke', 'Hancock', 'Tyler')))

WV_Names <- c('Monogolia', 'Marion', 'Wetzel', 'Preston', 'Ohio', 'Marshall', 'Brooke', 'Hancock', 'Tyler')

WV_MSAs <- data.frame(WV_Fips, WV_Names)

WV_MSAs <- WV_MSAs %>%
  mutate(State = "West_Virginia") %>%
  rename(FIPS = WV_Fips, County = WV_Names)

OH_Fips <- as.vector(fips('OH', county = c('Cuyahoga', 'Lake', 'Geauga', 'Ashtabula', 'Trumbull', 'Mahoning', 'Medina', 'Portage', 'Summit', 'Wayne', 
                                 'Stark', 'Columbiana', 'Holmes', 'Coshocton', 'Muskingum', 'Belmont', 'Tuscarawas', 'Carroll', 'Jefferson',
                                 'Harrison', 'Guernsey', 'Noble', 'Monroe', 'Washington', 'Morgan')))

OH_names <- c('Cuyahoga', 'Lake', 'Geauga', 'Ashtabula', 'Trumbull', 'Mahoning', 'Medina', 'Portage', 'Summit', 'Wayne', 
                    'Stark', 'Columbiana', 'Holmes', 'Coshocton', 'Muskingum', 'Belmont', 'Tuscarawas', 'Carroll', 'Jefferson',
                    'Harrison', 'Guernsey', 'Noble', 'Monroe', 'Washington', 'Morgan')

OH_MSAs <- data.frame(OH_Fips, OH_names)

OH_MSAs <- OH_MSAs %>%
  mutate(State = "Ohio") %>%
  rename(FIPS = OH_Fips, County = OH_names)

county_fips <- c(PA_Fips, WV_Fips, OH_Fips)

dfMSAs <- rbind(PA_MSAs, WV_MSAs, OH_MSAs) 

dfMSAs <- dfMSAs %>%
  mutate(placeholder = 1:58)

MSAs <- c("0038300", "0021500", "0049660", "0034060")

MSA_Names <- c("Pittsburgh, PA", 'Erie, PA', 'Youngstown-Warren-Boardman, OH-PA','Morgantown, WV' )

OES_Warehouse_Manager_Codes<- as.vector(sapply(seq_along(MSAs), function(i) (paste('OEUM', MSAs[i],'000000', '113071', '03', sep = ""))))

OES_Supervisor_Manager_Codes<- as.vector(sapply(seq_along(MSAs), function(i) (paste('OEUM', MSAs[i],'000000','511011','03', sep = ""))))

OES_Warehouse_Worker_Codes<- as.vector(sapply(seq_along(MSAs), function(i) (paste('OEUM', MSAs[i],'000000','537060', '03', sep = ""))))

OES_Forklift_Operator_Codes<- as.vector(sapply(seq_along(MSAs), function(i) (paste('OEUM', MSAs[i],'000000','537051','03', sep = ""))))

OES_Warehouse_Manager <- bind_rows(lapply(seq_along(OES_Warehouse_Manager_Codes), function(i) bls_api(OES_Warehouse_Manager_Codes[i], registrationKey = Sys.getenv("BLS_Key"))))

OES_Warehouse_Manager <- OES_Warehouse_Manager %>%
  mutate(MSA = MSA_Names)

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

df <- merge(dfMSAs, Pop)

df<- merge(df, Pop_Age)

df <- merge(df, Ed_total_25_65)

df <- merge(df, HS_Total_25_65)

df <- merge(df, Bach_Total_25_65)


df <- df %>%
  mutate(HS_Percentage = HS_Total_25_65/Ed_total_25_65, Bach_Percentage = Bach_Total_25_65/Ed_total_25_65) %>%
  arrange(placeholder)

