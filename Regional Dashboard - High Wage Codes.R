library(tidyverse)
library(rjson)
library(blscrapeR)
library(usmap)



Profile_High_Wage <- function(year, month_code) {

MSA_Total_Employees_Code <- c('SMU42383000000000001')
  
MSA_High_Wage_Codes <- c('SMU42383001000000001',
                        'SMU42383002000000001',
                        'SMU42383003000000001',
                        'SMU42383004100000001',
                        'SMU42383004322000001',
                        'SMU42383005000000001',
                        'SMU42383005552000001',
                        'SMU42383006054000001',
                        'SMU42383006055000001',
                        'SMU42383006561130001',
                        'SMU42383006562100001',
                        'SMU42383006562200001')

USA_Total_Employees_Code <- c('CES0000000001')
                        
USA_High_Wage_Codes <- c('CES1000000001',
                        'CES2000000001',
                        'CES3000000001',
                        'CES4142000001',
                        'CES4422000001',
                        'CES5000000001',
                        'CES5552000001',
                        'CES6054000001',
                        'CES6055000001',
                        'CES6561130001',
                        'CES6562100001',
                        'CES6562200001')

MSA_Total_Employees <- bls_api(MSA_Total_Employees_Code, registrationKey = Sys.getenv("BLS_KEY")) %>%
  filter(year == year & period == month_code | year == (year - 1) & period == month_code) %>%
  rename(Total_Employees = "value") %>%
  mutate(Geography = "Pittsburgh, MSA") %>%
  select(year, period, Geography, Total_Employees)

USA_Total_Employees <- bls_api(USA_Total_Employees_Code, registrationKey = Sys.getenv("BLS_KEY")) %>%
  filter(year == year & period == month_code | year == (year - 1) & period == month_code) %>%
  rename(Total_Employees = "value") %>%
  mutate(Geography = "Nation") %>%
  select(year, period, Geography, Total_Employees)
  

MSA_High_Wage <- bind_rows(lapply(seq_along(MSA_High_Wage_Codes), function(i) bls_api(MSA_High_Wage_Codes[i], registrationKey = Sys.getenv("BLS_KEY")))) %>%
  filter(year == year & period == month_code | year == (year - 1) & period == month_code) %>%
  group_by(year) %>%
  summarise(sum(value)) %>%
  rename(High_Wage_Employment = "sum(value)") %>% 
  select(High_Wage_Employment)


USA_High_Wage <- bind_rows(lapply(seq_along(USA_High_Wage_Codes), function(i) bls_api(USA_High_Wage_Codes[i], registrationKey = Sys.getenv("BLS_KEY")))) %>%
  filter(year == year & period == month_code | year == (year - 1) & period == month_code) %>%
  group_by(year) %>%
  summarise(sum(value)) %>%
  rename(High_Wage_Employment = "sum(value)") %>% 
  select(High_Wage_Employment)

return(df <- rbind(cbind(MSA_Total_Employees, MSA_High_Wage), cbind(USA_Total_Employees, USA_High_Wage)))

}

df <- Profile_High_Wage(2019, "M06")