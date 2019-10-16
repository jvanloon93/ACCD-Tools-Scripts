library(tidycensus)
library(blscrapeR)
library(tidyverse)
library(usmap)

Profile_UR_LF <- function(year, month_code){
  
  ten_county <- as.vector(fips('PA', county =  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')))
  
  Unemployment <- as.vector(sapply(seq_along(ten_county), function(i) (paste('LAUCN',ten_county[i],'0000000004', sep = ""))))
  
  Labor_Force <- as.vector(sapply(seq_along(ten_county), function(i) (paste('LAUCN',ten_county[i],'0000000006', sep = ""))))
  
  df_UN <- bls_api(c(unname(Unemployment)), registrationKey = Sys.getenv("BLS_Key"))
  
  df_LF <- bls_api(c(unname(Labor_Force)), registrationKey = Sys.getenv("BLS_Key"))
  
  UN_Filter <- df_UN %>%
    filter(year == year & period == month_code | year == (year - 1) & period == month_code) %>%
    group_by(year) %>%
    summarise(sum(value)) %>%
    mutate(month = "M03", Geography = 'Pittsburgh_Region') %>%
    rename(Unemployment = 'sum(value)')
  
  LF_Filter <- df_LF %>%
    filter(year == 2019 & period == month_code | year == (year - 1) & period == month_code) %>%
    group_by(year) %>%
    summarise(sum(value)) %>%
    rename(Labor_Force = 'sum(value)')
  
  df <- merge(UN_Filter, LF_Filter) %>%
    select(year, month, Geography, Unemployment, Labor_Force) %>%
    mutate(Unemployment_Rate = Unemployment / Labor_Force)
  
  USUN <- quick_unemp_level() %>%
    filter(year == 2019 & period == month_code | year == (year - 1) & period == month_code) %>%
    mutate(Geography = "Nation") %>%
    rename(Unemployment = "value", month = period) %>%
    select(year, month, Geography, Unemployment)
  
  USLF <- quick_laborForce_level() %>%
    filter(year == 2019 & period == month_code | year == (year - 1) & period == month_code) %>%
    rename(Labor_Force = "value") %>%
    select(Labor_Force)
  
  usdf <- cbind(USUN, USLF) %>%
    select(year, month, Geography, Unemployment, Labor_Force) %>%
    mutate(Unemployment_Rate = Unemployment / Labor_Force)
  
  
  return(rbind(df, usdf))
  
}

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

PRA_10_County_qtrly <- function(year, quarter) {
  
  ten_county <- as.vector(fips('PA', county =  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')))
  
  df <- bind_rows(lapply(seq_along(ten_county), function(i) qcew_api(year = year , qtr = quarter, slice = 'area', ten_county[i])))
  
  df <- df %>%
    filter(own_code == 0) %>%
    summarise(sum(month1_emplvl), sum(month2_emplvl), sum(month3_emplvl), Wage = sum(total_qtrly_wages)) %>%
    mutate(year = year, quarter = quarter) %>%
    select(year, quarter, "sum(month1_emplvl)", "sum(month2_emplvl)", "sum(month3_emplvl)", Wage)
  
  return(df)
  
}

USA_qrtly <- function(year, quarter) {
  
  df <- qcew_api(year = year, qtr = quarter, slice = "area", sliceCode = "US000")
  
  df <- df %>%
    mutate(year = year, quarter = quarter, Wage = total_qtrly_wages) %>%
    select(year, quarter, month1_emplvl, month2_emplvl, month3_emplvl, Wage)
  
  return(df)
  
}

Previous_year_Annual_Wage <- function(df) {
  emp <- mean(unname(unlist(df[1:4, 3:5])))
  
  wage <- sum(df$Wage)
  
  return(wage/emp)
}

USA_qrtly <- function(year, quarter) {
  
  df <- qcew_api(year = year, qtr = quarter, slice = "area", sliceCode = "US000")
  
  df <- df %>%
    filter(own_code == 0) %>%
    mutate(year = year, quarter = quarter, Wage = total_qtrly_wages) %>%
    select(year, quarter, month1_emplvl, month2_emplvl, month3_emplvl, Wage)
  
  return(df)
  
}

PRAdf <- bind_rows(PRA_10_County_qtrly(2019, 1), PRA_10_County_qtrly(2018, 4), PRA_10_County_qtrly(2018, 3), PRA_10_County_qtrly(2018, 2))

USAdf <- bind_rows(USA_qrtly(2019, 1), USA_qrtly(2018,4), USA_qrtly(2018, 3), USA_qrtly(2018, 2))
