install.packages('dplyr')
install.packages('rjson')
install.packages("blscrapR")
library(dplyr)
library(rjson)
library(blscrapeR)
library(ggplot2)
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


df <- Profile_UR_LF(2019, "M03")



