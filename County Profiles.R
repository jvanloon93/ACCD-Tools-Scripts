library(XLConnect)
library(XLConnectJars)
library(rJava)
library(tidycensus)
library(blscrapeR)
library(tidyverse)
library(usmap)

ten_county <- as.vector(fips('PA', county =  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')))

names(ten_county) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')

Census <- County_profile_Census_Pull(ten_county, Estimates_year = 2018, ACS_year = 2017, dataset = "acs5")

BLS <- UN_LF_County_Pull(ten_county, 2018)

CEW <- PRA_10_County(2018) %>% bind_rows()

CEW <- CEW %>%
filter(own_code == 0) %>% select(area_fips, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay)

row.names(CEW) <- (c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')
)

book <- loadWorkbook("County_Profile.xlsx", create = TRUE)

createSheet(book, "Census")
createSheet(book, "LAU")
createSheet(book, "CEW")

writeWorksheet(book, Census, "Census")
writeWorksheet(book, BLS, "LAU")
writeWorksheet(book, CEW, "CEW")
