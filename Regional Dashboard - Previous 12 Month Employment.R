library(tidyverse)
library(rjson)
library(blscrapeR)
library(usmap)

ten_county <- as.vector(fips('PA', county =  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')))

Employment_Code <- as.vector(sapply(seq_along(ten_county), function(i) (paste('ENU', ten_county[i],'10010', sep = ""))))

Total_Wage_Codes <- as.vector(sapply(seq_along(ten_county), function(i) (paste('ENU', ten_county[i],'30010', sep = ""))))

lst_Employment <- lapply(seq_along(Employment_Code), function(i) bls_api(Employment_Code[i]))

lst_Wages <- lapply(seq_along(Total_Wage_Codes), function(i) bls_api(Total_Wage_Codes[i]))

names(lst_Employment) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')

names(lst_Wages) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')

Annual_avg <- function(df) {
  subset(df, year == 2018)
  return(mean(df$value))
}

Annual_Employment_18 <- sapply(seq_along(lst_Employment), function(i) Annual_avg(as.data.frame(lst_Employment[[i]])))

Annual_Employment_18 <- as.data.frame(Annual_Employment_18)

rownames(Annual_Employment_18) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')

Annual_Wages_18 <- sapply(seq_along(lst_Wages), function(i) Annual_avg(as.data.frame(lst_Employment[[i]])))

Annual_Wages_18 <- as.data.frame(Annual_Wages_18)

rownames(Annual_Wages_18) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')
