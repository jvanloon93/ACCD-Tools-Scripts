library(tidyverse)
library(rjson)
library(blscrapeR)
library(usmap)

ten_county_USA <- as.vector(c(fips('PA', county =  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')), 'US000'))

Employment_Code <- as.vector(sapply(seq_along(ten_county_USA), function(i) (paste('ENU', ten_county_USA[i],'10010', sep = ""))))

Total_Wage_Codes <- as.vector(sapply(seq_along(ten_county_USA), function(i) (paste('ENU', ten_county_USA[i],'30010', sep = ""))))

lst_Employment <- lapply(seq_along(Employment_Code), function(i) bls_api(Employment_Code[i]))

lst_Wages <- lapply(seq_along(Total_Wage_Codes), function(i) bls_api(Total_Wage_Codes[i]))

names(lst_Employment) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland', 'USA')

names(lst_Wages) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland', 'USA')


Q1_Average_18 <- function(df) {
  df<- subset(df, year == 2018 & period == )
  return(mean(df$value))
}

Q1_Average_19 <- function(df) {
  df <-subset(df, year == 2019 & period == )
  return(sum(df$value))
}
  
Q1_19_Wages <- sapply(seq_along(lst_Wages), function(i) Q1_Average_19(as.data.frame(lst_Wages[[i]])))
  
Q1_19_Wages <- as.data.frame(Q1_19_Wages)
  
rownames(Q1_19_Wages) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland', 'USA')

Q1_18_Wages <- sapply(seq_along(lst_Wages), function(i) Q1_Average_18(as.data.frame(lst_Wages[[i]])))

Q1_18_Wages <- as.data.frame(Q1_18_Wages)

rownames(Q1_18_Wages) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland', 'USA')

