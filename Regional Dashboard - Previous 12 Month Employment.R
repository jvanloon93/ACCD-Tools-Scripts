library(tidyverse)
library(rjson)
library(blscrapeR)
library(usmap)

ten_county <- as.vector(fips('PA', county =  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')))

Employment_Code <- as.vector(sapply(seq_along(ten_county), function(i) (paste('ENU', ten_county[i],'10010', sep = ""))))

Total_Wage_Codes <- as.vector(sapply(seq_along(ten_county), function(i) (paste('ENU', ten_county[i],'30010', sep = ""))))

#df_Employment <- as.data.frame(sapply(seq_along(Employment_Code), function(i) bls_api(Employment_Code[i])))

lst_Employment <- lapply(seq_along(Employment_Code), function(i) bls_api(Employment_Code[i]))
    
#df_Wages <- as.data.frame(sapply(seq_along(Total_Wage_Codes), function(i) bls_api(Total_Wage_Codes[i])))

lst_Wages <- lapply(seq_along(Total_Wage_Codes), function(i) bls_api(Total_Wage_Codes[i]))

colnames(df_Employment) <-  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')

colnames(df_Wages) <-  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')

#try as list, not as df off the bat.

names(lst_Employment) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')

for(i in 1:nrow(df_Employment)) {
   
}


year_iso <- subset(df_Employment[[1]], df_Employment[[1]]$year == 2018)
