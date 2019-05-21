library(rjson)
library(blscrapeR)
library(dplyr)
library(ggplot2)
library(usmap)

ten_county <- as.vector(fips('PA', county =  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')))

names(ten_county) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')

df<- as.data.frame(sapply(seq_along(ten_county), function(i) qcew_api(year = 2017, qtr = 'a', slice = 'area', ten_county[i])))

colnames(df) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')

naics <- blscrapeR::niacs

Allegheny_empl <- subset(as.data.frame(df$Allegheny), select = c(industry_code, annual_avg_emplvl))

Armstrong_empl <- subset(as.data.frame(df$Armstrong), select = c(industry_code, annual_avg_emplvl))

                         
df_empl <- merge.data.frame(naics, Allegheny_empl, by.x = "industry_code")

df_empl <- merge.data.frame(df_empl, Armstrong_empl, by.x = 'industry_code')

reduce(merge( list()))

#Reduce & Merge Combo for multiple dataframes or list of dataframes 