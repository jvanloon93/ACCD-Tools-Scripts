library(rjson)
library(blscrapeR)
library(dplyr)
library(ggplot2)
library(usmap)

ten_county <- as.vector(fips('PA', county =  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')))

df_2017 <- as.data.frame(lapply(seq_along(ten_county), function(i) qcew_api(year = 2017, qtr = 'a', slice = 'area', ten_county[i])))

colnames(df_2017) 
