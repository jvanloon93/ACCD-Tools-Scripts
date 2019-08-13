library(rjson)
library(blscrapeR)
library(dplyr)
library(ggplot2)
library(usmap)


ten_county <- as.vector(fips('PA', county =  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')))
  
names(ten_county) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')
  
df<- bind_rows(lapply(seq_along(ten_county), function(i) qcew_api(year = 2018 , qtr = 'a', slice = 'area', ten_county[i])))
  
df<- bind_rows(lapply(seq_along(ten_county), function(i) qcew_api(year = 2018 , qtr = 'a', slice = 'area', 38300)))

MSA <- qcew_api(year = 2017, qtr = "a", slice = "area", sliceCode = 38300)

#Possible to combine all Counties? Or Pull MSAs? 