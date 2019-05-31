library(rjson)
library(blscrapeR)
library(dplyr)
library(ggplot2)
library(usmap)

ten_county <- as.vector(fips('PA', county =  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')))

time_frame <- c(2007:2017)

df_17 <- PRA_CEW_Tall(2017)
df_16 <- PRA_CEW_Tall(2016)
df <- as.data.frame(lapply(time_frame, PRA_CEW_Tall))