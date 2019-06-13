library(rjson)
library(blscrapeR)
library(dplyr)
library(ggplot2)
library(usmap)

ten_county <- as.vector(fips('PA', county =  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')))

df<- PRA_CEW_Tall_Data(2018)

df <- df %>%
  mutate(average_wage = total_annual_wages / annual_avg_emplvl)

total_industry <- df %>%
  filter(industry_code == 10) %>%
  mutate(average_wage = total_annual_wages / annual_avg_emplvl)


ggplot(total_industry, aes(x = Geography, y =  annual_avg_emplvl)) + geom_col() 

ggplot(total_industry, aes(x = Geography, y =  annual_avg_estabs)) + geom_col() 

ggplot(total_industry, aes(x = Geography, y =  average_wage)) + geom_col() 


  


