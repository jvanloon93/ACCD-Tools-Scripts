library(rjson)
library(blscrapeR)
library(dplyr)
library(ggplot2)
library(usmap)

ten_county <- as.vector(fips('PA', county =  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')))


<<<<<<< HEAD
df <- df %>%
  mutate(average_wage = total_annual_wages / annual_avg_emplvl)

total_industry <- df %>%
=======
d18<- PRA_CEW_Tall(2018)

total_industry <- d18 %>%
>>>>>>> b738ba32b99fefe607adec78c31939ee032400e6
  filter(industry_code == 10) %>%
  mutate(average_wage = total_annual_wages / annual_avg_emplvl)

d18$industry_code <- as.character(d18$industry_code)


ggplot(total_industry, aes(x = Geography, y =  annual_avg_emplvl)) + geom_col() 

ggplot(total_industry, aes(x = Geography, y =  annual_avg_estabs)) + geom_col() 

ggplot(total_industry, aes(x = Geography, y =  average_wage)) + geom_col() 



d18 <- d18 %>%
mutate(if(grepl("^11", industry_code)) {Sector = "Agriculture, forestry, fishing and hunting"})


d18 %>%
  isTRUE(grepl("^11", industry_code))
       
       for(i in 1:nrow(d18)){
  if (grepl("^11", d18$industry_code)) {
    print(paste("Agriculture, forestry, fishing and hunting"))
  }
} 

