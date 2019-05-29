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

#https://data.bls.gov/cew/doc/titles/ownership/ownership_titles.htm

Allegheny <- as.data.frame(df$Allegheny) %>%
  select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
  filter(own_code == 5) %>%
  rename(Allegheny_Employment = annual_avg_emplvl , Allegheny_Establishments = annual_avg_estabs, Allegheny_AvgAnn_Wage = avg_annual_pay)

Armstrong <- as.data.frame(df$Armstrong) %>%
  select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
  filter(own_code == 5) %>%
  rename(Armstrong_Employment = annual_avg_emplvl , Armstrong_Establishments = annual_avg_estabs, Armstrong_AvgAnn_Wage = avg_annual_pay)

Beaver <- as.data.frame(df$Beaver) %>%
  select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
  filter(own_code == 5) %>%
  rename(Beaver_Employment = annual_avg_emplvl , Beaver_Establishments = annual_avg_estabs, Beaver_AvgAnn_Wage = avg_annual_pay)

Butler <- as.data.frame(df$Butler) %>%
  select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
  filter(own_code == 5) %>%
  rename(Butler_Employment = annual_avg_emplvl , Butler_Establishments = annual_avg_estabs, Butler_AvgAnn_Wage = avg_annual_pay)

Fayette <- as.data.frame(df$Fayette) %>%
  select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay)%>% 
  filter(own_code == 5) %>%
  rename(Fayette_Employment = annual_avg_emplvl , Fayette_Establishments = annual_avg_estabs, Fayette_AvgAnn_Wage = avg_annual_pay)

Greene <- as.data.frame(df$Greene) %>%
  select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
  filter(own_code == 5) %>%
  rename(Greene_Employment = annual_avg_emplvl , Greene_Establishments = annual_avg_estabs, Greene_AvgAnn_Wage = avg_annual_pay)

Indiana <- as.data.frame(df$Indiana) %>%
  select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
  filter(own_code == 5) %>%
  rename(Indiana_Employment = annual_avg_emplvl, Indiana_Establishments = annual_avg_estabs, Indiana_AvgAnn_Wage = avg_annual_pay)

Lawrence <- as.data.frame(df$Lawrence) %>%
  select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
  filter(own_code == 5) %>%
  rename(Lawrence_Employment = annual_avg_emplvl , Lawrence_Establishments = annual_avg_estabs, Lawrence_AvgAnn_Wage = avg_annual_pay)

Washington <- as.data.frame(df$Washington) %>%
  select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
  filter(own_code == 5) %>%
  rename(Washington_Employment = annual_avg_emplvl , Washington_Establishments = annual_avg_estabs, Washington_AvgAnn_Wage = avg_annual_pay)

Westmoreland <- as.data.frame(df$Westmoreland) %>%
  select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
  filter(own_code == 5) %>%
  rename(Westmoreland_Employment = annual_avg_emplvl , Westmoreland_Establishments = annual_avg_estabs, Westmoreland_AvgAnn_Wage = avg_annual_pay)


naics_1 <- merge(naics, Allegheny)
naics_2 <- merge(naics_1, Armstrong, by = 'industry_code')

df_clean <- Reduce(function(x,y) merge(x,y, by = "industry_code"), list(naics, Allegheny, Armstrong, Beaver, Butler, Fayette, Greene, Indiana, Lawrence, Washington, Westmoreland))

# Goal - Data frame with totals of Annual Employment, Annual Establishments, and Avg Annual wages across ten countys. 