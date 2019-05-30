library(rjson)
library(blscrapeR)
library(dplyr)
library(ggplot2)
library(usmap)

PRA_CEW <- function(year) {
  
  ten_county <- as.vector(fips('PA', county =  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')))
  
  names(ten_county) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')
  
  df<- as.data.frame(sapply(seq_along(ten_county), function(i) qcew_api(year = year , qtr = 'a', slice = 'area', ten_county[i])))
  
  colnames(df) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')
  
  naics <- blscrapeR::niacs
  
  #https://data.bls.gov/cew/doc/titles/ownership/ownership_titles.htm
  
  Allegheny <- as.data.frame(df$Allegheny) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay,total_annual_wages) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay,total_annual_wages) %>%
    rename(Allegheny_Employment = annual_avg_emplvl , Allegheny_Establishments = annual_avg_estabs, Allegheny_AvgAnn_Wage = avg_annual_pay, Allegheny_Total_Wage = total_annual_wages)
  
  Armstrong <- as.data.frame(df$Armstrong) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay, total_annual_wages) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay,total_annual_wages) %>%
    rename(Armstrong_Employment = annual_avg_emplvl , Armstrong_Establishments = annual_avg_estabs, Armstrong_AvgAnn_Wage = avg_annual_pay, Armstrong_Total_Wage = total_annual_wages)
  
  Beaver <- as.data.frame(df$Beaver) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay, total_annual_wages) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay,total_annual_wages) %>%
    rename(Beaver_Employment = annual_avg_emplvl , Beaver_Establishments = annual_avg_estabs, Beaver_AvgAnn_Wage = avg_annual_pay, Beaver_Total_Wage = total_annual_wages)
  
  Butler <- as.data.frame(df$Butler) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay, total_annual_wages) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay,total_annual_wages) %>%
    rename(Butler_Employment = annual_avg_emplvl , Butler_Establishments = annual_avg_estabs, Butler_AvgAnn_Wage = avg_annual_pay, Butler_Total_Wage = total_annual_wages)
  
  Fayette <- as.data.frame(df$Fayette) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay, total_annual_wages)%>% 
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay,total_annual_wages) %>%
    rename(Fayette_Employment = annual_avg_emplvl , Fayette_Establishments = annual_avg_estabs, Fayette_AvgAnn_Wage = avg_annual_pay, Fayette_Total_Wage = total_annual_wages)
  
  Greene <- as.data.frame(df$Greene) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay, total_annual_wages) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay,total_annual_wages) %>%
    rename(Greene_Employment = annual_avg_emplvl , Greene_Establishments = annual_avg_estabs, Greene_AvgAnn_Wage = avg_annual_pay, Greene_Total_Wage = total_annual_wages)
  
  Indiana <- as.data.frame(df$Indiana) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay, total_annual_wages) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay,total_annual_wages) %>%
    rename(Indiana_Employment = annual_avg_emplvl, Indiana_Establishments = annual_avg_estabs, Indiana_AvgAnn_Wage = avg_annual_pay, Indiana_Total_Wage = total_annual_wages)
  
  Lawrence <- as.data.frame(df$Lawrence) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay, total_annual_wages) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay,total_annual_wages) %>%
    rename(Lawrence_Employment = annual_avg_emplvl , Lawrence_Establishments = annual_avg_estabs, Lawrence_AvgAnn_Wage = avg_annual_pay, Lawrence_Total_Wage = total_annual_wages)
  
  Washington <- as.data.frame(df$Washington) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay, total_annual_wages) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay,total_annual_wages) %>%
    rename(Washington_Employment = annual_avg_emplvl , Washington_Establishments = annual_avg_estabs, Washington_AvgAnn_Wage = avg_annual_pay, Westmoreland_Total_Wage = total_annual_wages)
  
  Westmoreland <- as.data.frame(df$Westmoreland) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay, total_annual_wages) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay,total_annual_wages) %>%
    rename(Westmoreland_Employment = annual_avg_emplvl , Westmoreland_Establishments = annual_avg_estabs, Westmoreland_AvgAnn_Wage = avg_annual_pay, Washington_Total_Wage = total_annual_wages)
  
  df_clean <- Reduce(function(x,y) merge(x,y, by = "industry_code"), list(naics, Allegheny, Armstrong, Beaver, Butler, Fayette, Greene, Indiana, Lawrence, Washington, Westmoreland))
  
  df_clean <- df_clean %>% 
    mutate(Employment_total = Allegheny_Employment + Armstrong_Employment + Beaver_Employment + Butler_Employment + Fayette_Employment + Greene_Employment + Indiana_Employment + Lawrence_Employment + Washington_Employment + Westmoreland_Employment, 
           Establishment_total = Allegheny_Establishments + Armstrong_Establishments + Beaver_Establishments + Butler_Establishments + Fayette_Establishments + Greene_Establishments + Indiana_Establishments + Lawrence_Establishments + Washington_Establishments + Washington_Establishments, 
           Average_Annual_Wage = (Allegheny_Total_Wage + Armstrong_Total_Wage + Beaver_Total_Wage + Butler_Total_Wage + Fayette_Total_Wage + Greene_Total_Wage + Indiana_Total_Wage + Lawrence_Total_Wage + Westmoreland_Total_Wage + Washington_Total_Wage) / Employment_total, 
           Year = year)
  
  return(df_clean)
}


