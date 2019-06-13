library(rjson)
library(blscrapeR)
library(dplyr)
library(ggplot2)
library(usmap)

PRA_CEW_Tall <- function(year) {
  
  ten_county <- as.vector(fips('PA', county =  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')))
  
  names(ten_county) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')
  
  df<- as.data.frame(sapply(seq_along(ten_county), function(i) qcew_api(year = year, qtr = 'a', slice = 'area', ten_county[i])))
  
  colnames(df) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')
  
  naics <- blscrapeR::niacs
  
  #https://data.bls.gov/cew/doc/titles/ownership/ownership_titles.htm
  
  Allegheny <- as.data.frame(df$Allegheny) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay,total_annual_wages) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay,total_annual_wages) %>%
    mutate(Geography = as.factor("Allegheny"))
  
  Allegheny <- merge(naics, Allegheny, by = "industry_code")

  Armstrong <- as.data.frame(df$Armstrong) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay, total_annual_wages) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay,total_annual_wages) %>%
    mutate(Geography = as.factor("Armstrong"))
  
  Armstrong <- merge(naics, Armstrong, by = "industry_code")

  Beaver <- as.data.frame(df$Beaver) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay, total_annual_wages) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay,total_annual_wages) %>%
    mutate(Geography = as.factor("Beaver"))
  
  Beaver <- merge(naics, Beaver, by = "industry_code")

  Butler <- as.data.frame(df$Butler) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay, total_annual_wages) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay,total_annual_wages) %>%
    mutate(Geography = as.factor("Butler"))
  
  Butler <- merge(naics, Butler, by = "industry_code")
  
  Fayette <- as.data.frame(df$Fayette) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay, total_annual_wages)%>% 
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay,total_annual_wages) %>%
    mutate(Geography = as.factor("Fayette"))
  
  Fayette <- merge(naics, Fayette, by = "industry_code")
  
  Greene <- as.data.frame(df$Greene) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay, total_annual_wages) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay,total_annual_wages) %>%
    mutate(Geography = as.factor("Greene"))
  
  Greene <- merge(naics, Greene, by = "industry_code")
  
  Indiana <- as.data.frame(df$Indiana) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay, total_annual_wages) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay,total_annual_wages) %>%
    mutate(Geography = as.factor("Indiana"))
  
  Indiana <- merge(naics, Indiana, by = "industry_code")
  
  Lawrence <- as.data.frame(df$Lawrence) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay, total_annual_wages) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay,total_annual_wages) %>%
    mutate(Geography = as.factor("Lawrence"))
  
  Lawrence <- merge(naics, Lawrence, by = "industry_code")
  
  Washington <- as.data.frame(df$Washington) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay, total_annual_wages) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay,total_annual_wages) %>%
    mutate(Geography = as.factor("Washington"))
  
  Washington <- merge(naics, Washington, by = "industry_code")
  
  Westmoreland <- as.data.frame(df$Westmoreland) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay, total_annual_wages) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay,total_annual_wages) %>%
    mutate(Geography = as.factor("Westmoreland"))
  
  Westmoreland <- merge(naics, Westmoreland, by = "industry_code")
  
  df_clean <- rbind.data.frame(Allegheny, Armstrong, Beaver, Butler, Fayette, Greene, Indiana, Lawrence, Washington, Westmoreland)
  
  df_clean <- df_clean %>%
    mutate(Year = year)
  
  #df_clean <- df_clean %>% 
    #mutate(Employment_total = Allegheny_Employment + Armstrong_Employment + Beaver_Employment + Butler_Employment + Fayette_Employment + Greene_Employment + Indiana_Employment + Lawrence_Employment + Washington_Employment + Westmoreland_Employment, 
           #Establishment_total = Allegheny_Establishments + Armstrong_Establishments + Beaver_Establishments + Butler_Establishments + Fayette_Establishments + Greene_Establishments + Indiana_Establishments + Lawrence_Establishments + Washington_Establishments + Washington_Establishments, 
           #Average_Annual_Wage = (Allegheny_Total_Wage + Armstrong_Total_Wage + Beaver_Total_Wage + Butler_Total_Wage + Fayette_Total_Wage + Greene_Total_Wage + Indiana_Total_Wage + Lawrence_Total_Wage + Westmoreland_Total_Wage + Washington_Total_Wage) / Employment_total, 
           #Year = year)
  
  return(df_clean)
}

