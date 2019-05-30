PRA_CEW <- function(year) {
  
  ten_county <- as.vector(fips('PA', county =  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')))
  
  names(ten_county) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')
  
  df<- as.data.frame(sapply(seq_along(ten_county), function(i) qcew_api(year = year , qtr = 'a', slice = 'area', ten_county[i])))
  
  colnames(df) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')
  
  naics <- blscrapeR::niacs
  
  #https://data.bls.gov/cew/doc/titles/ownership/ownership_titles.htm
  
  Allegheny <- as.data.frame(df$Allegheny) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
    rename(Allegheny_Employment = annual_avg_emplvl , Allegheny_Establishments = annual_avg_estabs, Allegheny_AvgAnn_Wage = avg_annual_pay)
  
  Armstrong <- as.data.frame(df$Armstrong) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
    rename(Armstrong_Employment = annual_avg_emplvl , Armstrong_Establishments = annual_avg_estabs, Armstrong_AvgAnn_Wage = avg_annual_pay)
  
  Beaver <- as.data.frame(df$Beaver) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
    rename(Beaver_Employment = annual_avg_emplvl , Beaver_Establishments = annual_avg_estabs, Beaver_AvgAnn_Wage = avg_annual_pay)
  
  Butler <- as.data.frame(df$Butler) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
    rename(Butler_Employment = annual_avg_emplvl , Butler_Establishments = annual_avg_estabs, Butler_AvgAnn_Wage = avg_annual_pay)
  
  Fayette <- as.data.frame(df$Fayette) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay)%>% 
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
    rename(Fayette_Employment = annual_avg_emplvl , Fayette_Establishments = annual_avg_estabs, Fayette_AvgAnn_Wage = avg_annual_pay)
  
  Greene <- as.data.frame(df$Greene) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
    rename(Greene_Employment = annual_avg_emplvl , Greene_Establishments = annual_avg_estabs, Greene_AvgAnn_Wage = avg_annual_pay)
  
  Indiana <- as.data.frame(df$Indiana) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
    rename(Indiana_Employment = annual_avg_emplvl, Indiana_Establishments = annual_avg_estabs, Indiana_AvgAnn_Wage = avg_annual_pay)
  
  Lawrence <- as.data.frame(df$Lawrence) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
    rename(Lawrence_Employment = annual_avg_emplvl , Lawrence_Establishments = annual_avg_estabs, Lawrence_AvgAnn_Wage = avg_annual_pay)
  
  Washington <- as.data.frame(df$Washington) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
    rename(Washington_Employment = annual_avg_emplvl , Washington_Establishments = annual_avg_estabs, Washington_AvgAnn_Wage = avg_annual_pay)
  
  Westmoreland <- as.data.frame(df$Westmoreland) %>%
    select(industry_code, own_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
    filter(own_code == 5) %>%
    select(industry_code, annual_avg_emplvl, annual_avg_estabs, avg_annual_pay) %>%
    rename(Westmoreland_Employment = annual_avg_emplvl , Westmoreland_Establishments = annual_avg_estabs, Westmoreland_AvgAnn_Wage = avg_annual_pay)
  
  df_clean <- Reduce(function(x,y) merge(x,y, by = "industry_code"), list(naics, Allegheny, Armstrong, Beaver, Butler, Fayette, Greene, Indiana, Lawrence, Washington, Westmoreland))
  
  df_clean <- df_clean %>% 
    mutate(Employment_total = Allegheny_Employment + Armstrong_Employment + Beaver_Employment + Butler_Employment + Fayette_Employment + Greene_Employment + Indiana_Employment + Lawrence_Employment + Washington_Employment + Westmoreland_Employment, 
           Establishment_total = Allegheny_Establishments + Armstrong_Establishments + Beaver_Establishments + Butler_Establishments + Fayette_Establishments + Greene_Establishments + Indiana_Establishments + Lawrence_Establishments + Washington_Establishments + Washington_Establishments) 
  
  return(df_clean)
  
}


# Annual_Average_Wage = ((Allegheny_AvgAnn_Wage * Allegheny_Employment) + (Armstrong_AvgAnn_Wage * Armstrong_Employment) + (Beaver_AvgAnn_Wage * Beaver_Employment)  + (Butler_AvgAnn_Wage * Butler_Employment) + (Fayette_AvgAnn_Wage * Fayette_Employment)
# + (Greene_AvgAnn_Wage * Greene_Employment) + (Indiana_AvgAnn_Wage * Indiana-Employment) + (Lawrence_AvgAnn_Wage * Lawrence_Employment) + (Washington_AvgAnn_Wage * Washington_Employment) + (Westmoreland_AvgAnn_Wage * Westmoreland_Employment)/Employment_total))