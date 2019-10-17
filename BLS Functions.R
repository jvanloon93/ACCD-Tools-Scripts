library(rJava)
library(XLConnect)
library(XLConnectJars)
library(tidycensus)
library(blscrapeR)
library(tidyverse)
library(usmap)



UN_LF_County_Pull <- function(county_fips, year) {
  
  Unemployment_Codes <- as.vector(sapply(seq_along(county_fips), function(i) (paste('LAUCN',county_fips[i],'0000000004', sep = ""))))
  Employment_Codes <- as.vector(sapply(seq_along(county_fips), function(i) (paste('LAUCN', county_fips[i], '0000000005', sep = ""))))
  Labor_Force_Codes <- as.vector(sapply(seq_along(county_fips), function(i) (paste('LAUCN',county_fips[i],'0000000006', sep = ""))))
  Unemployment <- bind_rows(lapply(seq_along(Unemployment_Codes), 
                                         function(i) bls_api(
                                           Unemployment_Codes[i],
                                           startyear = year,
                                           endyear = year,
                                           registrationKey = Sys.getenv("BLS_KEY")
                                         )))
  Unemployment <- Unemployment %>%
    filter(year == year) %>%
    group_by(seriesID) %>%
    summarise(mean(value)) %>% 
    rename(Annual_Unemployment = "mean(value)") %>%
    select(Annual_Unemployment)
  
  Employment <- bind_rows(lapply(seq_along(Employment_Codes), 
                                   function(i) bls_api(
                                     Employment_Codes[i],
                                     startyear = year,
                                     endyear = year,
                                     registrationKey = Sys.getenv("BLS_KEY")
                                   )))
  Employment <- Employment %>%
    filter(year == year) %>%
    group_by(seriesID) %>%
    summarise(mean(value)) %>% 
    rename(Annual_Employment = "mean(value)") %>%
    select(Annual_Employment)
  
  Labor_Force <- bind_rows(lapply(seq_along(Labor_Force_Codes), 
                                  function(i) bls_api(
                                    Labor_Force_Codes[i],
                                    startyear = year,
                                    endyear = year,
                                    registrationKey = Sys.getenv("BLS_KEY")
                                  )))
  
  Labor_Force <- Labor_Force %>%
    filter(year == year) %>%
    group_by(seriesID) %>%
    summarise(mean(value)) %>% 
    rename(Annual_Laborforce = "mean(value)") %>%
    select(Annual_Laborforce)
  
  df <- cbind(county_fips, Unemployment, Employment, Labor_Force)
  
  df<- df %>%
    mutate(Unemployment_Rate = Annual_Unemployment / Annual_Laborforce)
  
  return(df)
  
}
  

  