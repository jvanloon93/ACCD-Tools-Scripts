library(rjson)
library(blscrapeR)
library(dplyr)
library(ggplot2)
library(usmap)


PRA_10_County <- function(year) {

ten_county <- as.vector(fips('PA', county =  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')))

lst <- lapply(seq_along(ten_county), function(i) qcew_api(year = year , qtr = 'a', slice = 'area', ten_county[i]))

names(lst) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')

return(lst)

}

Aspriational_MSAs <- function(year) {
  
  Aspriational_FIPs <- c("C1242", "C1446", "C1974", "C3346", "C4266" )
  
  Aspriational_Names <- c("Austin-Round Rock, TX MSA", "Boston-Cambridge-Newton, MA-NH MSA", "Denver-Aurora-Lakewood, CO MSA",
                          "Minneapolis-St. Paul-Bloomington, MN-WI MSA", "Seattle-Tacoma-Bellevue, WA MSA")
  
  lst <- lapply(seq_along(Aspriational_FIPs), function(i) qcew_api(year = year , qtr = 'a', slice = 'area', Aspriational_FIPs[i]))
  
  names(lst) <- Aspriational_Names
  
  return(lst)
}

Competitive_MSAs <- function(year) {
  
  Competitive_FIPs <- c("C1258", "C1674","C2690", "C3498","C3798")
  
  Competitive_Names <- c("Baltimore-Columbia-Towson, MD MSA", "Charlotte-Concord-Gastonia, NC-SC MSA", " Indianapolis-Carmel-Anderson, IN MSA",
                  "Nashville-Davidson--Murfreesboro--Franklin, TN MSA", "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD MSA")
  
  lst <- lapply(seq_along(Competitive_FIPs), function(i) qcew_api(year = year , qtr = 'a', slice = 'area', Competitive_FIPs[i]))
  
  names(lst) <- Competitive_Names
  
  return(lst)
}

Peer_MSAs <- function(year) {
  
  Peer_FIPs <- c("C1714", "C1746", "C1982", "C3334", "C4118")
  
  Peer_Names <- c("Cincinnati, OH-KY-IN MSA", "Cleveland-Elyria, OH MSA", "Detroit-Warren_Dearborn, MI MSA",
                  "Milwaukee-Waukesha-West Allis, WI MSA","St. Louis, MO-IL MSA")
  
  lst <- lapply(seq_along(Peer_FIPs), function(i) qcew_api(year = year , qtr = 'a', slice = 'area', Peer_FIPs[i]))
  
  names(lst) <- Peer_Names
  
  return(lst)
}

Pittsburgh_PA_MSA <- function(year) {
  
  df<- (qcew_api(year = year, qtr = "a", slice = "area", sliceCode = "C3830"))
  
  return(df)
  
}

USA <- function(year) {
  
  df <- qcew_api(year = year, qtr = "a", slice = "area", sliceCode = "US000")
  
  return(df)
  
}

#Naming Logic: "TC" = Total Covered, "TP" = Total Private, "N" = Naics Supersector 
#Government Calculated Total Covered - Total Private
#County level NAICS sector agglvl_code == 74, MSA level == 44

PRA18 <- PRA_10_County(2018) %>% bind_rows()

PRATC18 <- PRA18 %>%
  filter(own_code == 0) %>%
  summarise(Emp = sum(annual_avg_emplvl), Est = sum(annual_avg_estabs), Wage = sum(total_annual_wages)) %>%
  mutate(Ann_Wage = Wage/Emp)

PRATP18 <- PRA18 %>%
  filter(own_code == 5 & industry_code == 10) %>%
  summarise(Emp = sum(annual_avg_emplvl), Est = sum(annual_avg_estabs), Wage = sum(total_annual_wages)) %>%
  mutate(Ann_Wage = Wage/Emp)

PRAN18 <- PRA18 %>%
  filter(agglvl_code == 74 & own_code == 5) %>%
  group_by(industry_code) %>%
  summarise(Emp = sum(annual_avg_emplvl), Est = sum(annual_avg_estabs), Wage = sum(total_annual_wages)) %>%
  mutate(Ann_Wage = Wage/Emp)

PRA17 <- PRA_10_County(2017) %>% bind_rows()

PRATC17 <- PRA17 %>%
  filter(own_code == 0) %>%
  summarise(Emp = sum(annual_avg_emplvl), Est = sum(annual_avg_estabs))

PRATP17 <- PRA17 %>%
  filter(own_code == 5 & industry_code == 10) %>%
  summarise(Emp = sum(annual_avg_emplvl), Est = sum(annual_avg_estabs))

PRAN17 <- PRA17 %>%
  filter(agglvl_code == 74 & own_code == 5) %>%
  group_by(industry_code) %>%
  summarise(NAICS_Emp = sum(annual_avg_emplvl), NAICS_Est = sum(annual_avg_estabs), NAICS_Wage = sum(total_annual_wages)) %>%
  mutate(Naics_Ann_Wage = NAICS_Wage/NAICS_Emp)

PRA16 <- PRA_10_County(2016) %>% bind_rows()


PRAN16 <- PRA16 %>%
  filter(agglvl_code == 74) %>%
  group_by(industry_code) %>%
  summarise(NAICS_Emp = sum(annual_avg_emplvl), NAICS_Est = sum(annual_avg_estabs), NAICS_Wage = sum(total_annual_wages)) %>%
  mutate(Naics_Ann_Wage = NAICS_Wage/NAICS_Emp)

USA17 <- USA(2017) 

USATC17 <- USA17 %>%
  filter(own_code == 0) %>%
  summarise(Emp = sum(annual_avg_emplvl), Est = sum(annual_avg_estabs))

USATP17 <- USA17 %>%
  filter(own_code == 5 & industry_code == 10) %>%
  summarise(Emp = sum(annual_avg_emplvl), Est = sum(annual_avg_estabs))

USAN17 <- USA17 %>%
  filter(agglvl_code == 14 & own_code == 5) %>%
  group_by(industry_code) %>%
  summarise(NAICS_Emp = sum(annual_avg_emplvl), NAICS_Est = sum(annual_avg_estabs), NAICS_Wage = sum(total_annual_wages)) %>%
  mutate(Naics_Ann_Wage = NAICS_Wage/NAICS_Emp)
