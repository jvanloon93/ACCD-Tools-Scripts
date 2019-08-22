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
  
  Peer_Names <- c("Cincinnati, OH-KY-IN MSA", "Cleveland-Elyria, OH MSA", "Denver-Aurora-Lakewood, CO MSA",
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



