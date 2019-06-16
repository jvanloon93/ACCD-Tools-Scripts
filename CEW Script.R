library(rjson)
library(blscrapeR)
library(dplyr)
library(ggplot2)
library(usmap)

ten_county <- as.vector(fips('PA', county =  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')))


d18<- PRA_CEW_Tall(2018)

total_industry <- d18 %>%
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

--- #More Promissing
  Benchmark <- c("C1242", "C1446", "C1974", "C3346", "C4266", "C1258", "C1674", "C2690", "C3498","C3798", "C1714", "C1746", "C1982", "C3334", "C4118")
Benchmarks_names <- c("Austin-Round Rock, TX MSA", "Boston-Cambridge-Newton, MA-NH MSA",  "Denver-Aurora-Lakewood, CO MSA",
                      "Minneapolis-St. Paul-Bloomington, MN-WI MS", "Seattle-Tacoma-Bellevue, WA MSA", "Baltimore-Columbia-Towson, MD MSA",
                      "Charlotte-Concord-Gastonia, NC-SC MSA", "Indianapolis-Carmel-Anderson, IN MSA", "Nashville-Davidson--Murfreesboro--Franklin, TN MSA", 
                      "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD MSA", "Cincinnati, OH-KY-IN MSA", "Cleveland-Elyria, OH MSA", "Detroit-Warren-Dearborn, MI MSA",
                      "Milwaukee-Waukesha-West Allis, WI MSA", "St. Louis, MO-IL MSA")

Benchmark <- data.frame(Benchmark, Benchmarks_names)

MSAGIT <- function(year) {
  MSA <- qcew_api(year = year, qtr = "a", slice = "area", sliceCode = "C3830")
  Greene <- qcew_api(year = year, qtr = "a", slice = "area", sliceCode = "42059")
  Indiana <- qcew_api(year = year, qtr = "a", slice = "area", sliceCode = "42063")
  Lawrence <- qcew_api(year = year, qtr = "a", slice = "area", sliceCode = "42073")
  df <- rbind.data.frame(MSA, Greene, Indiana, Lawrence)
}

Benchmarks <- function(year) { 
  Austin <-  qcew_api(year = year, qtr = "a", slice = "area", sliceCode = Benchmark[1])
  Boston <- qcew_api(year = year, qtr = "a", slice = "area", sliceCode = Benchmark[2])
  Denver <- qcew_api(year = year, qtr = "a", slice = "area", sliceCode = Benchmark[3])
  Minneapolis <- qcew_api(year = year, qtr = "a", slice = "area", sliceCode = Benchmark[4])
  Seattle <- qcew_api(year = year, qtr = "a", slice = "area", sliceCode = Benchmark[5])
  Baltimore <- qcew_api(year = year, qtr = "a", slice = "area", sliceCode = Benchmark[6])
  Charlotte <- qcew_api(year = year, qtr = "a", slice = "area", sliceCode = Benchmark[7])
  Indianapolis <- qcew_api(year = year, qtr = "a", slice = "area", sliceCode = Benchmark[8])
  Nashville <- qcew_api(year = year, qtr = "a", slice = "area", sliceCode = Benchmark[9])
  Philadelphia <- qcew_api(year = year, qtr = "a", slice = "area", sliceCode = Benchmark[10])
  Cincinati <- qcew_api(year = year, qtr = "a", slice = "area", sliceCode = Benchmark[11])
  Cleveland <- qcew_api(year = year, qtr = "a", slice = "area", sliceCode = Benchmark[12])
  Detriot <- qcew_api(year = year, qtr = "a", slice = "area", sliceCode = Benchmark[13])
  Milwaukee <- qcew_api(year = year, qtr = "a", slice = "area", sliceCode = Benchmark[14])
  St_Louis <- qcew_api(year = year, qtr = "a", slice = "area", sliceCode = Benchmark[15])
  return(rbind(Austin, Boston, Denver, Minneapolis, Seattle, Baltimore, Charlotte, Indianapolis, Nashville,
               Philadelphia, Cincinati, Detriot, Milwaukee, St_Louis))
}

write.csv(db18, file = "Benchmarks18.csv")
