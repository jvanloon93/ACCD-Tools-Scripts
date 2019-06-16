library(tidyverse)
library(rjson)
library(blscrapeR)
library(usmap)

ten_county_USA <- as.vector(c(fips('PA', county =  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')), 'US000'))

Employment_Code <- as.vector(sapply(seq_along(ten_county_USA), function(i) (paste('ENU', ten_county[i],'10010', sep = ""))))

Total_Wage_Codes <- as.vector(sapply(seq_along(ten_county_USA), function(i) (paste('ENU', ten_county[i],'30010', sep = ""))))

lst_Employment <- lapply(seq_along(Employment_Code), function(i) bls_api(Employment_Code[i]))

lst_Wages <- lapply(seq_along(Total_Wage_Codes), function(i) bls_api(Total_Wage_Codes[i]))

names(lst_Employment) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland', 'USA')

names(lst_Wages) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland', 'USA')

Annual_avg_2018 <- function(df) {
  df<- subset(df, year == 2018)
  return(mean(df$value))
}

Annual_sum_2018 <- function(df) {
 df <-subset(df, year == 2018)
  return(sum(df$value))
}

Annual_Employment_18 <- sapply(seq_along(lst_Employment), function(i) Annual_avg_2018(as.data.frame(lst_Employment[[i]])))

Annual_Employment_18 <- as.data.frame(Annual_Employment_18)

rownames(Annual_Employment_18) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland', 'USA' )

Annual_Wages_18 <- sapply(seq_along(lst_Wages), function(i) Annual_sum_2018(as.data.frame(lst_Wages[[i]])))

Annual_Wages_18 <- as.data.frame(Annual_Wages_18)

rownames(Annual_Wages_18) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland', 'USA')

df_18 <- cbind.data.frame(Annual_Employment_18, Annual_Wages_18)

Annual_avg_2017 <- function(df) {
  df<- subset(df, year == 2017)
  return(mean(df$value))
}

Annual_sum_2017 <- function(df) {
  df <-subset(df, year == 2017)
  return(sum(df$value))
}

Annual_Employment_17 <- sapply(seq_along(lst_Employment), function(i) Annual_avg_2017(as.data.frame(lst_Employment[[i]])))

Annual_Employment_17 <- as.data.frame(Annual_Employment_17)

rownames(Annual_Employment_18) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland', 'USA' )

Annual_Wages_17 <- sapply(seq_along(lst_Wages), function(i) Annual_sum_2017(as.data.frame(lst_Wages[[i]])))

Annual_Wages_17 <- as.data.frame(Annual_Wages_17)

rownames(Annual_Wages_18) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland', 'USA')

df_17 <- cbind.data.frame(Annual_Employment_17, Annual_Wages_17)

df_fn <- cbind(df_17, df_18)

df_fn <- rbind(df_fn, colSums(df_fn[1:10, 1:4]))

rownames(df_fn)[12] <- "Region"

(df_fn[12,2]/df_fn[12,1])*1000
(df_fn[12,4]/df_fn[12,3])*1000
(df_fn[11,2]/df_fn[11,1])*1000
(df_fn[12,4]/df_fn[12,3])*1000




