install.packages('dplyr')
install.packages('rjson')
install.packages("blscrapR")
library(dplyr)
library(rjson)
library(blscrapeR)
library(ggplot2)
library(usmap)

ten_county <- as.vector(fips('PA', county =  c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')))

Unemployment <- as.vector(sapply(seq_along(ten_county), function(i) (paste('LAUCN',ten_county[i],'0000000004', sep = ""))))

names(Unemployment) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')

Labor_Force <- as.vector(sapply(seq_along(ten_county), function(i) (paste('LAUCN',ten_county[i],'0000000006', sep = ""))))

names(Labor_Force) <- c('Allegheny', 'Armstrong', 'Beaver', 'Butler', 'Fayette','Greene', 'Indiana', 'Lawrence', 'Washington', 'Westmoreland')

df_UN <- bls_api(c(unname(Unemployment)))

df_LF <- bls_api(c(unname(Labor_Force)))


df_Un$value <- as.double(as.character(df_Un$value))

df_LF$value <- as.double(as.character(df_LF$value))

Mar_19_Un <- df_UN %>%
  filter(year == 2019 & period == 'M03') 

Mar_19_LF <- df_LF %>%
  filter(year == 2019 & period == 'M03')

Un <- sum(Mar_19_Un$value)

LF <-sum(Mar_19_LF$value)

(Un/LF)*100
