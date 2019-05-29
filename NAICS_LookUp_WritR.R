library(rjson)
library(blscrapeR)
library(sqldf)
library(dplyr)
library(ggplot2)

naics_code <- blscrapeR::niacs

write.csv(naics_code, file = "Naics_Lookup.csv")

