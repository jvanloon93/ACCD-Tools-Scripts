install.packages("dplyr")
install.packages("ggplot2")
install.packages("concordance")
install.packages('sqldf')
library(rjson)
library(sqldf)
library(dplyr)
library(ggplot2)
library(concordance)

naics_code <- codedesc$NAICS

naics_code <- as.numeric(as.character(naics_code))

naics_description <- codedesc$NAICS.Desc

naics_description <- as.character(naics_description)

naics <- as.data.frame(cbind(naics_code, naics_description))

write.csv(naics, file = "Naics_Lookup.csv")

