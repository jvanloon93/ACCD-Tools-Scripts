library(tidyverse)
library(rjson)
library(blscrapeR)
library(usmap)


pathPrep <- function(path = "clipboard") {
  y <- if (path == "clipboard") {
    readClipboard()
  } else {
    cat("Please enter the path:\n\n")
    readline()
  }
  x <- chartr("\\", "/", y)
  writeClipboard(x)
  return(x)
}

High_Wage_Codes <- read.csv("C:/Users/jvanl/Desktop/HighWage Codes.csv")

MSA_Wage_Code <- as.vector(High_Wage_Codes$ï..PA_MSA)

USA_High_Wage_Code <- as.vector(High_Wage_Codes$USA)

MSA_High_Wage <- sapply(seq_along(MSA_Wage_Code), function(i) bls_api(MSA_Wage_Code[i]))

USA_High_Wage <- sapply(seq_along(USA_High_Wage_Code), function(i) bls_api(USA_High_Wage_Code[i]))
