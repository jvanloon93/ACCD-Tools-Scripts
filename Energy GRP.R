library(tidyverse)
library(readxl)


lx <- 1:14

x <- lapply(seq_along(lx), function(i) read_excel("GRP.xlsx", sheet = lx[i]))

#names of each data frame 

names <- c()

excel_sheets(x)

for (i in 1:length(x)) {
  assign(paste0("names", i), as.data.frame(names()[[i]]))
}