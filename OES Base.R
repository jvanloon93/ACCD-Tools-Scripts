library(blscrapeR)
library(tidyverse)

Top_occ <- read.csv("Top_Occ.csv")

# Buyers and Purshasing agents mis-coded Changed 

Msas_codes <- c("38300", "18140", "17460")

Msa_Names <- c("Pittsburgh, PA", "Columbus, OH", "Cleveland-Elyria, OH")

Msadf <- data.frame(Msas_codes, Msa_Names)

oes_all <- read.csv("MSA_M2018_dl.csv")

oesmsa <- oes_all %>%
  filter(AREA %in% Msas_codes & OCC_CODE %in% Top_occ$SOC) %>%
  select(AREA_NAME, OCC_TITLE, TOT_EMP, A_MEAN, LOC.QUOTIENT)

natoes <- read.csv("national_M2018_dl.csv") %>%
  filter(OCC_CODE %in% Top_occ$SOC & OCC_GROUP == "detailed") %>% 
  mutate(AREA_NAME = "Nation", LOC.QUOTIENT = "1.00") %>%
  select(AREA_NAME, OCC_TITLE, TOT_EMP, A_MEAN, LOC.QUOTIENT)

#best form for analysis

oes <- bind_rows(oesmsa, natoes)

oes <- gather(oes, "Measure", "value", 3:5) %>%
  unite(temp, AREA_NAME, 3) %>%
  spread(temp, value)

oes <- oes %>% select(OCC_TITLE, Nation_TOT_EMP, Nation_A_MEAN, `Pittsburgh, PA_TOT_EMP`, `Pittsburgh, PA_A_MEAN`, `Pittsburgh, PA_LOC.QUOTIENT`,
                      `Cleveland-Elyria, OH_TOT_EMP`, `Cleveland-Elyria, OH_A_MEAN`, `Cleveland-Elyria, OH_LOC.QUOTIENT`, `Columbus, OH_TOT_EMP`, `Columbus, OH_A_MEAN`, `Columbus, OH_LOC.QUOTIENT`)

write.csv(oes, file = "oes2.csv")