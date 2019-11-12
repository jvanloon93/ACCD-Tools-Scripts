library(tidyverse)
library(tidyr)
library(httr)
library(blscrapeR)
library(readr)
library(ggplot2)


PITex <- read_csv('DATA_Pittsburgh__PA_Exports____USD.csv')

PITex <- PITex %>%
  gather("year", "export", `2005 (Full Year)`:`2018 (Full Year)`) 

PITex <- na.omit(PITex)

PITex$year <- as.numeric(substr(PITex$year, 1, 4))

PITex %>% 
  filter(Code != "RES" & Code != "ALL") %>%
  ggplot(aes(x = year, y = export, group = Description)) + geom_line(aes(color = Description))

#cool, but about 

industry_codes <- c("212", '331', "325", "333", "334")

BLS_estab_Codes <- as.vector(sapply(seq_along(industry_codes), function(i) (paste("ENUC3830205", industry_codes[i], sep = ""))))

BLS_emp_Codes <- as.vector(sapply(seq_along(industry_codes), function(i) (paste("ENUC3830105", industry_codes[i], sep = ""))))

dfemp <- bind_rows(lapply(seq_along(BLS_emp_Codes), 
                 function(i) bls_api(
                   BLS_emp_Codes[i],
                   startyear = 2005,
                   endyear = 2018,
                   annualaverage = TRUE,
                   registrationKey = Sys.getenv("BLS_KEY")
                 )))

dfestab <- bind_rows(lapply(seq_along(BLS_estab_Codes), 
                            function(i) bls_api(
                              BLS_estab_Codes[i],
                              startyear = 2005,
                              endyear = 2018,
                              annualaverage = TRUE,
                              registrationKey = Sys.getenv("BLS_KEY")
                            )))

# trim the fat 

dfestab <- dfestab %>%
  select(year, value, seriesID) %>%
  rename(Establishments = value, Code = seriesID)

dfestab$Code <- substr(dfestab$Code, 12, 14)

dfemp <- dfemp %>%
  select(year, value, seriesID) %>%
  rename(Employment = value, Code = seriesID) 

dfemp$Code <- substr(dfemp$Code, 12, 14)

# but still no key identifier to match 

PITex <- PITex %>%
  mutate(key = paste(Code, year))

dfestab <- dfestab %>%
  mutate(key = paste(Code, year)) %>%
  select(Establishments, key)

dfemp <- dfemp %>%
  mutate(key = paste(Code, year)) %>%
  select(Employment, key)

PITex <- left_join(left_join(PITex, dfestab, by = "key"), dfemp, by = "key")


PITex %>% 
  filter(Code != "RES" & Code != "ALL") %>%
  ggplot(aes(x = year, y = export, group = Description)) + geom_line(aes(color = Description)) +
  geom_point(aes(x = year, y = Establishments))

# putting estab/emp on the z axis? 




