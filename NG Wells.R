library(tidyverse)

ng <- list(read.csv("NG Allegheny.csv")[, -10], read.csv("NG Armstrong.csv")[, -10], read.csv("NG Beaver.csv")[, -10], read.csv("NG Butler.csv")[, -10],
           read.csv("NG Fayette.csv")[, -10], read.csv("NG Greene.csv")[, -10], read.csv("NG Indiana.csv")[, -10], read.csv("NG Lawrence.csv")[, -10], 
           read.csv("NG washington.csv")[, -10], read.csv("NG Westmoreland.csv")[, -10])

Active <- ng %>%
  bind_rows() %>%
  group_by(Well_County)%>%
  filter(Well_Status == "Active") %>%
  summarise(County_Total_Active = n())

Producing <- ng  %>%
  bind_rows() %>%
  group_by(Well_County) %>%
  filter(Production_Indicator == "Yes") %>%
  summarise(County_Total_Producing = n())

df <- inner_join(Active, Producing, by = "Well_County") %>%
  select(Well_County, County_Total_Producing, County_Total_Active)

write.csv(df, "NG Wells by County.csv")

df_tidy <- df %>%
  gather(key = "Category", value = "Count", -Well_County)


