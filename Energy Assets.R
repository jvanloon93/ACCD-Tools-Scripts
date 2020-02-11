library(readxl)
library(tidyverse)
install.packages("wesanderson")
library(wesanderson)

df<- read_excel("Energy Assets.xlsx", col_names = TRUE)

name <- factor(df$Category)

df_tidy <- df %>%
  gather(key = "Category", value = "Count", -County) %>%
  replace_na(list(Count = 0)) %>%
  group_by(Category) %>%
  mutate(Percent_of_Category = Count/ sum(Count)) %>%
  ungroup() %>%
  filter(Count != 0)

o <- ggplot(df_tidy, aes(x = County, y = Count, fill = factor(Category)))
  
o + geom_col() + scale_color_manual(values = wes_palette("GrandBudapest1"))





