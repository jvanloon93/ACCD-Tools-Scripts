library(tidycensus)
library(tidyverse)
library(ggplot2)

df <- read.csv(file = 'C:/Users/jvanloon/Desktop/2018_Pop_MSA.csv')
#American Fact Finder CSV, need to figure out how to 

df <- df %>%
select(c(Geography, Population.Estimate..as.of.July.1....2018)) %>%
arrange(desc(df_pop$Population.Estimate..as.of.July.1....2018)) %>%
  top_n(30)

df %>%
  ggplot(aes(x = Population.Estimate..as.of.July.1....2018, y = reorder(Geography, Population.Estimate..as.of.July.1....2018))) +
  geom_point()

write.csv(df, file = "MSA_Sorted.csv")