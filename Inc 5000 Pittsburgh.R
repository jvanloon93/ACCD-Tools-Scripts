library(tidyverse)
library(ggplot2)


df <- read.csv("https://query.data.world/s/lg5qczh7bbx2wfu6ckr6qkap57wq53", header=TRUE, stringsAsFactors=FALSE);

counts <- df %>%
  count(df$X_...metro) %>%
  rename(Geography = 'df$X_...metro') %>%
  mutate(Pittsburgh = ifelse(Geography == 'Pittsburgh, PA', T, F)) %>%
  arrange(desc(n))
  
counts  <- counts[-1, ] 

counts$Geography <- factor(counts$Geography, levels = counts$Geography[order(counts$n, decreasing = TRUE)])

ggplot(counts, aes(x = Geography, y = n, fill = Pittsburgh)) + 
  geom_col()+ 
  theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 1)) +
  geom_text(aes(label = n, angle = 90))
