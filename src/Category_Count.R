#AM Review paper figure prep

library(tidyverse)
library(stringr)

setwd("D:/OneDrive/GEM3_PostDoc/Review Paper AM")

animals <- read.csv("animals.csv", header = TRUE)
aq_marine <- read.csv("aquatic_marine.csv", header = TRUE)

animals$Cat <- c("Animals")
aq_marine$Cat <- c("Aquatic and Marine")

df <- rbind(animals, aq_marine)

df_plot <- df %>%
  group_by(Cat, Score) %>%
  tally()

df_plot$Score <- as.factor(df_plot$Score)

ggplot(data = df_plot, aes(x=Cat, y = n, fill = Score)) +
  geom_col(position = "dodge", colour="black") +
  scale_fill_brewer(palette = "Accent") +
  labs(
    x = "Category",
    y = "Count") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,7)) +
  theme_classic(base_size = 18)
