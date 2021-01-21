#AM Review paper figure prep

library(tidyverse)
library(stringr)

#data prep: need to remove all non-text data from the file list

#bring in each file individually. doing this to make it easier to edit as necessary (as opposed to looping rbinding)
animals <- read.csv("data/animals.csv", header = TRUE)
animals_NR <- read.csv("data/animals_not_relevant.csv", header = TRUE)
aq_marine <- read.csv("data/aquatic_marine.csv", header = TRUE)
aq_marine_NR <- read.csv("data/aquatic_marine_not_relevant.csv", header = TRUE)
plants <- read.csv("data/plants.csv", header = TRUE)
plants_NR <- read.csv("data/plants_not_relevant.csv", header = TRUE)

#add category tag
animals$Cat <- c("Animals")
animals_NR$Cat <- c("Animals")
aq_marine$Cat <- c("Aquatic and Marine")
aq_marine_NR$Cat <- c("Aquatic and Marine")
plants$Cat <- c("Plants")
plants_NR$Cat <- c("Plants")


df <- rbind(animals, animals_NR, aq_marine, aq_marine_NR, plants, plants_NR)

df$Score <- df$Notes

df_plot <- df %>%
  filter(!is.na(Notes)) %>%
  filter(Notes != "1") %>%
  filter(Notes != "") %>%
  filter(Notes != "b") %>%
  group_by(Cat, Score) %>%
  tally()

df_plot$Score <- as.factor(df_plot$Score)

#Add in NA for zeroes to get consistant width, missing Animals 4
animals_4 <- data.frame("Animals", as.factor(4), 0)
colnames(animals_4) <- c("Cat", "Score", "n")
df_plot <- rbind(df_plot, animals_4)

ggplot(data = df_plot, aes(x=Cat, y = n, fill = Score)) +
  geom_col(position = "dodge", colour="black") +
  scale_fill_brewer(palette = "Accent") +
  labs(
    x = "Category",
    y = "Count") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,50)) +
  theme_classic(base_size = 18)


#could also graph through time for each category
df_plot_time <- df %>%
  filter(!is.na(Notes)) %>%
  filter(Notes != "1") %>%
  filter(Notes != "") %>%
  filter(Notes != "b") %>%
  group_by(Cat, Score, Publication.Year) %>%
  tally()

df_plot_time2 = df_plot_time %>% group_by(Cat, Score) %>% arrange(Publication.Year) %>% mutate(cs = cumsum(n))

#problem here is that cumulative sum for the publication counts is missing whatever the max was



ggplot(data = df_plot_time2, aes(x=Publication.Year, y = cs, color = Score)) +
  geom_line(size = 2) +
  scale_color_brewer(palette = "Accent") +
  facet_wrap(~Cat) +
  labs(
    x = "Year",
    y = "Cumulative Count") +
  theme_classic(base_size = 18)

ggplot(data = df_plot_time2, aes(x=Publication.Year, y = n, color = Score)) +
  geom_line(size = 2) +
  scale_color_brewer(palette = "Accent") +
  facet_wrap(~Cat) +
  labs(
    x = "Year",
    y = "Cumulative Count") +
  theme_classic(base_size = 18)
