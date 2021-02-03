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

write.csv(df, "outputs/all_reviewed.csv", row.names = FALSE)

#pulling out just the 2, 3, 4 and "a"

df_234 <- NULL
df_234 <- rbind(df_234, df[grepl("2", df$Notes),])
df_234 <- rbind(df_234, df[grepl("3a", df$Notes),])
df_234 <- rbind(df_234, df[grepl("3", df$Notes),])
df_234 <- rbind(df_234, df[grepl("4a", df$Notes),])
df_234 <- rbind(df_234, df[grepl("4", df$Notes),])

#note, need to wipe the rest of the text out of the file. for now, just exporting an d reading back in. note also that some of the files had other numebrs in them (like animals has a paper scored 1, but the note had 4 in it due to pop number). Manual review occured using the "Score" column". Also, above grep keeps 3b and 4b in there due to looking for "3" and "4" will need to adjust grepl later. Put "0" for anything that didn't belong

write.csv(df_234, "outputs/df_234_full_note.csv", row.names = FALSE)

df_234_clean <- read.csv("outputs/df_234_full_note.csv")

#filter out zeros and tally

df_plot <- df_234_clean %>%
  group_by(Cat, Score) %>%
  filter(Score != 0) %>%
  tally()

#make Score a factor (as opposed to numeric/integer)

df_plot$Score <- as.factor(df_plot$Score)

count_plot <- ggplot(data = df_plot, aes(x=Cat, y = n, fill = Score)) +
  geom_col(position = "dodge", colour="black") +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    x = "Category",
    y = "Count") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,50)) +
  theme_classic(base_size = 18)

#save plot
ggsave("plots/count_plot.png", count_plot, width = 10, height = 6)
