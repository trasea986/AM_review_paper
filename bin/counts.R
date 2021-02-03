library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggpubr)
library(cowplot)

plants <- read.csv("data/plants.csv", stringsAsFactors = F, header = T)
seeds <- read.csv("data/seed_zone_results.csv", header=T)
#bind both datasets together
combined <- rbind(plants, seeds)

#subset relevant papers
combined_relevant <- NULL
combined_relevant <- rbind(combined_relevant, combined[grepl("3a", combined$Notes),])
combined_relevant <- rbind(combined_relevant, combined[grepl("4a", combined$Notes),])
combined_relevant <- rbind(combined_relevant, combined[grepl("3b", combined$Notes),])
combined_relevant <- rbind(combined_relevant, combined[grepl("4b", combined$Notes),])

#get counts for subsections
a <- sum(grepl("3a", combined$Notes)) + sum(grepl("4a", combined$Notes))
b <- sum(grepl("3b", combined$Notes)) + sum(grepl("4b", combined$Notes))
relevant <- sum(grepl("3a", combined$Notes)) + sum(grepl("4a", combined$Notes))+sum(grepl("3b", combined$Notes)) + sum(grepl("4b", combined$Notes))
percentage_a <- a*100/relevant
percentage_b <- b*100/relevant

#add category description
combined_relevant$Category <- ifelse(grepl("3a", combined_relevant$Notes),"3a",
                                 ifelse(grepl("3b", combined_relevant$Notes), "3b",
                                 ifelse(grepl("4a", combined_relevant$Notes), "4a","4b")))

#plot timeline
timeline <- combined_relevant %>% 
  group_by(Publication.Year, Category) %>% 
  summarise(n()) %>% ggplot(aes(x=Publication.Year, y=`n()`, color=Category))+
  geom_line()+geom_point()+
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,25)) +
  theme_classic(base_size = 16)+
  xlab("Publication Year")+
  ylab("Publication Count")

#plot total counts
counts_total <- combined_relevant %>% 
  group_by(Category) %>% 
  summarise(n()) %>% ggplot(aes(x=Category, y=`n()`, fill=Category))+
  geom_bar(stat="identity", color = "black")+
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,150)) +
  theme_classic(base_size = 16)+
  xlab("Category")+
  ylab("Publication counts")#+
  ggtitle("AM in plant studies")

combined_plot <- plot_grid(timeline, counts_total, labels = c('A)', 'B)'), label_size = 16)

combined_plot

ggsave("plots/plant_plot.png", combined_plot, width = 10, height = 6)


#subset papers which used genomic methods
genomic <- combined[grepl("genomic", combined$Notes),]
genomic_a <- genomic[grepl("3a|4a", genomic$Notes),]

genomic_a_notrait <- genomic_a[grepl("no trait", genomic_a$Notes),] # 4: 3 with climate association, 1 without climate assoc
genomic_a_trait <- genomic_a[!(genomic_a$Notes %in% genomic_a_notrait$Notes),] #9: 8 with climate association, 1 without climate assoc

#subset papers which used genetic methods
genetic <- combined[grepl("genetic", combined$Notes),]
genetic_a <- genetic[grepl("3a|4a", genetic$Notes),]
genetic_a_no_trait <- genetic_a[grepl("no trait", genetic_a$Notes),] # 18 with climate association, 1 without climate assoc
genetic_a_trait <- genetic_a[!(genetic_a$Notes %in% genetic_a_no_trait$Notes),] #2







str(combined_relevant)


sum(grepl("3b", seeds$Notes))
sum(grepl("4a", seeds$Notes))
sum(grepl("4b", seeds$Notes))
sum(grepl("5", seeds$Notes))
sum(grepl("3a", seeds$Notes))
sum(grepl("3a", plants$Notes))
sum(grepl("3b", plants$Notes))
sum(grepl("4a", plants$Notes))
sum(grepl("4b", plants$Notes))
sum(grepl("5", seeds$Notes))
nrow(plants)
nrow(seeds)






'''
184 relevant (either 3ab (36), 
4ab (148));
63 review;
43 category two;
16 not sure which category (but certainly not 3 or 4);
273 - category one;


'''
