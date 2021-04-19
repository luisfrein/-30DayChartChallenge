#Load packages
library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
library(tidyr)
library(ggtext)
library(extrafont)

#Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 13)

roll_calls <-  tuesdata$roll_calls

episodes <- read_csv('GOT_episodes_v4.csv') %>% 
  janitor::clean_names()

#Unnest words and create bigrams
  episodes_bigrams <- episodes %>%
    unnest_tokens(bigram, summary, token = "ngrams", n = 2)
  
#Separate bigrams  
  bigrams_separated <- episodes_bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ")

#Filter bigrams  
  bigrams_filtered <- bigrams_separated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)
  
# new bigram counts:
  bigram_counts <- bigrams_filtered %>% 
    count(word1, word2, sort = TRUE)
  
#Bigram for plot  
  bigram_graph <- bigram_counts %>%
    filter(n >= 2) %>%
    graph_from_data_frame()
  
#Plot  
a <- grid::arrow(type = "closed", length = unit(.12, "inches"))

set.seed(1512)  
  ggraph(bigram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = a, end_cap = circle(.035, 'inches'),
                   color = "#131516") +
    geom_node_point(size = 4,
                    color = "#B2DDF7") +
    geom_node_text(aes(label = name), 
                   vjust = 1, 
                   hjust = 1,
                   color = '#384042',
                   family = 'Roboto') +
    labs(title = 'The Game of Bigrams',
        subtitle = 'Most common bigrams (word pairs) found in Game of Thrones episodes summary.',
        caption = '<br>Graph: **@luisfreii**| Source: **Kaggle**') +
    theme_void() +
      theme(plot.margin = margin(25, 15, 10, 15),
            plot.title = element_text(size = 20,
                                      color = "#525252",
                                      face = "bold",
                                      family = "Fira Sans"),
            plot.subtitle = element_markdown(size = 12,
                                             color = "#525252",
                                             family = "Fira Sans"),
            plot.caption = element_markdown(size = 8,
                                            color = "#525252",
                                            family = "Fira Sans"),
            plot.title.position = "plot",
            plot.caption.position = "plot")


#Code to save the plot. I went for SVG, and then used InkScape to convert to PNG for better Antialiasing
# ggsave("D18.Connections.png",
#        width = 22,
#        height = 15,
#        dpi = 500,
#        units = "cm",
#        type = "cairo-png")

# ggsave("D18.Connections.svg",
#        width = 22,
#        height = 15,
#        dpi = 500,
#        units = "cm")
