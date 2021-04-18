#Load packages
library(tidyverse)
library(extrafont)
library(ggtext)
library(ggforce)
library(glue)

#Get the data. Link: https://www.kaggle.com/rezaghari/game-of-thrones
episodes <- read_csv('GOT_episodes_v4.csv') %>% 
  janitor::clean_names()

#explore
#Annotation
annotation <- episodes %>% 
  slice(73) %>% 
  mutate(label = glue("The last episode of\nthe show is also\nthe one with the\nlowest rating ({rating})"))

summary(episodes$duration) -> duration_summ

#Plot
  ggplot(episodes, aes(season, rating, color = duration, size = critics_reviews)) +
  geom_point(alpha = .9) +
  geom_mark_circle(annotation, 
                   mapping = aes(season, rating, label = title, description = label),
                   label.fill = NA,
                   label.buffer = unit(5, 'mm'),
                   label.width = unit(50, 'mm'),
                   label.colour = c('#525252', '#525252'),
                   color = 'black') +
  scale_color_gradient(low = "#22D37D",
                       high = "#282F3E") +
  labs(y = "Rating",
       x = "Season", 
       title = "Game of Thrones Through the Seasons",
       subtitle = glue("Episodes go from <span style='color:#22D37D;'>**{duration_summ[[1]]}**</span> minutes to <span style='color:#282F3E;'>**{duration_summ[[6]]}**</span> minutes.<br>Dot size is proportional to the number of reviews written by IMDb critics."),
       caption = '<br>Graph: **@luisfreii**| Source: **Kaggle**') +
  theme(plot.margin = margin(25, 15, 10, 15),
        panel.background = element_rect("#F5F5F5"),
        plot.background = element_rect("#F5F5F5"),
        panel.grid.major.y  = element_line(color = "#EBEBEB"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20,
                                  color = "#525252",
                                  face = "bold",
                                  family = "Fira Sans"),
        plot.subtitle = element_markdown(size = 12,
                                         color = "#525252",
                                         margin = margin(t = .2, b = .5, unit = "cm"),
                                         family = "Fira Sans"),
        plot.caption = element_markdown(size = 8,
                                        color = "#525252",
                                        family = "Fira Sans"),
        axis.title = element_text(color = "#525252",
                                  family = "Fira Sans"),
        axis.text = element_text(color = "#525252",
                                 family = "Fira Sans"),
        axis.ticks.y = element_blank(),
        legend.position = 'none',
        plot.title.position = "plot",
        plot.caption.position = "plot")
  
#Code to save the plot. Circle and wolf icon added using Inkscape
  # ggsave('17.Pop Culture.svg',
  #        width = 18,
  #        height = 16,
  #        units = 'cm',
  #        dpi = 320)
