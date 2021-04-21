#Load packages
library(tidyverse)
library(extrafont)
library(gganimate)
library(scales)
library(ggimage)

#Get the data. Link: https://finance.yahoo.com/quote/DOGE-USD/history/
doge <- read_csv("DOGE-USD.csv") %>% 
  janitor::clean_names() 

#Add doge image
doge$image <- 'dogecoin.png'

#Plot
doge %>% 
  filter(!is.na(close)) %>% 
  ggplot(aes(date, close)) +
  geom_line(size = 1,
            color = "#A48328") +
  labs(y = 'Price (USD)', x = NULL,
       title = 'The Rise of Doge (2021)',
       subtitle = 'Daily DogeCoin price from the start of the\nyear to Doge Day (April 20).',
       caption = '<br>Visualization: **@luisfreii** | Source: **Yahoo Finance**') +
  theme(plot.title = element_text(family = 'IBM Plex Sans',
                                  size = 20,
                                  color = "#C9A940",
                                  face = 'bold'),
        plot.subtitle = element_text(family = 'Fira Sans',
                                     color = "#525252"),
        plot.caption = ggtext::element_markdown(family = 'Fira Sans',
                                                size = 8,
                                                color = "#525252"),
        plot.title.position = 'plot',
        plot.caption.position = 'plot',
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.background = element_rect('#F5F5F5'),
        panel.background = element_rect('#F5F5F5'),
        axis.title = element_text(color = "#525252",
                                  family = "Fira Sans"),
        axis.text = element_text(color = "#525252",
                                 family = "Fira Sans"),
        plot.margin = margin(25, 15, 10, 15)) -> p1

#Add animation. Data appears gradually.
a1 <- p1  + geom_image(aes(image = image), size = 0.8) + transition_reveal(date) 

anim <- animate(a1, 
                nframes = 26, end_pause = 10,
                fps = 10, 
                height = 12, width = 15, units = "cm", res = 150)

#Code to save the animation
#anim_save("D20.Upwards.gif", animation = last_animation())
