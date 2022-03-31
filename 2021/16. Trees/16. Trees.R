#Load packages
library(tidyverse)
library(extrafont)
library(ggtext)
library(treemapify)
library(glue)

#Get the data. Link: https://www.kaggle.com/rezaghari/game-of-thrones.
#Icons: https://iconscout.com/icon-pack/game-of-thrones#
characters <- read_csv('characters_v4.csv') %>% 
  janitor::clean_names()

#explore
characters %>% 
  mutate(label = glue("{character}\n{episodes_appeared} episodes")) -> characters

characters %>% 
  slice(1:30) %>% 
  ggplot(aes(area = episodes_appeared, label = label, fill = episodes_appeared)) +
  geom_treemap(show.legend = FALSE,
               color = "#02223C") +
  geom_treemap_text(place = "topleft", 
                    reflow = TRUE, 
                    family = "IBM Plex Sans", 
                    color = "#FCEEEF",
                    size = 14) +
  scale_fill_gradient(low = "#503502",
                      high = "#F9AF24") +
  labs(title = "**The 30 Game of Thrones Characters with the Most Appearances**",
       caption = "<br>Graph: **@luisfreii** | Source: **Kaggle**") +
  theme(plot.background = element_rect("#02223C"),
        panel.background = element_rect("#02223C"),
        plot.margin = unit(c(2, 2, 1, 2), "lines"),
        plot.title = element_markdown(hjust = .5, 
                                      family = "Lato", 
                                      size = 18, 
                                      color = "#FEF8EB",
                                      margin = margin(b = .8, unit = "cm")),
        plot.subtitle = element_markdown(hjust = .5, 
                                         family = "Lato", 
                                         color = "#0E0E1B"),
        plot.caption = element_markdown(hjust = .5, 
                                        family = "Lato", 
                                        color = "#FEF8EB")
  )

#Code to save the plot. I added the icons on Inkscape. To do that I saved the plot as svg first
# ggsave('D16.Trees2.svg',
#        width = 21,
#        height = 16,
#        dpi = 320,
#        units = 'cm')

# ggsave('D16.Trees2.png',
#        width = 21,
#        height = 16,
#        dpi = 320,
#        units = 'cm')
