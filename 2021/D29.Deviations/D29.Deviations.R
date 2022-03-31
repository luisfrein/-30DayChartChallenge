#Load packages
library(tidyverse)
library(extrafont)
library(ggtext)
library(ggdist)

#Read the data. Economic freedom index link: https://www.heritage.org/index/explore?view=by-region-country-year&u=637552502077689344
corruption_perception <- readxl::read_xlsx("CPI2020_GlobalTablesTS_210125.xlsx", skip = 2) %>% 
  select(1:4, 6) %>% 
  janitor::clean_names()

#Plot
corruption_perception %>% 
  filter(!is.na(cpi_score_2020)) %>% 
  ggplot(aes(fct_reorder(region, cpi_score_2020), cpi_score_2020)) +
  stat_halfeye(side = 'left',
               scale = .5,
               aes(fill = stat(cut_cdf_qi(
    cdf,
    .width = c(.5, .8, .95),
    labels = scales::percent_format()
  )))) +
  stat_dotsinterval(scale = 0.5, point_size = 1, fill = '#A8764D') +
  scale_fill_brewer(direction = -1, na.translate = FALSE, palette = 'YlOrBr') +
  scale_x_discrete(labels = c('Sub-Saharan\nAfrica', 'Europe and\nCentral Asia', 'America', 'Asia-Pacific', 'Middle East and\nNorth Africa', 'European Union')) +
  scale_y_continuous(breaks = seq(0, 80, 20)) +
  labs(title = "Corruption Perception Index (2020)",
       subtitle = "Index scores by region. Low scores are associated\nwith highly corrupt countries. High scores are\nassociated with very clean countries.",
       caption = 'Visualization: **@luisfreii** | Source: **Transparency International**',
       x = NULL,
       y = 'CPI Score',
       fill = "Interval") +
  theme(panel.background = element_rect('#EBECF5'),
        plot.background = element_rect('#EBECF5'),
        panel.grid = element_blank(),
        plot.title.position = 'plot',
        plot.caption.position = 'plot',
        plot.title = element_text(family = 'Lato',
                                 size = 20,
                                 color = "#525252"),
        plot.subtitle = element_text(family = 'Fira Sans',
                                     color = '#525252'),
        plot.caption = ggtext::element_markdown(family = 'Fira Sans',
                                                size = 8,
                                                color = "#525252"),
        axis.title = element_text(family = 'Fira Sans',
                                  color = "#525252"),
        axis.text = element_text(family = 'Fira Sans',
                                 color = "#525252"),
        plot.margin = margin(25, 15, 10, 15),
        axis.ticks.y = element_blank(),
        legend.position = c(.05, .83),
        legend.background = element_rect('#EBECF5'),
        legend.text = element_text(family = 'Fira Sans',
                                   color = "#525252",
                                   size = 8),
        legend.title = element_text(family = 'Fira Sans',
                                    color = "#525252",
                                    size = 10))

#Code to save the plot
# ggsave('D29.Deviations.svg',
#        width = 24,
#        height = 14,
#        units = 'cm',
#        dpi = 320)
# 
# ggsave('D29.Deviations.png',
#        width = 24,
#        height = 14,
#        units = 'cm',
#        dpi = 320)