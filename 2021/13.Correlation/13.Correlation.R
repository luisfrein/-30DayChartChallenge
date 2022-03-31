#Load packages
library(tidyverse)
library(extrafont)
library(ggrepel)
library(ggtext)

#Get the data
corruption_perception <- readxl::read_xlsx("CPI2020_GlobalTablesTS_210125.xlsx", skip = 2) %>% 
  janitor::clean_names()

gdp <- readxl::read_xlsx("imf_gdp_pc.xlsx") 

#Filter gdp for only 2020 values
gdp %>% 
  select(1, 42) %>% 
  rename('value' = '2020' ) %>% 
  mutate(country_name = str_trim(country_name),
         value = as.numeric(value)) -> gdp

#Join dfs
left_join(corruption_perception, gdp, by = c('country' = 'country_name')) -> gdp_and_corruption


gdp_and_corruption %>% 
  ggplot(aes(cpi_score_2020, value, label = country, color = cpi_score_2020)) +
  geom_point(size = 4, 
             alpha = .75) +
  geom_text_repel(max.overlaps = 5,
                  family = "Fira Sans") +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  scale_color_gradient(low = '#282F3E',
                       high = "#22D37D") +
  labs(y = "GDP per capita",
       x = "Corruption Perception Index (2020)", 
       title = "Corruption Perception Index\nAnd GDP Per Capita ",
       subtitle = "Countries go from <span style='color:#282F3E;'>**highly corrupt**</span> to <span style='color:#22D37D;'>**very clean**</span>.",
       caption = '<br>Graph: **@luisfreii**<br>Sources: **Transparency International** & **IMF**') +
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
                                        family = "Fira Sans",
                                        hjust = 0),
        axis.title = element_text(color = "#525252",
                                  family = "Fira Sans"),
        axis.text = element_text(color = "#525252",
                                 family = "Fira Sans"),
        axis.ticks.y = element_blank(),
        legend.position = 'none',
        plot.title.position = "plot",
        plot.caption.position = "plot")

#Code to save the plot
# ggsave('13.Correlation.svg',
#        width = 26,
#        height = 16,
#        units = 'cm',
#        dpi = 320)
# 
# ggsave('13.Correlation.png',
#        width = 26,
#        height = 16,
#        units = 'cm',
#        dpi = 320)