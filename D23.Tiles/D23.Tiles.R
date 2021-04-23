#Load packages
library(tidyverse)
library(extrafont)
library(lubridate)
library(cowplot)

#Get the data
monthly_inflation_raw <- read_csv('monthly_inflation_rate.csv')

#Turn fecha column to date type, get year and month, and make BCV column numeric
monthly_inflation_raw %>% 
  mutate(fecha = str_c(fecha, '01', sep = '-'),
         fecha = myd(fecha),
         year = year(fecha),
         month = month(fecha),
         BCV = as.numeric(str_remove(BCV, '%'))) -> monthly_inflation

#Tibble with annotations
tibble(
  y = c(2017, 2018, 2019, 2020, 2020.75, 2020.75),
  x = c(-.4, -.1, 0, .1, 1, 12),
  labels = c('2017', '2018', '2019', '2020', 'Jan', 'Dec'),
  angle = c(0, 0, 0, 0, 350, 320)
) -> annotations

#Plot
  ggplot() +
  geom_tile(monthly_inflation,
            mapping = aes(month, year, fill = BCV),
            color = '#F5F5F5') +
  scale_x_continuous(breaks = seq(1, 12, 1),
                     labels = month.abb,
                     limits = c(-5.75, 13)) +
  scale_y_continuous(breaks = seq(2017, 2020, 1),
                     limits = c(2015, 2021)) +
  geom_text(annotations,
            mapping = aes(x = x, y = y, label = labels, angle = angle),
            size = 3.5,
            family = 'Fira Sans',
            color = '#525252') +
  scale_fill_viridis_c(option = 'cividis',
                       breaks = seq(40, 160, 40)) +
  coord_polar(start = 4 * pi / 3, theta = 'x') +
  labs(caption = 'Visualization: **@luisfreii** | Source: **Central Bank of Venezuela**',
       fill = 'Rate (%)') +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = .5, 
                               barwidth = unit(.75, 'lines'), 
                               barheight = unit(7.5, 'lines'))) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.background = element_rect(color = "#F5F5F5",
                                        fill = "#F5F5F5"),
        plot.background = element_rect(color = "#F5F5F5",
                                       fill = "#F5F5F5"),
        plot.caption = ggtext::element_markdown(size = 7,
                                                color = "#525252",
                                                family = "Fira Sans"),
        legend.position = c(.045, .6),
        legend.background = element_rect(fill = "#F5F5F5"),
        legend.title = element_text(color = "#525252",
                                    size = 8,
                                    family = "Fira Sans"),
        legend.text = element_text(family = "Fira Sans",
                                   color = "#525252",
                                   size = 6)) -> plot

#Add title and subtitle
final_plot <- ggdraw(plot) +
  draw_text("Venezuela",
            x = 0.27, 
            y = .86, 
            hjust = 0, 
            color = "#525252",
            fontface = 'bold',
            size = 22, 
            family = 'IBM Plex Sans') +
  draw_text("Monthly Inflation Rate",
            x = 0.27, 
            y = .81, 
            hjust = 0, 
            color = "#525252",
            size = 9, 
            family = 'IBM Plex Sans')
    
  
#Code to save the plot. White borders trimmed with Inkscape
# ggsave('D23.Tiles.svg',
#        width = 28,
#        height = 14,
#        units = 'cm',
#        dpi = 320)

# ggsave('D23.Tiles.png',
#        width = 28,
#        height = 14,
#        units = 'cm',
#        dpi = 320)
