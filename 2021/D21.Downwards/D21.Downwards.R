#Load packages
library(tidyverse)
library(extrafont)
library(scales)
library(cowplot)

#Get the data
readxl::read_excel("oil_production.xlsx") %>% 
  janitor::clean_names() %>% 
  select(1, 4) %>% 
  slice(265:336) -> oil_production

#Plot
ggplot() +
  geom_col(oil_production,
            mapping = aes(date, crude_oil_production_venezuela_monthly,
                          fill = crude_oil_production_venezuela_monthly),
            show.legend = FALSE) +
  scale_fill_gradient(low = "#9d0208", 
                      high = "#ffba08") +
  scale_y_continuous(breaks = seq(500, 2500, 500),
                     labels = c("0.5M", "1.0M", "1.5M", "2.0M", "2.5M")) +
  scale_x_datetime(labels = label_date_short(), date_breaks = '1 year') +
  labs(y = 'Crude Oil Production (In Barrels)',
       x = NULL,
       caption = "Made by **@luisfreii** | Source: **U.S. Energy Information Administration (EIA)**") +
  coord_cartesian(expand = FALSE) +
  theme(plot.margin = margin(25, 15, 10, 15),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.background = element_rect(color = "#F5F5F5",
                                        fill = "#F5F5F5"),
        plot.background = element_rect(color = "#F5F5F5",
                                       fill = "#F5F5F5"),
        panel.grid = element_blank(),
        plot.caption = ggtext::element_markdown(size = 8,
                                                color = "#525252",
                                                family = "Roboto"),
        axis.title = element_text(color = "#525252",
                                  family = "Roboto"),
        axis.text = element_text(color = "#525252",
                                 family = "Roboto")) -> plot

final_plot <- ggdraw(plot) +
  draw_text("Venezuela's Oil Production", 
            x = 0.55, 
            y = .9, 
            hjust = 0, 
            color = "#525252",
            fontface = 'bold',
            size = 24, 
            family = 'IBM Plex Sans') + 
  draw_text("From 2015 to 2020.", 
            x = 0.55, 
            y = .845, 
            hjust = 0, 
            color = '#525252',
            size = 14, 
            family = 'Fira Sans', 
            lineheight = 0.9) 

#Code to save the plot
# ggsave('D20.Downwards.svg',
#        width = 25,
#        height = 15,
#        units = 'cm',
#        dpi = 320)

# ggsave('D20.Downwards.png',
#        width = 25,
#        height = 15,
#        units = 'cm',
#        dpi = 320)
