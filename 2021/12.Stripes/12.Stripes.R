#Load packages
library(tidyverse)
library(extrafont)
library(scales)

#Get the data. Link to it: https://www.kaggle.com/kaushiksuresh147/ethereum-cryptocurrency-historical-dataset
eth <- read.csv("Ethereum Historical Data.csv") %>% 
  janitor::clean_names()  

#Mutate date column and price column
eth %>% 
  mutate(i_date = lubridate::mdy(i_date),
         price = str_remove(price, ","),
         price = as.numeric(price)) -> eth

#Plot
eth %>% 
  ggplot(aes(i_date, 1, fill = price)) +
  geom_tile() +
  scale_fill_gradient(low = "#4E216E", 
                      high = "#5CEFFF",
                      limits = c(5, 2000),
                      breaks = seq(300, 1800, 300),
                      labels = c("300", "600", "900", "1,200", "1,500", "1,800")) +
  scale_x_date(labels = label_date_short(),
               date_breaks = "4 month") +
  coord_cartesian(expand = FALSE) +
  labs(fill = "Price (USD)",
       y = NULL, 
       x = NULL,
       title = "Ethereum Price (USD)",
       subtitle = "Daily Ethereum price. March 2016 to March 2021.",
       caption = "Made by **@luisfreii** | Source: **Kaggle**") +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = .5, 
                               barwidth = unit(20, 'lines'), 
                               barheight = unit(.5, 'lines'))) +
  theme(legend.position = "top",
        plot.margin = margin(25, 15, 10, 15),
        panel.background = element_rect(color = "#F5EFFA"),
        plot.background = element_rect(color = "#F5EFFA"),
        panel.grid = element_blank(),
        plot.title = element_text(size = 16,
                                  color = "#525252",
                                  face = "bold",
                                  family = "Fira Sans",
                                  hjust = .5),
        plot.subtitle = element_text(size = 12,
                                     color = "#525252",
                                     margin = margin(t = .2, b = .5, unit = "cm"),
                                     family = "Fira Sans",
                                     hjust = .5),
        plot.caption = ggtext::element_markdown(size = 7,
                                                color = "#525252",
                                                family = "Fira Sans"),
        legend.text = element_text(size = 9,
                                   family = "Fira Sans",
                                   color = "#525252"),
        legend.title = element_text(size = 10,
                                    family = "Fira Sans",
                                    color = "#525252"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
  
#Code to save the plot
# ggsave("12.Stripes.png",
#        width = 20,
#        height = 10,
#        units = "cm",
#        dpi = 320,
#        type = "cairo-png")
