#Load packages
library(tidyverse)
library(extrafont)
library(ggtext)

#Idea and numbers from Kurzgesagt's video: https://www.youtube.com/watch?v=dGiQaabX3_o
#Had to make the human number larger than 200000. Otherwise the bar wouldn't be visible
tibble(
  x = c(4500000000, 15000000),
  y = 1,
  z = c("earth", "humans"),
  percent = x / lag(x),
  color = c("#1F1F1F", "#F50021")
) -> earth_history

#Plot
ggplot(earth_history, aes(x = 1, y = x, fill = fct_rev(color))) +
  geom_col(position = "stack") +
  scale_fill_identity() +
  labs(x = "x", y = "y") +
  coord_flip(clip = "off") +
  annotate("text", x = 1, y = 2250000000, label = "EARTH’S AGE\n4.5 BILLION YEARS", color = "#F5F5F5",
           family = "Libre Franklin",
           size = 4.5,
           fontface = "bold") +
  annotate("text", x = 1.65, y = 4000000000, label = "HUMANITY’S AGE\n200,000 YEARS", hjust = 0,
           color = "#F50021",
           family = "Libre Franklin") +
  annotate("curve", 
           x = 1.42,
           xend = 1.55,
           y = 4500000000,
           yend = 4500000000,
           curvature = 0.2, 
           arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
           color = "#F50021") +
  labs(caption = "Made by **@luisfreii** | Source **Kurzgesagt**") +
  theme_void() +
  theme(plot.margin = margin(25, 25, 20, 25),
        plot.caption.position = "plot",
        plot.caption = element_markdown(color = "#858585"),
        plot.background = element_rect(color = "#F5F5F5",
                                       fill = "#F5F5F5"),
        panel.background = element_rect(color = "#F5F5F5",
                                        fill = "#F5F5F5"))

#Code to save the plot    
# ggsave("3.Historical.svg",
#        width = 18,
#        height = 9,
#        dpi = 320)

# ggsave("3.Historical.png",
#        width = 18,
#        height = 9,
#        dpi = 320)