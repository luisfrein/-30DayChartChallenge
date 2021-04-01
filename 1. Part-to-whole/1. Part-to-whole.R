#Load packages
library(tidyverse)
library(extrafont)

read.csv("Books_read - Hoja 1.csv") %>% 
  janitor::clean_names() -> books

#Plot!
books %>% 
  count(book_language) %>% 
  mutate(color = c("#141924", "#163FA7")) %>% 
  ggplot(aes(1, n, fill = color, 
             label = paste0(book_language, "\n", n, " books"))) +
  geom_col(position = "stack") +
  geom_segment(x = .55,
               xend = 1.45,
               y = 0,
               yend = 0,
               color = "#D6DCEC",
               size = 1.5) +
  geom_segment(x = .55,
               xend = 1.45,
               y = 13,
               yend = 13,
               color = "#D6DCEC",
               size = 1.5) +
  geom_text(aes(1, n - 7),
            color = "#D6DCEC",
            fontface = "bold",
            family = "Fira Sans Medium",
            size = 5) +
  coord_polar(theta = "y") +
  scale_fill_identity() +
  labs(title = "In Which Language Do I Prefer Reading?",
       caption = "Made by **@luisfreii** | My Book Data") +
  theme_void() +
  theme(panel.background = element_rect(color = "#D6DCEC",
                                        fill = "#D6DCEC"),
        plot.background = element_rect(color = "#D6DCEC",
                                       fill = "#D6DCEC"),
        plot.title = element_text(hjust = .5,
                                  family = "Fira Sans Medium",
                                  color = "#141924",
                                  size = 18),
        plot.caption = ggtext::element_markdown(hjust = .5,
                                                family = "Fira Sans Medium",
                                                color = "#4D65A8",
                                                size = 10),
        plot.margin = margin(25, 25, 10, 25))

#Code to save the plot svg or png
ggsave("part-to-whole.svg",
       width = 15,
       height = 15,
       units = "cm",
       dpi = 320)

ggsave("part-to-whole.png",
       width = 15,
       height = 15,
       units = "cm",
       dpi = 320)