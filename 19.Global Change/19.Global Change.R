#Load packages
library(tidyverse)
library(extrafont)
library(ggtext)
library(gganimate)
library(scales)

#Get the data
internet_usage_percentage_raw <- read_csv("Individuals using the Internet (% of population).csv")

#Set text encoding
Encoding(internet_usage_percentage_raw$`Country Name`) <- "latin1"

internet_usage_percentage_raw %>% 
  select(-(3:34), -(63:65)) %>% 
  pivot_longer(3:30, names_to = "year", values_to = "usage_percentage") %>%   mutate(`Country Name` = fct_recode(`Country Name`, "América Latina y\nel Caribe" = "América Latina y el Caribe"),
                                                                                     year = as.numeric(year)) -> internet_usage_percentage

#Plot
internet_usage_percentage %>% 
  filter(`Country Name` %in% c('Ingreso alto', 'Ingreso mediano', 'Países de ingreso bajo')) %>% 
  ggplot(aes(year, usage_percentage, color = `Country Name`, group = `Country Name`)) +
  geom_line(size = 1.25) +
  scale_x_continuous(breaks = pretty_breaks(5)) +
  scale_y_continuous(breaks = pretty_breaks(5), labels = label_number(suffix = "%")) +
  scale_color_manual(values = c('#2D7DD2', '#858585', '#9C3848')) +
  labs(y = "% of Individuals\n", x = NULL, 
       title = "**Global Internet Access by Income 1990-2017**",
       subtitle = "Percentage of individuals using the internet in <span style='color:#2D7DD2;'>**high-income countries**</span>,<br><span style='color:#858585;'>**middle-income countries**</span>, and <span style='color:#9C3848;'>**low-income countries**</span>.",
       caption = "<br>Graph: **@luisfreii** | Source: **World Bank**") +
  theme_minimal() +
  theme(plot.title = element_markdown(family = 'IBM Plex Sans',
                                      size = 16,
                                      color = "#525252"),
        plot.subtitle = element_markdown(family = 'Fira Sans',
                                         color = "#525252"),
        plot.caption = element_markdown(family = 'Fira Sans',
                                        size = 8,
                                        color = "#525252"),
        plot.title.position = 'plot',
        plot.caption.position = 'plot',
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = 'none',
        plot.background = element_rect('#F5F5F5',
                                       color = '#F5F5F5'),
        panel.background = element_rect('#F5F5F5',
                                        color = '#F5F5F5'),
        axis.title = element_text(color = "#525252",
                                  family = "Fira Sans"),
        axis.text = element_text(color = "#525252",
                                 family = "Fira Sans"),
        plot.margin = margin(25, 15, 10, 15)) -> p1

#Add animation and tweaks
a1 <- p1 + transition_reveal(year) + shadow_mark()
 
 anim <- animate(a1, 
                 nframes = 26, end_pause = 10,
                 fps = 10, 
                 height = 15, width = 20, units = "cm", res = 150)
 anim
 
#Code to save the animation
 anim_save("D19.Global Change.gif", animation = last_animation())
