#Load packages
library(tidyverse)
library(extrafont)
library(ggforce)
library(scales)

#Create tibble with data. Numbers come from: https://solarsystem.nasa.gov/ and https://www.nasa.gov/sites/default/files/files/YOSS_Act1.pdf
tibble(
  planet = c("Mercury", 'Venus', 'Earth', 'Mars', 'Jupiter', 'Saturn', 'Uranus', 'Neptune'),
  length_of_year_in_days = c(88, 225, 365, 687, 4333, 10759, 30687, 60190),
  distance_to_sun_million_km = c(57, 108, 149, 228, 780, 1437, 2871, 4530),
  size_relative_to_earth = c(.38, .91, 1, .53, 11, 9.1, 4, 3.9),
  size_for_desc = c(1 - .38, 1 - .91, 1, 1 - .53, 11, 9.1, 4, 3.9)
  ) -> planets

planets %>% 
mutate(big_small = case_when(size_relative_to_earth < 1 ~ 'smaller',
                             size_relative_to_earth > 1 ~ 'bigger',
                             TRUE ~ 'equal'))  -> planets

days_for_desc <- format(planets$length_of_year_in_days, big.mark = ',') %>% 
  str_trim()

#Tibble with description
description <- glue::glue("{planets$size_for_desc} times {planets$big_small}\nthan Earth. A\nyear takes {days_for_desc}\nearth days.")

description[3] <- 'A year takes 365\ndays.'

#Plot
planets %>% 
ggplot(aes(length_of_year_in_days, distance_to_sun_million_km, size = size_relative_to_earth)) +
  geom_point() +
  scale_size_identity() +
  geom_mark_circle(aes(label = planet, group = planet, description = description),
                   size = .75,
                   color = "#4A7E82",
                   label.fill = "#D6D6D6",
                   con.colour = "#4A7E82",
                   label.family = 'IBM Plex Sans') +
  scale_x_log10(label = label_comma()) +
  scale_y_log10(label = label_comma()) +
  labs(x = '\nLength of Year In Earth Days (log 10)',
       y = 'Distance To The Sun in Million of kms (log 10)\n',
       title = 'A Year In Our Solar System',
       subtitle = 'How many days does a year last in the planets of our solar system, and their distance to the sun.',
       caption = 'Graphic: **@luisfreii** | Source: **NASA**') +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(color = "#474747",
                                  family = 'Nasalization Rg'),
        axis.text = element_text(color = "#474747",
                                 family = 'IBM Plex Sans'),
        plot.title = element_text(color = "#474747",
                                  family = 'Nasalization Rg',
                                  size = 20),
        plot.subtitle = element_text(color = "#474747",
                                     family = 'IBM Plex Sans'),
        plot.caption = ggtext::element_markdown(color = "#474747",
                                                family = 'Nasalization Rg'),
        plot.title.position = 'plot',
        plot.caption.position = 'plot',
        plot.margin = margin(25, 25, 10, 25),
        plot.background = element_rect('#F5F5F5'),
        panel.background = element_rect('#F5F5F5'),
        axis.ticks = element_blank())

#Code to save the plot
# ggsave('14.Space.png',
#        width = 40,
#        height = 20,
#        units = 'cm',
#        dpi = 320)

# ggsave('14.Space.svg',
#        width = 40,
#        height = 20,
#        units = 'cm',
#        dpi = 320)
