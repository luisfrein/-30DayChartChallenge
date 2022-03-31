#Load packages
library(waffle)
library(tidyverse)
library(extrafont)

#Get data
readxl::read_xlsx("indicators_12_02_2020.xlsx", sheet = "Reserv. Int Año", skip = 2,col_types = c("numeric", "numeric", "skip", "skip", "skip", "skip")) %>%
  janitor::clean_names() -> reservas

#English Plot
reservas %>% 
  mutate(bcv = round(bcv / 1000),
         fecha = as_factor(fecha)) %>% 
  filter(fecha %in% c(2000, 2005, 2010, 2015, 2020)) %>% 
  count(fecha, wt = bcv) %>% 
  ggplot(aes(label = fecha, values = n)) +
  geom_pictogram(
    n_rows = 3, size = 6, colour = "#85bb65", flip = TRUE,
    family = "Font Awesome 5 Free Solid") +
  scale_label_pictogram(
    name = NULL,
    values = c("money-bill"),
    labels = c("Reservas")) +
  facet_wrap(~fecha, nrow = 1, strip.position = "bottom") +
  coord_equal() +
  scale_y_continuous(labels = function(x) x * 3, 
                     expand = c(0,0),
                     breaks = scales::breaks_pretty(n = 5)) +
  labs(title = "Venezuela International Reserves (USD)",
       subtitle = "<span style='font-family: \"Font Awesome 5 Free Solid\";color:#85bb65;'>&#xf0d6;</span> = 1 billion<br>",
       caption = "<br>Made by **@luisfreii** | Source: **Central Bank of Venezuela**") +
  theme(plot.title = element_text(family = "Lato",
                                  size = 20,
                                  color = "#1E2619"),
        plot.subtitle = ggtext::element_markdown(family = "Lato",
                                                 size = 14,
                                                 color = "#1E2619"),
        plot.caption = ggtext::element_markdown(family = "Lato",
                                                color = "#1E2619"),
        strip.text = element_text(family = "Lato",
                                  size = 12,
                                  color = "#1E2619"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.margin = margin(25, 25, 10, 25),
        legend.position = "none",
        plot.background = element_rect("#DBDBDB"),
        panel.background = element_rect("#DBDBDB"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed"),
        axis.text.y = element_text(family = "Lato",
                                   size = 12,
                                   color = "#1E2619"),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

#Spanish Plot
reservas %>% 
  mutate(bcv = round(bcv / 1000),
         fecha = as_factor(fecha)) %>% 
  filter(fecha %in% c(2000, 2005, 2010, 2015, 2020)) %>% 
  count(fecha, wt = bcv) %>% 
  ggplot(aes(label = fecha, values = n)) +
  geom_pictogram(
    n_rows = 3, size = 6, colour = "#85bb65", flip = TRUE,
    family = "Font Awesome 5 Free Solid") +
  scale_label_pictogram(
    name = NULL,
    values = c("money-bill"),
    labels = c("Reservas")) +
  facet_wrap(~fecha, nrow = 1, strip.position = "bottom") +
  coord_equal() +
  scale_y_continuous(labels = function(x) x * 3, 
                     expand = c(0,0),
                     breaks = scales::breaks_pretty(n = 5)) +
  labs(title = "Reservas Internacionales Venezolanas (Dólar Estadounidense)",
       subtitle = "<span style='font-family: \"Font Awesome 5 Free Solid\";color:#85bb65;'>&#xf0d6;</span> = 1 millardo<br>",
       caption = "<br>Creado por **@luisfreii** | Fuente: **Banco Central de Venezuela**") +
  theme(plot.title = element_text(family = "Lato",
                                  size = 20,
                                  color = "#1E2619"),
        plot.subtitle = ggtext::element_markdown(family = "Lato",
                                                 size = 14,
                                                 color = "#1E2619"),
        plot.caption = ggtext::element_markdown(family = "Lato",
                                                color = "#1E2619"),
        strip.text = element_text(family = "Lato",
                                  size = 12,
                                  color = "#1E2619"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.margin = margin(25, 25, 10, 25),
        legend.position = "none",
        plot.background = element_rect("#DBDBDB"),
        panel.background = element_rect("#DBDBDB"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed"),
        axis.text.y = element_text(family = "Lato",
                                   size = 12,
                                   color = "#1E2619"),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

#Code to save the plot, svg or png
# ggsave("2Pictogram.png",
#        width = 20,
#        height = 15,
#        units = "cm",
#        dpi = 320)

# ggsave("2Pictogram.svg",
#        width = 23,
#        height = 15,
#        units = "cm",
#        dpi = 320)

