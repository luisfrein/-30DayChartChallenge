#Load packages
library(tidyverse)
library(showtext)

#Get font
font_add_google('Fira Sans', family = 'fira')

## Automatically use showtext to render text for future devices
showtext_auto()
showtext_opts(dpi = 300)

#Numbers come from Encovi 2021 report, available here: https://www.proyectoencovi.com/
tibble(
  poverty_type = c('extreme_poverty','poverty', 'no_poverty'),
  percentage = c(76.6, 17.9, 5.5)
) -> poverty

tibble(
  poverty_type = c('Extremely Poor (76.6%)','Poor (17.9%)', 'Non\nPoor\n(5.5%)'),
  position = c(76.6 / 2, (17.9 / 2) + 76.6, (5.5/2) + 94.5)
) -> poverty_label

#Plot
poverty %>% 
  ggplot() +
  geom_col(aes(1, percentage, fill = fct_reorder(poverty_type, percentage))) +
  geom_text(poverty_label,
            mapping = aes(1, position, label = poverty_type),
            family = 'fira',
            size = 4,
            color = '#FFFAFA') +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_fill_manual(values = c('#707070', '#C04A35', '#803123')) +
  coord_flip(expand = FALSE,
             clip = 'off') +
  labs(y = 'Percentage of Population',
       x = NULL,
       title = 'Share of Venezuelans Living in Poverty in 2021',
       subtitle = "According to a poll done by the National Survey of Living Conditions (known in Spanish as ENCOVI),<br>around 77 Venezuelans were living in <span style='color:#803123;'>**extreme poverty**</span>, 18 in <span style='color:#C04A35;'>**poverty**</span>, and just about 5 were <span style='color:#707070;'>**not poo**r</span>.",
       caption = 'Source: **ENCOVI** | Visualization: **@luisfreii**') +
  theme(panel.background = element_rect('#FFFAFA'),
        plot.background = element_rect('#FFFAFA'),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(family = 'fira',
                                    color = "#525252"),
        axis.text.x = element_text(family = 'fira',
                                   color = "#525252"),
        axis.ticks.x = element_line(color = "#525252"),
        legend.position = 'none',
        plot.title = element_text(family = 'fira',
                                  color = "#525252",
                                  face = 'bold'),
        plot.subtitle = ggtext::element_markdown(family = 'fira',
                                                 color = "#525252"),
        plot.caption = ggtext::element_markdown(family = 'fira',
                                                color = "#525252"),
        plot.margin = margin(25, 15, 10, 15))

#Code to save the plot
# ggsave('D1_2022.png',
#        width = 25,
#        height = 15,
#        dpi = 320,
#        units = 'cm')