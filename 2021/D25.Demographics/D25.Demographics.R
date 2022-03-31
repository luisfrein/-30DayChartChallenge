#Load packages
library(tidyverse)
library(extrafont)
library(readxl)
library(scales)
library(glue)

#Read the data in a list. I need sheets 1 to 4. Link: https://population.un.org/wpp/Download/Standard/Population/
sheets <- lapply(1:4, function(i) read_excel("WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx", sheet = i, skip = 16))

#Bind all the elementes of the sheets list
population_prospects <- 
  plyr::rbind.fill(sheets) %>% 
  janitor::clean_names()

#Wrangle
population_prospects %>% 
  filter(str_detect(region_subregion_country_or_area, 'Venezuela')) %>%  #Filter for Venezuela
  select(-1, -(4:7)) %>% 
  pivot_longer(3:153 ,values_to = 'population', names_to = 'year') %>% #Wide to long format
  mutate(year = as.numeric(str_remove(year, 'x')),
         population = as.numeric(population)) %>% 
  pivot_wider(names_from = variant, values_from = population) %>% 
  janitor::clean_names() -> population_prospects_ven

#Tibble with arrow labels
tibble(
  year = c(1995.5, 2031),
  y = c(37300, 22900),
  label = c('30M residents\nin 2015', '28M residents\nin 2020')
) -> arrow_labels

#Df with projection labels
population_prospects_ven[151, ] %>% 
  select(-3) %>% 
  pivot_longer(3:5, names_to = 'variant', values_to = 'population') %>% 
  mutate(population_int = round(population / 1000, digits = 0)) -> population_prospects_label
    
population_prospects_label %>% 
mutate(label = case_when(population_int == 34 ~ glue('Mid: {population_int}M'),
                         population_int == 53 ~ glue('High: {population_int}M'),
                         population_int == 21 ~ glue('Low: {population_int}M'))) -> population_prospects_label

#Plot
  ggplot(population_prospects_ven) +
  geom_ribbon(aes(year, 
                  ymax = high_variant, 
                  ymin = low_variant), 
              fill = "#D6D6D6", 
              alpha = 0.7) +
  geom_line(aes(year, estimates),
            size = 1.1,
            color = '#595988') +
  geom_line(aes(year, medium_variant), 
            linetype = 'dashed',
            size = 1.1,
            color = '#595988') +
  geom_curve(aes(x = 2005,
                 xend = 2014,
                 y = 35000,
                 yend = 31000),
             arrow = arrow(length = unit(0.02, "npc")),
             curvature = -.1,
             color = '#3D3D3D') +
  geom_curve(aes(x = 2030,
                 xend = 2021,
                 y = 24900,
                 yend = 27900),
             arrow = arrow(length = unit(0.02, "npc")),
             curvature = .1,
             color = '#3D3D3D') +
  geom_text(population_prospects_label,
            mapping = aes(year + 1, population, label = label),
            hjust = 0,
            family = 'Fira Sans',
            size = 3,
            color = "#525252") +
  geom_text(arrow_labels,
            mapping = aes(year, y = y, label = label),
            family = 'Fira Sans',
            hjust = 0,
            size = 3,
            color = "#525252") +
  scale_x_continuous(breaks = seq(1950, 2100, 25),
                     limits = c(1950, 2118)) +
  scale_y_continuous(labels = label_number(scale = 1 / 1000, suffix = 'M')) +
  labs(title = "**Venezuela’s Population Going Forward**",
       subtitle = 'Population projections for Venezuela, from 2020 to 2100.',
       caption = '<br>Visualization: **@luisfreii** | Source: **United Nations**',
       y = 'Population', x = NULL) +
  coord_cartesian(expand = FALSE, 
                  clip = 'off') +
  theme(panel.background = element_rect('#FFFAFA'),
        plot.background = element_rect('#FFFAFA'),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title.position = 'plot',
        plot.caption.position = 'plot',
        plot.title = ggtext::element_markdown(family = 'IBM Plex Sans',
                                              size = 20,
                                              color = "#525252"),
        plot.subtitle = element_text(family = 'Fira Sans',
                                     color = "#525252"),
        plot.caption = ggtext::element_markdown(family = 'Fira Sans',
                                                size = 8,
                                                color = "#525252"),
        axis.title = element_text(family = 'Fira Sans',
                                  color = "#525252"),
        axis.text = element_text(family = 'Fira Sans',
                                 color = "#525252"),
        plot.margin = margin(25, 15, 10, 15),
        axis.ticks.y = element_blank())

#Code to save the plot
  # ggsave('D25.Demographics.svg',
  #        width = 20,
  #        height = 12.5,
  #        units = 'cm',
  #        dpi = 320)
   
  # ggsave('D25.Demographics.png',
  #        width = 20,
  #        height = 12.5,
  #        units = 'cm',
  #        dpi = 320)


