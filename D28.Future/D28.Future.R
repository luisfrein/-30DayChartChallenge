#Load packages
library(tidyverse)
library(extrafont)
library(readxl)
library(scales)
library(glue)

#Get the data
sheets <- lapply(1:4, function(i) read_excel('WPP2019_FERT_F01_BIRTHS_BOTH_SEXES.xlsx', sheet = i, skip = 16))

#Bind all the elementes of the sheets list
births_prospects <- 
  plyr::rbind.fill(sheets) %>% 
  janitor::clean_names()

#Wrangle
births_prospects %>% 
  filter(str_detect(region_subregion_country_or_area, 'Venezuela')) %>%  #Filter for Venezuela
  select(-1, -(4:7)) %>% 
  pivot_longer(3:32 ,values_to = 'births', names_to = 'year') %>% #Wide to long format
  mutate(year = str_remove(year, 'x'),
         year = as.numeric(str_sub(year, start = -4)),
         births = as.numeric(births)) %>% 
  pivot_wider(names_from = variant, values_from = births) %>% 
  janitor::clean_names() -> births_prospects_ven

births_prospects_ven[14, 4:6] <- births_prospects_ven[14, 3]

#Df with projection labels
births_prospects_ven[30, ] %>% 
  select(-3) %>% 
  pivot_longer(3:5, names_to = 'variant', values_to = 'births') %>% 
  mutate(population_int = round(births / 1000, digits = 1)) -> births_prospects_label

births_prospects_label %>% 
  mutate(label = case_when(population_int == 1.6 ~ glue('Mid: {population_int}M'),
                           population_int == 3.6 ~ glue('High: {population_int}M'),
                           population_int == .6 ~ glue('Low: {population_int}M'))) -> births_prospects_label

#Tibble with arrow labels
tibble(
  year = c(1987, 2000),
  y = c(3300, 2100),
  label = c('2.9M births at the\nend of 2015', '2.6M births at the\nend of 2020')
) -> arrow_labels

#Plot
ggplot(births_prospects_ven) +
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
  geom_text(births_prospects_label,
            mapping = aes(year + 1, births, label = label),
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
  geom_curve(aes(x = 2005,
                 xend = 2014,
                 y = 3250,
                 yend = 3000),
             arrow = arrow(length = unit(0.02, "npc")),
             curvature = -.1,
             color = '#525252') +
  geom_curve(aes(x = 2010,
                 xend = 2019,
                 y = 2300,
                 yend = 2570),
             arrow = arrow(length = unit(0.02, "npc")),
             curvature = -.1,
             color = '#525252') +
  scale_x_continuous(breaks = seq(1950, 2100, 25),
                     limits = c(1955, 2118)) +
  scale_y_continuous(labels = label_number(scale = 1 / 1000, suffix = 'M')) +
  labs(title = "Venezuela’s Births Going Forward",
       subtitle = 'Projected births for Venezuela, from 2020 to 2100.',
       caption = '<br>Visualization: **@luisfreii** | Source: **United Nations**',
       y = 'Number of Births', x = NULL) +
  coord_cartesian(expand = FALSE, 
                  clip = 'off') +
  theme(panel.background = element_rect('#FFFAFA'),
        plot.background = element_rect('#FFFAFA'),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title.position = 'plot',
        plot.caption.position = 'plot',
        plot.title = element_text(family = 'IBM Plex Sans',
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
# ggsave('D28.Future.svg',
#        width = 20,
#        height = 12.5,
#        units = 'cm',
#        dpi = 320)

# ggsave('D28.Future.png',
#        width = 20,
#        height = 12.5,
#        units = 'cm',
#        dpi = 320)