#Load packages
library(tidyverse)
library(extrafont)
library(scales)
library(ggforce)

#Read the data
read_csv("Books_read - Hoja 1.csv") %>% 
  janitor::clean_names() -> books


#Plot
books %>% 
  filter(year_read == 2020) %>% 
  group_by(month_finished_read) %>% 
  summarise(avg_words = mean(length_in_words)) -> books_avg_words

books %>% 
  filter(year_read == 2020) %>% 
  group_by(month_finished_read) %>% 
  count(month_finished_read) -> books_count

#Left join both dfs
books_avg_words %>% 
  left_join(books_count) %>% 
  mutate(month_finished_read = str_c(month_finished_read, '-01-20'),
         month_finished_read = lubridate::mdy(month_finished_read)) -> books_4plot
   
#Tibble with annotations
tibble(
  month = as.Date(c('2020-06-01', '2020-07-01', '2020-09-01', '2020-12-01')),
  y = c(3, 3, 6, 4),
  labels = c('Started reading about\nStoicism', 'Read the Mistborn\nTrilogy', 'Started reading Percy Jackson', 'Decided to read Sci-fi')
) -> annotations


#Plot
books_4plot %>% 
  ggplot(aes(month_finished_read, n)) +
  geom_line(size = 1.25,
            color = "#211A1D") +
  geom_point(aes(size = avg_words),
             pch = 21,
             stroke = 1.5,
             fill = '#F5F5F5',
             color = "#211A1D") +
  geom_mark_circle(annotations,
                   mapping = aes(month, y, group = month, description = labels),
                   color = NA, 
                   label.fill = NA,
                   label.fontsize = c(0, 8),
                   label.family = c('Fira Sans', 'Fira Sans'),
                   con.colour = "#211A1D") +
  scale_size(range = c(4, 7),
             labels = label_comma()) +
  coord_cartesian(clip = 'off') +
  scale_x_date(labels = label_date_short(), 
               breaks = breaks_width('1 month')) +
  labs(x = NULL, y = 'Number of Books Read',
       size = 'Avg Word Count',
       title = 'My Reading Timeline (2020)',
       subtitle = 'Started reading in March after lockdown started.',
       caption = 'Visualization: **@luisfreii** | Made up Data') +
  guides(size = guide_legend(title.position = "top",
                               title.hjust = .5)) +
  theme(panel.background = element_rect('#F5F5F5'),
        plot.background = element_rect('#F5F5F5'),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'top',
        legend.background = element_blank(),
        plot.title.position = 'plot',
        plot.caption.position = 'plot',
        plot.title = element_text(family = 'IBM Plex Sans',
                                  size = 20,
                                  color = "#211A1D"),
        plot.subtitle = element_text(family = 'Fira Sans',
                                     color = "#525252"),
        plot.caption = ggtext::element_markdown(family = 'Fira Sans',
                                                size = 8,
                                                color = "#525252"),
        axis.title = element_text(family = 'Fira Sans',
                                  color = "#525252"),
        axis.text = element_text(family = 'Fira Sans',
                                 color = "#525252"),
        legend.text = element_text(size = 9,
                                   family = "Fira Sans",
                                   color = "#525252"),
        legend.title = element_text(size = 10,
                                    family = "Fira Sans",
                                    color = "#525252"),
        plot.margin = margin(25, 15, 10, 15))


#Code to save the plot
# ggsave('D24.Monochrome.svg',
#        width = 19,
#        height = 15,
#        units = 'cm',
#        dpi = 320)
# 
# ggsave('D24.Monochrome.png',
#        width = 19,
#        height = 15,
#        units = 'cm',
#        dpi = 320)