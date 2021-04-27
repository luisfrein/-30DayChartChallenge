#Load packages
library(tidyverse)
library(extrafont)
library(ggdist)

#Get the data. Link: https://www.kaggle.com/spscientist/students-performance-in-exams
students_perfomance <- read_csv('StudentsPerformance.csv') %>% 
  janitor::clean_names()

#Plot
students_perfomance %>%
  mutate(parental_level_of_education = factor(parental_level_of_education, levels = c('some high school', 'high school', 'some college', "associate's degree", "bachelor's degree", "master's degree"))) %>% 
  ggplot(aes(parental_level_of_education, math_score)) +
  stat_slab(side = "left", 
            scale = 0.5,
            fill = '#808A9F') +
  stat_dotsinterval(scale = 0.6,
                    fill = '#808A9F',
                    color = '#2B303B') +
  labs(title = "Parental Levels of Education and Students Scores",
       y = 'Students Math Scores',
       x = '\nParental Level of Education',
       caption = 'Visualization: **@luisfreii** | Source: **Kaggle**') +
  theme(panel.background = element_rect('#FFFAFA'),
        plot.background = element_rect('#FFFAFA'),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title.position = 'plot',
        plot.caption.position = 'plot',
        plot.title = ggtext::element_markdown(family = 'Lato',
                                              size = 20,
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
# ggsave('D27.Educational.svg',
#        width = 25,
#        height = 15,
#        units = 'cm',
#        dpi = 320)

# ggsave('D27.Educational.png',
#        width = 25,
#        height = 15,
#        units = 'cm',
#        dpi = 320)
