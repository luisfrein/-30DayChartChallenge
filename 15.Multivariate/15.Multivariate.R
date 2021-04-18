#Load packages
library(tidyverse)
library(extrafont)
library(ggforce)
library(scales)
library(ggrepel)
library(ggtext)

#Get the data
readxl::read_xlsx('HDI_2019.xlsx') %>% 
  janitor::clean_names() -> hdi

#Set HDI categories
hdi %>% 
  mutate(mean_years_of_schooling = as.numeric(mean_years_of_schooling),
         hdi_category = case_when(hdi_2019 >= .8 ~ 'Very High',
                                  hdi_2019 >= .7 & hdi_2019 < .8 ~ 'High',
                                  hdi_2019 <= .5 ~ 'Low',
                                  TRUE ~ 'Medium')) %>% 
  filter(!is.na(mean_years_of_schooling)) -> hdi

#Plot
  hdi %>% 
    ggplot(aes(mean_years_of_schooling, gni_per_capita, color = hdi_2019, label = country)) +
    geom_smooth(se = FALSE, 
                method = 'lm', 
                color = "#525252") +
    geom_point(aes(size = life_expectancy_at_birth),
               alpha = .7) +
    scale_color_gradient(low = "#282F3E",
                         high = "#22D37D") +
    scale_y_log10(label = label_comma(), breaks = breaks_log()) +
    scale_x_continuous(breaks = c(3, 6, 9, 12)) +
    geom_text_repel(max.overlaps = 2) +
    labs(y = "GNI per capita (Log 10)",
         x = "Mean Years of Schooling", 
         title = "Human Development Index And Its Components",
         subtitle = "Countries go from <span style='color:#282F3E;'>**low**</span> HDI to <span style='color:#22D37D;'>**very high**</span> HDI.<br>Dot size is proportional to the country's life expectancy at birth.",
         caption = '<br>Graph: **@luisfreii**<br>Source: **UNITED NATIONS DEVELOPMENT PROGRAMME**') +
    theme(plot.margin = margin(25, 15, 10, 15),
          panel.background = element_rect("#F5F5F5"),
          plot.background = element_rect("#F5F5F5"),
          panel.grid.major.y  = element_line(color = "#EBEBEB"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 20,
                                    color = "#525252",
                                    face = "bold",
                                    family = "Fira Sans"),
          plot.subtitle = element_markdown(size = 12,
                                           color = "#525252",
                                           margin = margin(t = .2, b = .5, unit = "cm"),
                                           family = "Fira Sans"),
          plot.caption = element_markdown(size = 8,
                                          color = "#525252",
                                          family = "Fira Sans",
                                          hjust = 0),
          axis.title = element_text(color = "#525252",
                                    family = "Fira Sans"),
          axis.text = element_text(color = "#525252",
                                   family = "Fira Sans"),
          axis.ticks.y = element_blank(),
          legend.position = 'none',
          plot.title.position = "plot",
          plot.caption.position = "plot")

  
  #Code to save the plot
  # ggsave('15.Multivariate.svg',
  #        width = 26,
  #        height = 16,
  #        units = 'cm',
  #        dpi = 320)
  # 
  # ggsave('15.Multivariate.png',
  #        width = 26,
  #        height = 16,
  #        units = 'cm',
  #        dpi = 320)
