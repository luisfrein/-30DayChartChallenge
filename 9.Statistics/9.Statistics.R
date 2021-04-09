#Load packages
library(tidyverse)
library(extrafont)
library(ggridges)

#Get data. Link: https://www.kaggle.com/unitednations/international-financial-statistics. Data file is too big, and I can't upload it on GitHub
financial_statistics <- read_csv("international_financial_statistics_data.csv")

#Filter for bond yield category
financial_statistics %>% 
  filter(category == "bond_yields") -> bond_yields

#Plot
bond_yields %>% 
  filter(value <= 20, year >= 1990) %>% 
  ggplot(aes(value, fct_rev(as.character(year)))) +
  geom_density_ridges(alpha = .7, 
                      scale = 1.5, 
                      fill = "#FA7921",
                      color = "#1F1F1F") +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Yield", y = NULL,
       title = "International Financial Statistics",
       subtitle = "Distribution of Bond Yields (%) for 69 countries. 1990 - 2009.",
       caption = "Made by @luisfreii | Source: United Nations/Kaggle") +
  theme(plot.margin = margin(25, 25, 10, 25),
        plot.title = element_text(size = 18,
                                  color = "#FFECD6",
                                  family = "Lato"),
        axis.title = element_text(color = "#FFECD6",
                                  family = "Lato"),
        plot.subtitle = element_text(color = "#FFECD6",
                                     family = "Lato"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.background = element_rect("#1F1F1F"),
        plot.background = element_rect("#1F1F1F"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.caption = ggtext::element_markdown(color = "#FFECD6",
                                                family = "Lato"),
        axis.text = element_text(color = "#FFECD6",
                                 family = "Lato"))

#Code to save the plot
# ggsave("yields.png",
#        width = 21,
#        height = 29.7,
#        units = "cm")

# ggsave("yields.svg",
#        width = 21,
#        height = 29.7,
#        units = "cm")





