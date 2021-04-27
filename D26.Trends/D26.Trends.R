#Load packages
library(tidyverse)
library(extrafont)
library(modelr)
library(broom)
library(distributional)
library(ggdist)

#Get data. Link: https://www.kaggle.com/alinedebenath/global-shark-attacks
shark_attacks <- read.csv("GSAF5.csv") %>% 
  janitor::clean_names()

#Number of attacks per year and fatility
shark_attacks %>%
  group_by(year) %>% 
  count(fatal_y_n) %>% 
  filter(!fatal_y_n %in% c("", "UNKNOWN"),
         year >= 1990) -> shark_attacks

#Fit a simple linear model
model_shark_attacks <- lm(n ~ year * fatal_y_n, data = shark_attacks)

#Plot. Visit https://mjskay.github.io/ggdist/articles/freq-uncertainty-vis.html for tutorials on how to make this type of plots and some others.
shark_attacks %>%
  group_by(fatal_y_n) %>%
  data_grid(year = seq_range(year, n = 26)) %>%
  augment(model_shark_attacks, newdata = ., se_fit = TRUE) %>%
  ggplot(aes(year, fill = ordered(fatal_y_n), color = ordered(fatal_y_n))) +
  stat_dist_lineribbon(
    aes(dist = dist_student_t(df = df.residual(model_shark_attacks), 
                              mu = .fitted, 
                              sigma = .se.fit),
        fill_ramp = stat(level)),
    alpha = .6) +
  geom_point(aes(y = n), data = shark_attacks) +
  scale_fill_manual(values = c('#30B06A', '#FC6722')) +
  scale_color_manual(values = c('#30B06A', '#FC6722')) +
  scale_y_continuous(breaks = seq(0, 125, 25)) +
  scale_x_continuous(breaks = seq(1990, 2015, 5)) +
  labs(y = "Number of Attacks",
       x = NULL,
       title = "Global Shark Attacks Are Rising",
       subtitle = "<span style='color:#FC6722;'>**Fatal**</span> attacks have been staying relatively the same throughout the years.<br><span style='color:#30B06A;'>**Non-fatal**</span> shark attacks have been rising from 1990 to 2016.<br>Trendline shows, from outer to inner, the 95, 80 and 50% confidence intervals.",
       caption = "<br>Made by **@luisfreii** | Source: **Global Shark Attack File/Kaggle**") +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme(panel.background = element_rect('#FFFAFA'),
        plot.background = element_rect('#FFFAFA'),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title.position = 'plot',
        plot.caption.position = 'plot',
        plot.title = ggtext::element_markdown(family = 'Lato',
                                              size = 20,
                                              color = "#525252"),
        plot.subtitle = ggtext::element_markdown(family = 'Lato',
                                                 color = "#525252",
                                                 size = 10,
                                                 lineheight = .9),
        plot.caption = ggtext::element_markdown(family = 'Fira Sans',
                                                size = 8,
                                                color = "#525252"),
        axis.title = element_text(family = 'Fira Sans',
                                  color = "#525252"),
        axis.text = element_text(family = 'Fira Sans',
                                 color = "#525252"),
        plot.margin = margin(25, 15, 10, 15),
        axis.ticks.y = element_blank(),
        legend.position = 'none')

#Code to save the plot
# ggsave('D26.Trends.svg',
#        width = 18,
#        height = 15,
#        units = 'cm',
#        dpi = 320)

# ggsave('D26.Trends.png',
#        width = 18,
#        height = 14,
#        units = 'cm',
#        dpi = 320)