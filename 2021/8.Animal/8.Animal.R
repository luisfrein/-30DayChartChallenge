#Load packages
library(tidyverse)
library(extrafont)

#Get data. Link: https://www.kaggle.com/alinedebenath/global-shark-attacks
shark_attacks <- read.csv("GSAF5.csv") %>% 
  janitor::clean_names()

#Explore!
shark_attacks %>% 
  filter(!fatal_y_n %in% c("", "UNKNOWN"),
         year >= 1996) %>%
  group_by(year) %>% 
  count(fatal_y_n) %>% 
  ungroup() %>% 
  mutate(year = factor(year, levels = c(1996:2016))) -> shark_attacks

shark_attacks %>% 
  group_by(fatal_y_n) %>% 
  summarise(total_attacks = sum(n), 
            mean_attacks = total_attacks / 17) -> v_lines

shark_attacks %>% 
  group_by(year) %>% 
  summarise(diff = n - lead(n), total = sum(n)) %>% 
  filter(!is.na(diff)) %>% 
  mutate(text = paste0("Δ ", diff),
         position = total / 2) -> annotations

#Plot
  ggplot() +
  geom_line(shark_attacks, 
            mapping = aes(n, fct_rev(year),group = year), 
            size = 6.5, color = "darkgrey") +
  geom_point(shark_attacks,
             mapping = aes(n, fct_rev(year), color = fatal_y_n),
             size = 6.5,
             show.legend = FALSE) +
  geom_text(annotations, 
            mapping = aes(position, year, label = text),
            family = "Fira Sans",
            color = "#333333") +
  scale_color_manual(values = c("#1E5567", "#91274B")) +
  scale_x_continuous(breaks = seq(0, 120, 30)) +
    labs(y = NULL, x = "Number of Attacks",
         title = "Global Shark Attacks",
         subtitle = "<span style='color:#91274B;'>**Fatal**</span> and <span style='color:#1E5567;'>**non-fatal**</span> shark attacks from 1996 to 2016.",
         caption = "<br>Made by **@luisfreii** | Source: **Global Shark Attack File/Kaggle**") +
    theme(plot.margin = margin(25, 25, 10, 25),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          panel.background = element_rect("#F5F5F5"),
          plot.background = element_rect("#F5F5F5"),
          panel.grid = element_blank(),
          plot.title = element_text(size = 16,
                                    color = "#525252",
                                    face = "bold"),
          plot.subtitle = ggtext::element_markdown(color = "#525252",
                                                   family = "Fira Sans"),
          plot.caption = ggtext::element_markdown(color = "#525252",
                                                  family = "Fira Sans",
                                                  size = 7),
          axis.title = element_text(color = "#525252",
                                    size = 9),
          axis.text = element_text(family = "Fira Sans",
                                   color = "#525252",
                                   size = 9),
          axis.ticks = element_line(color = "#525252"))

#Code to save the plot
  # ggsave("8.Animal.png",
  #        width = 15,
  #        height = 20,
  #        units = "cm",
  #        dpi = 320)
  
  # ggsave("8.Animal.svg",
  #        width = 15,
  #        height = 20,
  #        units = "cm",
  #        dpi = 320)

  