#Load packages
library(tidyverse)
library(extrafont)
library(ggridges)
library(cowplot)

#Tibble with colors
tibble(
    colors = c("#163FA7", "#85bb65", "#F50021", "#F18805", "#CB2C2A", "#0091AD", "#7c7998", "#91274B", "#FA7921", "#1E5567"),
  saturation = c(77, 39, 100, 96, 66, 100, 13, 58, 96, 55),
  light = c(37, 56, 48, 48, 48, 34, 54, 36, 55, 26)
) -> colores

#Plot abstract part
abstract <- 
  ggplot() +
  geom_polygon(aes(x = c(1, 1 , 3, 3),
                   y = c(2.525, 2.475, 2.475, 2.525)),
               fill = colores[[4, 1]]) +
  geom_polygon(aes(x = c(1, 1 , 3, 3),
                   y = c(-0.525, -0.475, -0.475, -0.525)),
               fill = colores[[4, 1]]) +
  geom_polygon(aes(x = c(2, 2, 2.2, 2.2),
                   y = c(3, -1, -1, 3)),
               fill = colores[[2, 1]],
               size = 1.25) +
  geom_polygon(aes(x = c(1.8, 1.8, 2, 2),
                   y = c(3, -1, -1, 3)),
               fill = colores[[9, 1]],
               size = 1.25) +
  geom_polygon(aes(x = c(1, 1.5, 2.5, 1),
                   y = c(2, 2, 0, 0)),
               fill = colores[[8, 1]],
               size = .5) +
  geom_polygon(aes(x = c(3, 3, 1.5, 2.5),
                   y = c(0, 2, 2, 0)),
               fill = colores[[10, 1]]) +
  geom_polygon(aes(x = c(1.5, 3, 3, 2.5),
                   y = c(5, 5, 3, 3)),
               fill = colores[[1, 1]]) +
  geom_polygon(aes(x = c(2.5, 1, 1, 1.5),
                   y = c(-3, -3, -1, -1)),
               fill = colores[[3, 1]]) +
  geom_polygon(aes(x = c(1, 1.5, 2.5, 1),
                   y = c(5, 5, 3, 3)),
               fill = colores[[5, 1]]) +
  geom_polygon(aes(x = c(3, 2.5, 1.5, 3),
                   y = c(-3, -3, -1, -1)),
               fill = colores[[6, 1]]) +
  coord_cartesian(expand = FALSE) +
  theme_void() +
  theme(panel.background = element_rect(colores[[7, 1]]),
        panel.grid = element_blank()) 

#Plot distributions
colores %>% 
  pivot_longer(2:3, names_to = "type", values_to = "value") %>% 
  ggplot(aes(value, type)) +
  geom_density_ridges(fill = colores[[7, 1]], 
                      alpha = .75,
                      size = 0.25,
                      rel_min_height = .0125,
                      scale = .95) +
  scale_y_discrete(labels = c("Lightness", "Saturation")) +
  scale_x_continuous(breaks = seq(0, 100, 25)) +
  labs(x = "Value", y = NULL) +
  coord_cartesian(expand = FALSE) +
  theme(panel.background = element_rect("#F5F5F5"),
        plot.background = element_rect("#F5F5F5"),
        panel.grid = element_blank(),
        axis.text = element_text(family = "Roboto"),
        axis.title = element_text(family = "Roboto")) -> distribution
  
#Create subtitles for plot
abstract_subtitle <- paste(
  "Abstract Art made with the colors I used for",
  "the first 9 days of the #30DayChartChallenge.",
  "Along with its lightness and saturation distribution.",
  sep = '\n'
)

#Add title and subtitles for abstract plot
p2 <- ggdraw(abstract) + 
  draw_text("The Colors of Viz", x = 0.025, y = 0.86, hjust = 0, color = 'white',
            size = 28, family = 'Tw Cen MT') + 
  draw_text(abstract_subtitle, x = 0.025, y = 0.795, hjust = 0, color = 'white',
            size = 11, family = 'Roboto', lineheight = 0.9) + 
  draw_line(x = c(0.025, 0.49), y = 0.835, color = 'white', size = 0.5) 

#Combine abstract and distribution
plot_grid(p2, distribution, ncol = 1, rel_heights = c(4, 1))

#Code to save the plot
# ggsave("10.Abstract.png",
#        height = 24.5,
#        width = 14.5,
#        unit = "cm",
#        dpi = 320,
#        type = "cairo-png")

# ggsave("10.Abstract.svg",
#        height = 24.5,
#        width = 14.5,
#        unit = "cm",
#        dpi = 320)
