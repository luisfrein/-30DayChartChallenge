#Load packages
library(tidyverse)
library(extrafont)

#Create tibble
tibble(
  x = c(1:4),
  "\nTimes I Searched\n‘Magic’ in Google" = c(0, 0, 0, 40) 
) %>% 
  ggplot(aes(x, `\nTimes I Searched\n‘Magic’ in Google`)) +
  geom_area(fill = "#F18805") +
  annotate("text", x = 1.7, y = 20, label = "#30DayChartChallenge\nDay 4: Magical\nhappened",
           hjust = 0,
           color = "#291600",
           family = "IBM Plex Sans") +
  annotate("curve", 
           x = 3.5,
           xend = 3.05,
           y = 20,
           yend = 23,
           curvature = 0.2, 
           arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
           color = "#291600") +
  labs(title = "My interest in Magic",
       caption = "Made by **@luisfreii** | Made up Data",
       x = NULL) +
  scale_x_continuous(labels = c("Jan\n2021", "Feb", "Mar", "Apr")) +
  coord_cartesian(clip = "off", expand = FALSE) +
  theme(plot.margin = margin(25, 25, 20, 25),
        axis.title.y = element_text(angle = 0, hjust = 0,
                                    family = "IBM Plex Sans"),
        plot.title = element_text(size = 18, margin = margin(b = .75, unit = "cm"),
                                  color = "#291600",
                                  family = "IBM Plex Sans"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.background = element_rect("#F5F5F5"),
        plot.background = element_rect("#F5F5F5"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.caption = ggtext::element_markdown(color = "#291600",
                                                family = "IBM Plex Sans"),
        axis.text = element_text(family = "IBM Plex Sans"))

#Code to save the plot. I always save it to svg, and then export to png. I get better antialiasing that way.
# ggsave("4.Magical.svg",
#        width = 15,
#        height = 11,
#        units = "cm",
#        dpi = 320)

# ggsave("4.Magical.png",
#        width = 15,
#        height = 11,
#        units = "cm",
#        dpi = 320)
