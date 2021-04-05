#Load packages
library(tidyverse)
library(extrafont)

#Create tibble. Numbers come from pdf in IOM, PDF in the folder.
tibble(
  migrants = c(695551, 4769498),
  year = c(2015, 2019),
  total_pop = 28515829,
  percentage = migrants * 100 / total_pop
) -> migrants


#Create tibble with labels
tibble(
  position = c(2014.9, 2019.1),
  value = migrants$migrants,
  text = c(glue::glue("{format(migrants[[1, 1]], big.mark = ',')}\n{round(migrants[[1, 4]], 1)}% of the\npopulation"),
           glue::glue("{format(migrants[[2, 1]], big.mark = ',')}\n{round(migrants[[2, 4]], 1)}% of the\npopulation"))
)  -> labels



#Plot
ggplot(migrants) +
  geom_line(aes(year, migrants), 
            color = "#CB2C2A", 
            size = 1.25) + 
  geom_text(labels, mapping = aes(position, value, label = text, hjust = c(1, 0)),
            family = "Arial Narrow",
            color = "#CB2C2A",
            fontface = "bold",
            size = 3) +
  geom_text(label = "2015", x = 2014.9, 
            y = 1.1 * (max(migrants$migrants)), 
            hjust = 1, 
            size = 4, 
            family = "Arial Narrow",
            color = "#3B3B3F") + 
  geom_text(label = "2019", x = 2019.1, 
            y = 1.1 * (max(migrants$migrants)), 
            hjust = 0, 
            size = 4, 
            family = "Arial Narrow",
            color = "#3B3B3F") +
  geom_vline(xintercept = 2015, 
             linetype = "dashed", 
             size = .1, 
             color = "#3B3B3F") + 
  geom_vline(xintercept = 2019, 
             linetype = "dashed", 
             size = .1, 
             color = "#3B3B3F") +
  xlim(2014, 2020) +
  ylim(600000, 4850000) +
  coord_cartesian(clip = "off") +
  labs(title = "The Venezuelan Diaspora",
       subtitle = "Number of Venezuelan migrants.",
       caption = "\nMade by @luisfreii | Source: International Organization for Migration (IOM)") +
  theme_void() +
  theme(plot.subtitle = element_text(margin = margin(t = .2, b = 1, unit = "cm"),
                                     family = "Arial Narrow",
                                     size = 11,
                                     color = "#3B3B3F"),
        plot.margin = margin(25, 25, 20, 25),
        plot.title.position = "plot",
        plot.caption = element_text(face = "bold", 
                                    family = "Arial Narrow", 
                                    size = 5, 
                                    hjust = 0.5,
                                    color = "#8B8B92"),
        plot.title = element_text(family = "Arial Narrow",
                                  size = 14,
                                  face = "bold",
                                  color = "#3B3B3F"),
        panel.background = element_rect(color = "#F5F5F5",
                                        fill = "#F5F5F5"),
        plot.background = element_rect(color = "#F5F5F5",
                                       fill = "#F5F5F5"))
  
#Code to save the plot
# ggsave("5.Slope.png",
#        width = 10,
#        height = 15,
#        units = "cm",
#        dpi = 320)

# ggsave("5.Slope.svg",
#        width = 10,
#        height = 15,
#        units = "cm",
#        dpi = 320)
