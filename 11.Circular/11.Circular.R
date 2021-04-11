#Load packages
library(tidyverse)
library(extrafont)

#Get the data
readxl::read_excel("oil_production.xlsx") %>% 
  janitor::clean_names() %>% 
  select(1, 4) %>% 
  slice(265:336) -> oil_production

#tibble with text
tibble(
  month = c(1, 12),
  year = 2015,
  text = c("Jan - 15", "Dec- 15")
) -> annotations

#tibble with text
tibble(
  month = 1,
  year = 2016:2020,
  text = 2016:2020
) -> annotations2 #This is a super bad name for this
  
oil_production %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) -> oil_production

ggplot(oil_production, aes(date, crude_oil_production_venezuela_monthly, fill = crude_oil_production_venezuela_monthly)) +
  geom_col() +
  ylim(-500, 2600) +
  coord_polar() +
  scale_fill_gradient(low = "#faae7b", 
                      high = "#432371",
                      limits = c(380, 2600),
                      breaks = seq(500, 2500, 500)) +
  labs(fill = "Monthly Crude Oil Production") +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = .5, 
                               barwidth = unit(20, 'lines'), 
                               barheight = unit(.5, 'lines'))) +
  theme(legend.position = "top")

  ggplot() +
  geom_tile(oil_production,
            mapping = aes(month, year,
                          fill = crude_oil_production_venezuela_monthly),
            color = "white") +
  #xlim(-11, 13) + 
  ylim(2011, 2021) +
  geom_text(annotations,
            mapping = aes(month, year, label = text),
            angle = c(-23, -345),
            color = "white",
            size = 2.5,
            family = "Fira Sans") +
  geom_text(annotations2,
            mapping = aes(month, year, label = text),
            angle = -20,
            color = "white",
            size = 2.5,
            family = "Fira Sans") +
  coord_polar() +
  scale_fill_gradient(low = "#ffba08", 
                      high = "#9d0208",
                      limits = c(380, 2600),
                      breaks = seq(500, 2500, 500)) +
  labs(fill = "Monthly Crude Oil Production",
       title = "Venezuela's Oil Production",
       subtitle = "Venezuela's main exports are oil. Here is the\ncountry's crude oil production from 2015 to 2020.",
       caption = "Made by **@luisfreii** | Source: **U.S. Energy Information Administration (EIA)**") +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = .5, 
                               barwidth = unit(15, 'lines'), 
                               barheight = unit(.5, 'lines'))) +
  theme_void() +
  theme(legend.position = "top",
        plot.margin = margin(25, 15, 10, 15),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.background = element_rect(color = "#F5F5F5",
                                        fill = "#F5F5F5"),
        plot.background = element_rect(color = "#F5F5F5",
                                       fill = "#F5F5F5"),
        panel.grid = element_blank(),
        plot.title = element_text(size = 16,
                                  color = "#525252",
                                  face = "bold",
                                  hjust = .5,
                                  family = "Fira Sans"),
        plot.subtitle = element_text(size = 12,
                                     color = "#525252",
                                     hjust = .5,
                                     margin = margin(t = .2, b = .5, unit = "cm"),
                                     family = "Fira Sans"),
        plot.caption = ggtext::element_markdown(size = 7,
                                                color = "#525252",
                                                hjust = .5,
                                                family = "Fira Sans"),
        legend.text = element_text(size = 9,
                                   family = "Fira Sans"),
        legend.title = element_text(size = 10,
                                    family = "Fira Sans"))

#Code to save the plot. The PNG save method creates some white space, you can remove that with the magick package using image_trim. I always save it first to SVG and then PNG so I remove the white space on InkScape
  # ggsave("11.Circular.png",
  #      width = 12,
  #      height = 18,
  #      units = "cm",
  #      dpi = 320,
  #      type = "cairo-png")
  
  # ggsave("11.Circular.svg",
  #        width = 12,
  #        height = 18,
  #        units = "cm",
  #        dpi = 320)
  

