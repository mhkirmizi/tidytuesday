library(tidyverse)
library(showtext)
### Fonts
font_add_google("Montserrat", 'mont')
font_add_google("Autour One", 'aut')
showtext_auto()
### Title & Subtitle
title <- "Forecasting Summer by a Snowman: The Böögg Hypothesis"
subtitle <- "Sechseläuten is an old Swiss-German tradition for predicting summer weather: a fast-exploding Böögg signals \na warm, sunny season, while a slow burn suggests a cold, rainy one. Interestingly, the data appears to support the claim"

### Data
df <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-02/sechselaeuten.csv')
### Data Wrangling
df |> 
  filter(year >= 1970) |> 
  ggplot() +
  geom_pointrange(
    aes(x = year,  
        y =tre200m0, 
        ymax = tre200mx, 
        ymin = tre200mn, 
        colour = duration, 
        shape = record)) +
  #geom_hline(yintercept = 19, linetype = 'dashed', color = 'grey36') +
  scale_y_continuous(limits = c(0,35), breaks = seq(10, 35, 10),
                     labels = seq(10, 35, 10)) +
  annotate("text", x = 1978, y = 4, 
           label = "Min Summer Temp: \n6.10°C" , 
           vjust = 1, color= "grey66", 
           family = 'aut') +
  geom_curve(
    aes(x = 1976, y = 2.8, xend = 1970.5, yend = 6),
    arrow = arrow(length = unit(0.2, "in")),
    curvature = -0.3,
    color = "gray66"
  ) +
  annotate("text", x = 1993, y = 35, 
           label = "Max Summer Temp: \n34.57°C" , 
           vjust = 1, color= "grey66", 
           family = 'aut') +
  geom_curve(
    aes(x = 1997, y = 34.5, xend = 2002.5, yend = 34.57),
    arrow = arrow(length = unit(0.2, "in")),
    curvature = -0.3,
    color = "gray66"
  ) +
  labs(x = '', 
       y = 'Avg Temp (C)', 
       title = title, 
       subtitle = subtitle, 
       color = 'Duration', 
       shape = 'Hot Summer',
       caption = 'TidyTuesday Week: 48 | Vis: MhKirmizi') +
  paletteer::scale_color_paletteer_c("harrypotter::lunalovegood") +
  cowplot::theme_minimal_hgrid() +
  theme(
    legend.position = c(.8, .15),
    legend.direction = 'horizontal',
    text = element_text(size =14, family = 'aut'), 
    plot.title = element_text(size = 36, hjust = .5, family = 'mont', face = 'bold'), 
    plot.subtitle = element_text(size = 17, hjust = .5, family = 'aut'),
    plot.background = element_rect(fill = "white", colour = "white"), 
    panel.background = element_rect(fill = "white", colour = "white")
  )

ggsave("week48.png", width = 1920, height = 1080, units = "px", dpi = 132)  


