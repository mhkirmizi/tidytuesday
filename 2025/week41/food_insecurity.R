library(tidyverse)
library(showtext)
## Fonts
font_add_google("Playfair Display", "playfair")
font_add_google("Lato", "lato")
showtext_auto()
### Text
subtitle <- 'Food insecurity remains stable in Europe, the Americas, and Oceania, but rises sharply in Africa and Asia.'
### Data Wrangling
df <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-14/food_security.csv')
glimpse(df)
df_food <- df |> 
  filter(Item == 'Number of severely food insecure people (million) (3-year average)')  
df_food_total  <- df_food |> slice(205:n())
df_food_total |> 
  select(Year_End, Area, Value) |> 
  filter(Area %in% c("Africa", "Northern America", 
                     "Europe", "South America", "Asia", "Oceania")) |> 
  ggplot(aes(Year_End, Value, group = Area))+
  geom_line(aes(color = Area, label = Area), size = 1, alpha = .8) +
  geom_point(aes(colour = Area, fill = Area), size = 3, alpha = .8) +
  labs(x = '', 
       y = 'Food Insecure Poeple (million)', 
       title = 'Trends in Global Food Insecurity Over Time', 
       caption = 'Data: Suite of Food Insecurities | Vis: MhKirmizi', 
       subtitle = subtitle,
       color = '',
       fill = '') +
  cowplot::theme_minimal_hgrid() +
  theme(
    text = element_text(size = 14, family = 'lato'),
    legend.position = 'top', 
    legend.justification = 'center',
    plot.title = element_text(size = 36, hjust = .5, family = 'playfair', face= 'bold'), 
    plot.subtitle = element_text(size = 24, hjust =.5, face = 'italic'), 
    plot.caption = element_text(size = 12),
    plot.background = element_rect(fill = 'white', color = 'white'), 
    panel.background = element_rect(fill = 'white', color = 'white')
  ) +
  guides(color = guide_legend(nrow= 1))
ggsave("food_insecurity.png", width = 1920, height = 1080, units = "px", dpi = 132)


