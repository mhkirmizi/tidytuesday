library(tidyverse)
library(gganimate)
library(showtext)
### Fonts
font_add_google("Montserrat", "Montserrat")
font_add_google('Outfit', 'Outfit')
showtext_auto()
### Colors
hue<- c("#FBA93A", "#cc2d36", "#93BFE5", "#D8ACD8", "#f0813c","#5D77AA","#2AA3A6")
### Data
country_lists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-09/country_lists.csv')
rank_by_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-09/rank_by_year.csv')

### Data Wrangling & Plotting
q <- rank_by_year |> 
  filter(visa_free_count !=0 ) |> 
  mutate(region = str_to_sentence(region)) |> 
  ggplot(aes(region, visa_free_count, label = country)) +
  geom_point(position = position_jitter(width = .1), 
             aes(colour = region, size = 1/rank)) +
  lemon::coord_flex_cart(bottom= lemon::brackets_horizontal()) +
  scale_color_manual(values = hue)+
  labs(x = '', 
       y = '', 
       title = "The Change in Rank of Passport between (2006-2025)")+
  cowplot::theme_minimal_hgrid() +
  theme(
    text = element_text(size = 12, family = "Montserrat"),
    legend.position = 'none', 
    plot.title = element_text(size = 18, 
                              hjust = .5, 
                              face = 'bold', 
                              family = "Outfit"), 
    panel.grid.major.y = element_line(linetype = 'dashed'),
    plot.background = element_rect(color = 'white' , fill = 'white'), 
    panel.background = element_rect(color = "white", fill = "white")
  )+
  transition_time(year)

### Animating
anim <- animate(q, renderer = gifski_renderer(), width = 800, height = 600, fps = 10)
anim_save("rank_animation.gif", animation = anim)
  
  





