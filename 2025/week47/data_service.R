library(tidyverse)
library(gganimate)
library(showtext)
### Fonts
font_add_google('Roboto', 'robo')
font_add_google('Montserrat', 'mono')
showtext_auto()
## Data
spi_indicators <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-25/spi_indicators.csv')

### gganimate 
g <- spi_indicators |> 
  filter(income != 'Not classified') |> 
  filter(year >= 2018) |> 
  ggplot(aes(data_products_score, overall_score, 
             size = population, color = country)) +
  geom_point(alpha = .75, show.legend = FALSE) +
  scale_size(range = c(2, 12)) +
  facet_wrap(~income) +
  labs(title = 'Development of Data Products', 
       x = 'Data Products Score', y = 'Overall Data Score') +
  theme_minimal(base_size = 12, base_family = 'robo') +
  theme(plot.title = element_text(size = 18, hjust = .5, family = 'mono')) +
  transition_time(year) 
## Saving
anim <- animate(g, renderer = gifski_renderer(), width = 800, height = 600, fps = 10)
anim_save("rank_animation.gif", animation = anim)
