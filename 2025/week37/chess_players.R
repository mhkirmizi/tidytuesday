library(tidyverse)
library(showtext)
### Fonts
font_add_google('Montserrat', 'mont')
font_add_google("Noto Sans", "noto")
showtext_auto()

fide_ratings_september <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_september.csv')
### The grand masters

fide_ratings_september |> 
  filter(title %in% c('GM', 'WGM')) |> 
  mutate(title_fac = factor(title, levels = c('WGM', 'GM'))) |> 
  ggplot(aes(x = title_fac, y = rating, color = title_fac, fill = title_fac)) +
  ggrain::geom_rain(
    boxplot.args = list(
      color = 'black', 
      outlier.shape = NA
    )
  ) +
  coord_flip() +
  labs(x = '', 
       y = 'Rating', 
       title = "Men vs Women Grandmasters: Rating Patterns",
       caption = 'Data: FIDE | Vis: mhkirmizi') +
  scale_fill_manual(values = c("WGM" = "#94475EFF", "GM" = "#364C54FF")) +
  scale_color_manual(values = c("WGM" = "#94475EFF", "GM" = "#364C54FF")) +
  theme_minimal(base_size = 12, base_family = 'noto') +
  theme(
   legend.position = 'none', 
   plot.title = element_text(hjust = .5, 
                             size = 36, 
                             family = 'mont', 
                             face = 'bold', 
                             color = "#121510FF"), 
   plot.caption = element_text(size =12, 
                               family = 'noto'), 
   plot.background = element_rect(color = 'white', fill = 'white'), 
   panel.background = element_rect(color = 'white', fill = 'white')
  )
ggsave("chess_player.png", width = 1920, height = 1080, units = "px", dpi = 132)


