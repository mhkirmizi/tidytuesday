library(tidyverse)
library(showtext)

###Fonts
font_add_google("Montserrat", "Montserrat")
font_add_google('Outfit', 'Outfit')
showtext_auto()

## Data Wrangling
df <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-12-09/qatarcars.csv')

### Visulization
ggplot(df, aes(performance, horsepower)) +
  geom_point(aes(color = enginetype, shape = type)) +
  labs(x = '0 to 100km/h', 
       y = NULL, 
       title = 'Acceleration vs. Horsepower Across Car Models in Qatar') + 
  guides(color = guide_legend(title = 'Engine'),
         shape = guide_legend(title = 'Body')) +
  scale_x_continuous(labels = function(x) paste0(x, ' s')) +
  scale_y_continuous(labels = function(y) paste0(y , ' hp')) +
  scale_color_brewer(palette = 'Dark2') +
  cowplot::theme_cowplot() +
  theme(
    text = element_text(size = 14, family = 'Outfit'), 
    legend.position = c(.9, .6),
    plot.title = element_text(size = 32, 
                              family = "Montserrat", 
                              face = 'bold', hjust = 0.5), 
    plot.background = element_rect(fill = 'white', color = 'white'), 
    panel.background = element_rect(fill = 'white', color = 'white')
    
  ) 
ggsave('week49.png', width = 1920, height = 1080, units = "px", dpi = 132)
