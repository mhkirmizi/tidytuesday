library(tidyverse)
library(lemon)
library(scales)
library(showtext)
### Fonts
font_add_google('Roboto', 'robo')
font_add_google("Montserrat", 'mons')
showtext_auto()

### Data
cranes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-30/cranes.csv')
Subtitle <- 'The data from Lake Hornborgasjön, Sweden, provides valuable insights into crane populations, showing a consistent rise in observations since 1994.'
#### Data Wrangling
df<-cranes |> 
  mutate(year = lubridate::year(date), 
         month = lubridate::month(date), 
         week = lubridate::week(date)) |> 
  filter(!week %in% c(19:30)) |> 
  mutate(season = if_else(week %in%c(10:19), 'Spring', 'Fall'))
### Plotting
  ggplot(df, aes(week, observations)) +
  geom_point(aes(color = year), 
             position = position_jitter(width = .1)) +
  geom_smooth(data = df |> filter(week %in% c(11:17)), 
              color = 'black', se = FALSE, method = 'loess') +
  geom_smooth(data = df |> filter(week %in% c(31:42)), 
              color = 'black', se = FALSE,method = 'loess') +
  scale_color_viridis_c(option = 'F', direction = -1) +
  scale_x_continuous(breaks = c(14, 38), 
                     labels = c('Spring', 'Fall')) +
  scale_y_continuous(breaks = seq(10000, 20000, 10000), 
                     labels = label_number(scale = 1e-3, suffix = "K")) +
  labs(x = 'Season', 
       y = 'Number of the Observed Cranes', 
       title = 'Crane Observations at Lake Hornborgasjön: A Seasonal Shift', 
       subtitle = Subtitle,
       caption = 'Data = Carl Borstell | Vis: Mhkirmizi', 
       color = 'Year') +
  cowplot::theme_minimal_hgrid() +
  theme(
    text = element_text(family = 'robo', size = 12),
    legend.position = c(.95, .75), 
    plot.title = element_text(size = 36, hjust = .5, 
                              color = '#141824FF',
                              family = 'mons', face = 'bold'),
    plot.subtitle = element_text(size = 20, hjust = .5, color = '#181830FF'),
    plot.caption = element_text(size = 13), 
    plot.background = element_rect(color = 'white', fill = 'white'), 
    panel.background = element_rect(color = 'white', fill = 'white'),
    margin(10, 20, 10, 20)  # top, right, bottom, left
  )
  ggsave("crane.png", width = 1920, height = 1080, units = "px", dpi = 132)
  