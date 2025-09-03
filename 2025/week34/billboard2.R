library(tidyverse)
library(showtext)

##Fonts 
font_add_google('Montserrat', 'mon')
font_add_google('Inter')
showtext_auto()
## text
subtitle <- 'The Billboard charts are a ruthless battlefield, sparing no one. Legends rise, empires fall, and even icons like Elvis Presley have struggled to seize the crown. \nYet among countless challengers, only three have truly claimed it: The Beatles, the Bee Gees, and Mariah Carey. '

billboard <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-26/billboard.csv')
names<- billboard |> 
  mutate(year = lubridate::year(date)) |> 
  group_by(artist) |>
  summarise(n = sum(weeks_at_number_one), .groups = "drop") |> 
  arrange(desc(n)) |> head(n = 30) |> 
  pull(artist)
#### 
billboard |> 
  mutate(year = lubridate::year(date)) |> 
  filter(artist %in% names) |> 
  arrange(date, .by_group = TRUE) |> 
  group_by(artist, year) |> 
  summarise(yearly_weeks = sum(weeks_at_number_one), .groups = "drop_last") |> 
  mutate(
    true_cum = cumsum(yearly_weeks),
    cumulative = lag(cumsum(yearly_weeks), default = 0), 
    cumulative = ifelse(row_number() == n(), true_cum, cumulative)) |> 
  ggplot(aes(x = year, 
             y = cumulative, 
             group = artist,
             colour = artist
             )) +
  geom_step(size = 1) +
  geom_point(size = 3) +
  gghighlight::gghighlight(
    artist %in% c('Mariah Carey', 'The Beatles', 'Adele', 
                  'Taylor Swift', 
                  'Bee Gees')) +
  annotate('text', x = 1967, y = 58, size = 5, 
           family = 'Inter', label = 'The Beatles') +
  annotate('text', x = 1981.25, y = 28, size = 5, 
           family = "Inter", label = 'Bee Gees') +
  annotate('text', x = 2014, y = 68, size =5, 
           family = "Inter", label = 'Mariah Carey: The Queen') +
  annotate('text', x = 2022, y = 38, size=5, 
           family = "Inter", label = 'Adele vs Taylor Swift') +
 
labs(x = '', 
     y = 'Total Weeks at Number One', 
     title = "Song of Ice and Fire: Battles for BillBoard # 1", 
     subtitle = subtitle,
     caption = "Data:  Billboard Hot 100 Number Ones Database | Vis: MhKirmizi"
       ) +
scale_x_continuous(breaks = seq(1970, 2025, 10), labels = seq(1970, 2025, 10)) +
scale_y_continuous(breaks = seq(20, 60, 20), labels = seq(20, 60, 20)) +
paletteer::scale_color_paletteer_d("LaCroixColoR::Pamplemousse") +
cowplot::theme_minimal_hgrid() +
  theme(
    text = element_text(size = 14, family = 'Inter'),
    legend.position = 'none', 
    plot.title = element_text(size = 36, hjust = .5, 
                              face = 'bold', family = 'mon', 
                              color = "#172869FF"), 
    plot.subtitle = element_text(size = 18, hjust = .5, family = 'Inter'),
    plot.background = element_rect(colour = "#FEFEFEFF", fill = "#FEFEFEFF"), 
    panel.background = element_rect(colour = "#FEFEFEFF", fill = "#FEFEFEFF")
  )
ggsave("billboard2.png", width = 1920, height = 1080, units = "px", dpi = 132)



