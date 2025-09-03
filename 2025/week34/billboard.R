library(tidyverse)
library(showtext)
font_add_google('Montserrat', 'mon')
font_add_google('Inter')
showtext_auto()


billboard <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-26/billboard.csv')
names<- billboard |> 
  mutate(year = lubridate::year(date)) |> 
  group_by(artist) |>
  summarise(n = sum(weeks_at_number_one), .groups = "drop") |> 
  arrange(desc(n)) |> head(n = 6) |> 
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
    cumulative = ifelse(row_number() == n(), true_cum, cumulative), 
    label = ifelse(row_number() == n(), artist, "")) |> 
  ggplot(aes(x = year, 
             y = cumulative, 
             group = artist,
             label = label,
             colour = artist
             )) +
  geom_step(size = 1) +
  geom_point(size = 3) +
  ggrepel::geom_text_repel(nudge_x = 4,
                           nudge_y = 1.5,
                           direction = 'both',
                           hjust = 'right',
                           max.overlaps = Inf, 
                           aes(size = 3)
                           ) +
labs(x = '', 
     y = 'Total Weeks at Number One', 
     title = "Decades of Dominance: A Look at Billboard's #1 Legends", 
     caption = "Data:  Billboard Hot 100 Number Ones Database | Vis: MhKirmizi"
       ) +
scale_x_continuous(breaks = seq(1970, 2025, 10), labels = seq(1970, 2025, 10)) +
paletteer::scale_color_paletteer_d("LaCroixColoR::CranRaspberry") +
cowplot::theme_cowplot() +
  theme(
    text = element_text(size = 14, family = 'Inter'),
    legend.position = 'none', 
    plot.title = element_text(size = 36, hjust = .5, 
                              face = 'bold', family = 'mon', 
                              color = "#2A2432FF"),
    plot.background = element_rect(colour = "#EFEFCFFF", fill = "#EFEFCFFF"), 
    panel.background = element_rect(colour = "#EFEFCFFF", fill = "#EFEFCFFF")
  )
ggsave("billboard.png", width = 1920, height = 1080, units = "px", dpi = 132)
