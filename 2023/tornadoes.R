library(tidyverse)
library(showtext)
## fonts
font_add_google("PT Sans")
showtext_auto()

## Data 
state_regions <- data.frame(
  state = state.name,  # Full state names
  st = state.abb,      # State abbreviations
  region = state.region # Corresponding region names
) 
tornadoes <- tidytuesdayR::tt_load(x = "2023-05-16")
tidytuesdayR::readme(tornadoes)
tornados <- tornadoes$tornados
remove(tornadoes)
### Data Wrangling
tornados |> 
  filter(yr %in% c(1950, 2022))|> 
  group_by(st, yr) |> 
  summarise(total = n(), .groups = "drop") |> 
  left_join(state_regions, by =c("st" = "st")) |> 
  ggplot(aes(yr, total, group = st)) +
  geom_line(alpha = .3, size =1 ,aes(colour = region)) +
  ggrepel::geom_text_repel(data = \(df) df |> filter(yr == 2022 & total >= 40), 
                           aes(label = state, color = region), 
                           size = 5, hjust = 0, 
                           nudge_x = 2, 
                           segment.color = 'transparent',
                           direction = "y", 
  ) + 
  geom_point(aes(color = region, size = total)) +
  labs(
    x= "", 
    y = "Number of Tornados", 
    fill = "Region of USA", 
    title = "Number of Tornados  from 1950 to 2022", 
    caption = "Data: National Weather Service| Vis: MhKirmizi", 
    color = "Region"
  ) +
  scale_color_manual(values = c("South" = "#447861FF" , 
                                "North Central" = "#B24422FF", 
                                "Northeast" = "#B1A1CCFF", 
                                "West" = "#C44D76FF")) +
  scale_x_continuous(breaks = c(1950, 2022), limits = c(1950, 2037)) +
  guides(size = "none") +
  theme_classic(base_size = 12, base_family = "PT Sans") +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 16, face = 'bold', colour = 'grey3'),
    plot.background = element_rect(color = "#faf9f5", fill = "#faf9f5"), 
    panel.background = element_rect(color = "#faf9f5", fill = "#faf9f5"),
    axis.text = element_text(size = 14, color = 'grey3'), 
    plot.title = element_text(size = 48, face = "bold", 
                              hjust = .5, color = "#13315FFF", 
                              family = "PT Sans"), 
    plot.caption = element_text(size = 12, colour = "grey3")
  )
ggsave("tornadoes.png", width = 1920, height = 1080, units = "px", dpi = 132)
