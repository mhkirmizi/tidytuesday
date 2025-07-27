library(tidyverse)
library(showtext)
library(ggtext)

pokemon_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv')

## Text
title <- "Gotta Catch 'Em All!"
subtitle <- "The Pokémon data reveals an interesting trend where most Pokémon types have higher <br>attack stats  than defense. However, while some types like Water, Grass, and Bug <br>display a more balanced distribution."
## Fonts
font_add_google("Press Start 2P")
font_add("pokemon_solid", "D:/VIS/pokemon/Pokemon Solid.ttf")
showtext_auto()
##Plotting
pokemon_df |> 
  mutate(
    type_1 = str_to_sentence(type_1)
  ) |> 
  ggplot(aes(defense, attack)) +
  geom_point() +
  gghighlight::gghighlight(attack > defense) +
  facet_wrap(~type_1, ncol = 6) +
  labs(x = "Defense", 
       y = "Attack", 
       title = title, 
       subtitle = subtitle,
       caption = "Data: {pokemon} | Vis: MhKirmizi")+
  cowplot::theme_minimal_grid() +
    theme(
    text = element_text(size = 14, family =  "Press Start 2P"),
    plot.title = element_text(size = 40, face = "bold", 
                              hjust = .5, 
                              family = "pokemon_solid", 
                              colour = "#DB4743FF" ), 
    plot.subtitle = element_markdown(size = 16, hjust = .5, halign = .5),
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    plot.background = element_rect(fill = "#FEF4D5FF"), 
    panel.background = element_rect(fill = "#FEF4D5FF")

  )
ggsave("pokemon.png", width = 1920, height = 1080, units = "px", dpi = 132)
