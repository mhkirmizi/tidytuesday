library(tidyverse)
library(showtext)

pokemon_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv')
## Colors
paletteer::paletteer_d("nationalparkcolors::Badlands")
c("#5495CFFF", "#F5AF4DFF", "#DB4743FF", "#7C873EFF", "#FEF4D5FF")
## Text
title <- "Gotta Catch 'Em All!"
subtitle <- "The Pokémon data reveals an interesting trend where most Pokémon types have higher attack stats than defense. \nHowever, while some types like Water, Grass, and Bug display a more balanced distribution, the majority are heavily attack-oriented."
## Fonts
font_add_google("Press Start 2P")
font_add("pokemon_solid", "D:/VIS/pokemon/Pokemon Solid.ttf")
showtext_auto()
##Vis
pokemon_df |> 
  mutate(
    type_1 = str_to_sentence(type_1)
  ) |> 
  ggplot(aes(defense, attack)) +
  geom_point() +
  gghighlight::gghighlight(attack > defense) +
  facet_wrap(~type_1) +
  labs(x = "Defense", 
       y = "Attack", 
       title = title, 
       subtitle = subtitle,
       caption = "Data: {pokemon} | Vis: MhKirmizi")+
  theme_bw(base_family = "Press Start 2P", base_size = 14) +
    theme(
    plot.title = element_text(size = 40, face = "bold", 
                              hjust = .5, 
                              family = "pokemon_solid", 
                              colour = "#DB4743FF" ), 
    plot.subtitle = element_text(size = 20, hjust = .5),
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    plot.background = element_rect(fill = "#FEF4D5FF"), 
    panel.background = element_rect(fill = "#FEF4D5FF")

  )
ggsave("pokemon.pdf", dpi = 360, height = 9, width = 12)
