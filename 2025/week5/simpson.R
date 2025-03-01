library(tidyverse)
library(see)
library(tvthemes)
tuesdata <- tidytuesdayR::tt_load('2025-02-04')
  
## Text
title = "Donuts, Data, and D'oh - A Deep Dive into The Simpsons"
subtitle = "An analysis of the beloved TV series reveals intriguing patterns in IMDb ratings across seasons. While some episodes receive 
<br> ratings as low as 4.5 and others soar above 8, the true success lies in the consistent average ratings throughout the series. 
<br> This stability underscores the show's enduring appeal and quality over time."
caption = "Data:  The Simpsons by the Data | Vis: MhKirmizi"

## Data Wrangling
df <- tuesdata$simpsons_script_lines |> 
  left_join(tuesdata$simpsons_characters, by = c("character_id" = "id")) |> 
  filter(!is.na(gender)) |> 
  left_join(tuesdata$simpsons_episodes, by = c("episode_id" = "id")) |> 
  group_by(gender, season) |> 
  mutate(total = sum(word_count, na.rm = TRUE), 
         mean  =  mean(word_count, na.rm = TRUE))  
  
df2 <- df |> 
  group_by(season) |> 
  summarise(imdb_rating = mean(imdb_rating, na.rm = TRUE))
  
df |> 
  ggplot(aes(as.factor(season), imdb_rating, fill = as.factor(season))) +
  geom_violinhalf() +
  geom_point(data = df2, aes(x = as.factor(season), y = imdb_rating)) +
  coord_flip() +
  scale_fill_simpsons(type = "discrete") +
  labs(x = "Season", 
       y = "Imdb Rating", 
       title = title, 
       subtitle = subtitle, 
       caption= caption, 
       fill = "Season") +
  theme_simpsons() +
  theme(
    plot.title = element_text(size = 32, face = "bold", hjust = .5), 
    plot.subtitle = ggtext::element_markdown(size = 18, hjust = .5), 
    plot.caption = element_text(size =12)
  )
ggsave("simpson.png", dpi = 360, width = 16, height = 9)
  
