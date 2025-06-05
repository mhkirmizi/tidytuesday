library(tidyverse)
library(showtext)
library(ggrepel)
## Fonts & Texts
font_add_google(name = 'Outfit', family = 'Outfit')
showtext_auto()
subtitle <- subtitle <- "Pixar Animation, a pioneering animation studio, released its first-ever CGI-animated film, Toy Story, in 1996, which became a massive success. \nSince then, the studio has released a new film almost every year, setting industry standards for CGI animation. \nHowever, the studio's recent movies have had a hard time surpassing the high benchmark they previously established."
## Colors
bg_color <- colorspace::darken("#435774", 0.2)
text_color <- "#fff"

films <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-11/pixar_films.csv"
)
public <- read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-11/public_response.csv"
)
df<- films |> 
  left_join(public, by = c('film' = 'film')) 
df <- df |> 
  mutate(year = year(release_date))
df <- df |> 
  mutate(metacritic = ifelse(film == "Luca", 71, metacritic))
df <- df |> 
  mutate(metacritic = ifelse(film == "Turning Red", 83, metacritic)) |> 
  mutate(metacritic = ifelse(film == "Lightyear", 61, metacritic))
df <- df |> mutate(film= ifelse(is.na(film), "Elemental", film)) 
df <- df |> 
  mutate(metacritic = ifelse(film == "Elemental", 58, metacritic))
inside_out2 <- data.frame(
  number = 28, 
  film = "Inside Out 2", 
  release_date = as.Date("2024-06-14"), 
  run_time = NA, 
  film_rating = NA, 
  rotten_tomatoes = NA, 
  metacritic = 73, 
  cinema_score = NA, 
  critics_choice = NA, 
  year = 2024
)
df <- df |> 
  select(number: critics_choice)
df$year <- lubridate::year(df$release_date)
df <- rbind(df, inside_out2)
### Plotting
ggplot(df, aes(year, metacritic)) + 
  geom_smooth(linetype = 'dashed', se= FALSE, colour = 'white') +
  geom_label_repel(aes(label = film), 
                   fill = "#D8ACD8", 
                   color = "white", 
                   fontface = "bold", 
                   box.padding = .28, 
                   point.padding = .3) +
  labs(x = '', 
       y = 'Metacritic', 
       title = 'Pixar Movies', 
       subtitle = subtitle,
       caption = "Data: {pixarfilms} | Vis: MhKirmizi") +
  scale_x_continuous(breaks  = seq(1996, 2025, 4), labels = seq(1996, 2025, 4) ) +
  scale_y_continuous(breaks = seq(0, 100, 25), labels = seq(0, 100, 25), 
                     limits = c(0,100))+ 
  theme_classic(base_family = 'Outfit', base_size = 14) +
  theme(
    plot.background = element_rect(colour = bg_color, fill = bg_color), 
    panel.background = element_rect(colour = bg_color, fill = bg_color), 
    plot.title = element_text(size = 48, colour = 'white', face = 'bold', hjust = .5),
    plot.subtitle  = element_text(size = 20, colour = 'white', face = 'bold', hjust = .5),
    plot.caption = element_text(size = 12, colour = 'white', face = 'bold'),
    axis.title = element_text(size = 14, colour = 'white', face = 'bold'), 
    axis.text = element_text(size = 12, colour = 'white', face = 'bold'), 
    axis.line = element_line(colour = 'white'), 
    axis.ticks = element_line(colour = 'white')
  )
ggsave("pixar_px.png", width = 1920, height = 1080, units = "px", dpi = 132)


