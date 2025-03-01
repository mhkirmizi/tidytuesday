library(tidyverse)
library(lubridate)
library(ggtext)
library(dlookr)
library(showtext)
### Fonts
font_add_google("Merriweather", "merriweather")
showtext_auto()

### |- plot aesthetics ----
colors <- c("Cat" = "#CD5733FF", "Dog" = "#979461FF", "Bird" = "#ACC2CFFF", "Rabbit" = "#ACC2CFFF", 
           "Reptile" = "#ACC2CFFF", "Wild" = "#ACC2CFFF", "Other" = "#ACC2CFFF")
## Text
title <- "Animal Rescue at Long Beach"
subtitle <- "The data from animal rescue at Long Beach reveals that **<span style='color:#CD5733FF'>Cats</span>**, those little troublemakers, are the ones who need rescue the most. <br>Our best friends, **<span style='color:#979461FF'>Dogs</span>**, are another group that often requires rescuing. Other animals, whether wild or pets, also get rescued from time to time."
caption <- "Data: Long Beach Animal Shelter | Vis: MhKirmizi"
## Data Wrangling
dlookr::diagnose(longbeach)
longbeach <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-04/longbeach.csv')
glimpse(longbeach)

animals <- longbeach |>
  filter(jurisdiction == "Long Beach") |> 
  mutate(year = year(intake_date), 
         animal_type = str_to_lower(animal_type)) |>
  count(animal_type, sort = TRUE) |> 
  slice_max(n , n=7) |> pull(animal_type)

df <- longbeach |> 
  filter(jurisdiction == "Long Beach" & animal_type %in% animals) |> 
  mutate(year = year(intake_date), 
         animal_type = str_to_lower(animal_type)) |> 
  group_by(year, animal_type) |> 
  count() |> 
  mutate(animal_type = str_to_sentence(animal_type))
label_data <- df |> 
  group_by(animal_type) |> 
  filter(year == max(year)) |> 
  mutate(
    animal_type = str_to_sentence(animal_type)
  )
  ggplot(df, aes(year, n, group = animal_type, color = animal_type)) +
  geom_line(size = 1.5) +
  geom_text(data = label_data, aes(label = animal_type),  
            hjust = -0.1, vjust = 0.5, size  = 5, family = "Merriweather") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.1))) +
  scale_color_manual(values = colors) +
    labs(x = "Year", 
         y = "", 
         title = title, 
         subtitle = subtitle, 
         caption = caption) +
  theme(
    text = element_text(size = 14, family = "Merriweather" , color = "black"), 
    plot.title = element_text(size = 36, 
                              family = "Merriweather", 
                              face = "bold", 
                              color = "#2a475e", hjust= .5), 
    plot.subtitle = element_markdown(size = 16, 
                                 family = "Merriweather",
                                 color = "#1b2838", hjust = .5),
    plot.title.position = "plot", 
    legend.position = "none", 
    axis.ticks = element_blank(), 
    axis.line = element_line(color = "grey50"), 
    panel.grid = element_line(color = "#b4aea9"), 
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4")
  )
ggsave("animal_rescue.pdf", width = 15, height = 9, dpi = 360)  
  






  