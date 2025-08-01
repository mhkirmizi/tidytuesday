library(tidyverse)
library(dlookr)
library(lubridate)
library(packcircles)
library(ggforce)
library(showtext)

## Fonts
font_add_google("Montserrat", 'mon')
font_add_google('Roboto', 'roboto')
showtext_auto()
## Fonts

##Text
title <- "From CoComelon to Bridgerton: Who’s Bingeing What?"
subtitle <- "The top 15 Netflix shows of 2024. Bigger circles mean more views; darker \nshades mean more hours watched. Kids’ shows get more clicks, adults watch longer."
## Data wrangling
shows <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-29/shows.csv')
diagnose(shows)
shows |> head()
df <- shows |> 
  mutate(hours = as.numeric(str_extract(runtime, "\\d+(?=H)")), 
         minutes = as.numeric(str_extract(runtime, "\\d+(?=M)")), 
         total_minutes = 60*coalesce(hours, 0) + coalesce(minutes, 0)) 

df <-df |>  
  mutate(release_year = as.integer(str_extract(report, "^\\d{4}")), 
         report = str_extract(report, "[A-Za-z]+-[A-Za-z]+"))
df_2024 <- df |> 
  rename(report_year = release_year) |> 
  filter(report_year == 2024)
diagnose(df_2024)         
df_2024 <- df_2024 |> 
  slice(-c(1:4))

df_2024 <-df_2024 |> 
  mutate(title_en = str_trim(str_split_fixed(title, "//", 2)[, 1]),
         title_clean = str_remove(title_en, ":.*$") |> str_trim(), 
         season = str_extract(title_en, "(?<=: ).*$"))

df_2024_top_15 <- df_2024 |> 
  group_by(title_clean) |> 
  summarise(sum_views = sum(views), 
            sum_watch = sum(hours_viewed)) |> 
  slice_max(order_by =sum_views, n= 15)

data_circle <- circleProgressiveLayout(df_2024_top_15$sum_views, 
                                       sizetype = "area")
df2 <- cbind(df_2024_top_15, data_circle)

ggplot() +
  geom_circle(
    data = df2, 
    aes(
      x0= x, y0 = y, 
      fill = sum_watch,
      r= radius), 
    alpha = .6, color= "black"
  ) +
  scale_fill_gradient(
    low = "#F26C6C",  # lighter Netflix red (tint)
    high = "#E50914"  # official Netflix red
  )+
  geom_text(
    data = df2, 
    aes(x = x, y = y, 
      label = title_clean 
      ), 
    colour = 'white',
    fontface = 'bold',
    size = 4,
    hjust = .5
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_fixed(ratio = 1) +
  labs(
    title = title, ,
    subtitle = subtitle, 
    caption = 'Data: Netflix Engagement Report | Vis: MhKirmizi'
  ) +
  theme_void(base_size = 12, base_family = 'roboto') +
  theme(
    plot.background = element_rect(fill = 'black', color = 'black'), 
    panel.background = element_rect(fill = 'black', color = 'black'), 
    legend.position = 'none', 
    plot.title = element_text(size = 28, hjust = 0.5, 
                              family = 'mon',
                              face = 'bold', 
                              color = 'white'), 
    plot.subtitle = element_text(size = 16, hjust = 0.5, 
                                 colour = 'white',
                                 face = 'bold', ), 
    plot.caption = element_text(color = 'white', size = 12, hjust =.5, face = 'bold'), 
    plot.margin = margin(r= 80, l=80)
  )
ggsave("netflix.png", width = 1920, height = 1080, units = "px", dpi = 132)
