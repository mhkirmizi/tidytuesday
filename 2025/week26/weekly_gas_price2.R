library(tidyverse)
library(showtext)
### Fonts
font_add_google("Lato", "Lato")
showtext_auto()


### Data Wrangling

df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-01/weekly_gas_prices.csv')
df <- df |> 
  filter(grade == "regular", formulation == "all") 
df <-df |> 
  mutate(
    year = lubridate::year(date), 
    month = lubridate::month(date,label = TRUE, abbr = TRUE), 
    month_num = lubridate::month(date),
    week = lubridate::week(date)
  ) |> 
  filter(!year %in% c(1990,1991,2025))
unique(df$week)
df |> 
  group_by(week) |> 
  count() |> view()
df <- df |> 
  filter(week != 53)
### Plotting
ggplot(df, aes(x = week, y = year, fill = price)) +
  geom_tile(color= "white",size=0.1) +
  scale_fill_viridis_c(option = "F") +
  labs(x = "Weeks", 
       y = "Years from 1992 to 2024", 
       title = "Three Decades in Fuel: A Weekly Price Heatmap", 
       caption = "Data: EIA | Vis: MhKirmizi",
       fill = "Price") +
  scale_x_continuous(breaks = seq(1, 52, 4), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(1992, 2022, 10), expand = c(0, 0)) +
  theme_bw(base_size = 14, base_family = "Lato") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.background = element_blank(),  
    panel.border = element_blank(), 
    plot.title = element_text(size = 36, hjust = .5, face = "bold"), 
    plot.caption = element_text(size = 14)
  )
ggsave("weekly_gas_price2.png", width = 1920, height = 1080, units = "px", dpi = 132)
