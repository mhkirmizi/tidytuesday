library(tidyverse)
library(dlookr)
library(waffle)
library(showtext)
### Fonts
font_add_google(name = "Montserrat", family = "montserrat")
font_add_google(name = "Fira Sans", family = "fira")
showtext_auto()
## Data Wrangling
cases_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_year.csv')
df_waffle <- cases_year |> 
  group_by(region, year) |> 
  summarise(total = sum(measles_total, na.rm = TRUE), .groups = "drop")
df_waffle <- df_waffle |> 
  mutate(scaled_total = as.integer(round(total / 10000)))
df_waffle <- df_waffle |> 
  mutate(region_label = recode(region,
                               "AFRO" = "Africa",
                               "AMRO" = "Americas",
                               "EMRO" = "Eastern Med.",
                               "EURO" = "Europe",
                               "SEARO" = "SE Asia",
                               "WPRO" = "W. Pacific"
  ))
ggplot(df_waffle, aes(values = scaled_total, fill = region_label)) +
  geom_waffle(n_rows = 3, size = .25, color= "white", flip = TRUE) +
  facet_wrap(~year, nrow = 1,strip.position = "bottom") +
  coord_equal() +
  guides(fill = guide_legend(nrow = 1))+
  scale_x_discrete() +
  scale_fill_brewer(palette = "Dark2") +
  labs(y = "", 
       x = "", 
       fill = "Region", 
       title = "How Measles Spread Over the Years: A Global Snapshot", 
       subtitle = "The waffle chart below shows the global total of measles cases, with each square representing 10,000 cases.", 
       caption = "Data: WHO | Vis: MhKirmizi") +
  theme_minimal(base_size = 16, base_family = "fira") +
  theme(
    plot.title = element_text(size = 48, hjust = .5, family = "montserrat", face = "bold"), 
    plot.subtitle = element_text(size = 24, hjust = .5), 
    plot.background = element_rect(color = "white", fill = "white"), 
    panel.background = element_rect(color = "white", fill = "white"),
    legend.position = "top", 
    axis.ticks.x = element_blank(), 
    axis.text = element_blank()
  )
ggsave("measles2.png", width = 1920, height = 1080, units = "px", dpi = 132)

