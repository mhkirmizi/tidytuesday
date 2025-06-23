library(tidyverse)
library(dlookr)
library(ggbump)
library(showtext)
### Fonts
font_add_google("Fira Sans", "fira")
showtext_auto()
api_categories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_categories.csv')
api_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_info.csv')
api_logos <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_logos.csv')
api_origins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_origins.csv')
apisguru_apis <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/apisguru_apis.csv')
df <- apisguru_apis |> 
  left_join(api_logos, by = "name") |> 
  left_join(api_info, by = "name") |> 
  left_join(api_categories, by = "name") 
remove(api_origins)
diagnose(df) |> print(n= 22)

df<- df |> 
  mutate(apisguru_category = as.factor(apisguru_category), 
         provider_name = as.factor(provider_name)
         )
top10 <- df |> 
  group_by(apisguru_category) |> 
  count() |> 
  ungroup() |> 
  arrange(-n) |> 
  drop_na() |> 
  top_n(n = 10) |> 
  pull(apisguru_category)
df <- df |> 
  mutate(year = lubridate::year(added))
top_4_provider <- df |> 
  group_by(provider_name) |> 
  count() |> 
  arrange(-n) |> head(n= 3) |> 
   pull(provider_name) |> 
   as.character()
plot_df <- df |> 
  filter(provider_name %in% top_4_provider) |> 
  group_by(provider_name, year) |> 
  count()


ggplot(plot_df, aes(year, n, color = provider_name)) +
  geom_bump(size = 2) +
  geom_point(size = 5) +
  scale_color_brewer(palette = "Accent") +
  labs(x = "", 
       y = "", 
       title = "Leading API Providers Over Time", 
       caption = "Data: APIs.guru | Vis: MhKirmizi", 
       color = "Provider Name") +
  theme_minimal(base_size = 12, base_family = "fira") +
  theme(
    plot.title = element_text(size = 36, hjust = .5, face = "bold"),
    plot.background = element_rect(fill = "white", color = "white"), 
    panel.background = element_rect(fill = "white", color = "white"),
    legend.position = "top"
  )
ggsave("ApiR.png", width = 1920, height = 1080, units = "px", dpi = 132)
