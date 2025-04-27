library(tidyverse)
library(scales)
library(ggstatsplot)
library(dlookr)
daily_accidents_420 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-22/daily_accidents_420.csv')
diagnose(daily_accidents_420)
daily_accidents_420 <- daily_accidents_420 |> 
  drop_na() 
daily_accidents_420 |> 
  ggstatsplot::ggbetweenstats(x = e420, 
                              y = fatalities_count)
df <- daily_accidents_420 |> 
  mutate(month = month(date), 
         year = year(date)) |> 
  group_by(year) |> 
  summarise(total = sum(fatalities_count))
df_month<- daily_accidents_420 |> 
  mutate(month = month(date)) |> 
  group_by(month) |> 
  summarise(total = sum(fatalities_count))

daily_accidents_420 |> 
  ggplot(aes(e420, fatalities_count, fill = e420)) +
  ggrain::geom_rain() +
  labs(title= )
df<- daily_accidents_420 |> 
  mutate(month = month(date),
         year = year(date), 
         year = as.factor(year)) |> 
  group_by(year, month) |> 
  summarise(total = sum(fatalities_count, na.rm = TRUE), .groups = "drop") 
  ggplot(aes(year, total)) +
  ggridges::geom_density_ridges()
  
class(df)
df$month <- as.factor(df$month)
df$total <- as.numeric(df$total)
ggplot(df, aes(month, total, group = month)) +
  geom_boxplot()
  
ggplot(df, aes(total, month, fill = stat(x))) +
  ggridges::geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_viridis_c(option = "C", direction = -1, name = "Fatalities") +
  scale_x_continuous(labels = label_number(scale_cut = cut_short_scale()))+
  labs(x = "Total number of Fatalites", 
       y = "Month of Year", 
       title = "Distribution of Total Number of Accident by Month") +
  ggridges::theme_ridges() +
  theme(
    plot.title = element_text(size = 32, hjust = .5, face = "bold")
  ) +
  guides(fill = "none")
ggsave("accident.pdf", dpi =360, width = 12, height = 9)  


