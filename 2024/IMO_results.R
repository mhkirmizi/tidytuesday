library(tidyverse)
library(ggtext)
library(tidytuesdayR)
library(cowplot)

tuesdata <- tidytuesdayR::tt_load('2024-09-24')
tuesdata$country_results_df -> country

country_data <- country |> 
  select(country, awards_gold, awards_silver, awards_bronze) |> 
  mutate(country = case_when(
    country == "Union of Soviet Socialist Republics" ~ "Russia",
    country == "Russian Federation" ~ "Russia",
    country == "German Democratic Republic" ~ "Germany",
    country == "People's Republic of China" ~ "China",
    country == "United States of America" ~ "USA", 
    country == "Islamic Republic of Iran" ~ "Iran", 
    country == "United Kingdom" ~ "UK",
    country == "Republic of Korea" ~ "Korea",
    TRUE ~ country
  )) |> 
  group_by(country) |> 
  summarise(gold = sum(awards_gold), 
            silver = sum(awards_silver), 
            bronze = sum(awards_bronze)) |> 
  arrange(-gold) |> head(15) |> 
  pivot_longer(2:4)

country_data$name <- factor(country_data$name, levels = c("gold", "silver", "bronze"))

colors  <- c(gold ="#d6af36", silver = "#d7d7d7", bronze = "#824a02" )

ggplot(country_data) +
  geom_col(aes(x = country, y = value, fill = name), position = "dodge") +
  scale_fill_manual(values = colors) +
  labs(
    title = "Top 15 performers of IMO",
    subtitle = "IMO results highlight an intriguing balance between power and tradition: <br> superpowers often vie for <span style='color:#d6af36'><strong>gold</strong></span>, <br> while smaller nations with a rich tradition in mathematics, such as Hungary, aim for <span style='color:#d7d7d7'><strong>silver</strong></span> and <span style='color:#824a02'><strong>bronze</strong></span>.",
    caption = "Source: IMO · Graphic: MhKirmizi"
  ) +
  theme_minimal_hgrid() +
  theme(
    legend.position = "none", 
    plot.title = element_markdown(hjust = 0.5, face = "bold", size = 28 ), 
    plot.subtitle = element_markdown(hjust = 0.5, size = 14, lineheight = 1.4), 
    plot.caption = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(), 
    plot.background = element_rect(fill = "grey28")
  )
  
