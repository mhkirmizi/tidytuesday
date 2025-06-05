library(tidyverse)
library(ggtext)
library(showtext)
library(tidytuesdayR)
library(cowplot)
### Fonts
font_add_google("Eczar", "Eczar")
showtext_auto()
### Colors
colors <- c(gold ="#e2c764", silver = colorspace::darken("#d7d7d7", amount = 0.15), bronze = "#9a5a05" )

### Data 
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

##Plotting
ggplot(country_data) +
  geom_col(aes(x = reorder(country, -value, sum), 
               y = value, fill = name), position = "stack") +
  scale_fill_manual(values = colors) +
  labs(
    x = '', 
    y = 'Total number of medal',
    title = "Top 15 performers of IMO",
    subtitle = "IMO results highlight an intriguing balance between power and tradition: superpowers often vie for <span style='color:#e2c764'><strong>gold</strong></span>, <br> while smaller nations with a rich tradition in mathematics, such as Hungary, aim for <span style='color:#B7B7B7'><strong>silver</strong></span> and <span style='color:#9a5a05'><strong>bronze</strong></span>.",
    caption = "Source: IMO · Graphic: MhKirmizi"
  ) +
  theme_classic(base_family = "Eczar", base_size = 14) +
  theme(
    legend.position = "none", 
    plot.title = element_markdown(hjust = 0.5, face = "bold", 
                                  size = 45, family = "Eczar"), 
    plot.subtitle = element_markdown(hjust = 0.5, size = 26, lineheight = 1.4), 
    plot.background = element_rect(fill = "grey90", colour = 'grey90'), 
    panel.background = element_rect(fill = "grey90", colour = 'grey90')
  )
ggsave("IMO_results.png", width = 1920, height = 1080, units = "px", dpi = 132)  
