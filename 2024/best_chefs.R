library(tidyverse)
library(treemapify)
library(skimr)
library(pillar)

restaurant_and_chef <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-31/restaurant_and_chef.csv')
skim(restaurant_and_chef)
pillar::glimpse(restaurant_and_chef)

restaurant_and_chef$subcategory <- as.factor(restaurant_and_chef$subcategory)
restaurant_and_chef$rank <- as.factor(restaurant_and_chef$rank)
restaurant_and_chef$name <- as.factor(restaurant_and_chef$name)


data <- restaurant_and_chef |> 
  filter(!is.na(restaurant) | is.na(city)) |> 
  separate(city, into = c("city", "state"), sep = ",") |> 
  mutate(extracted_city = str_extract(restaurant, "^[^,]+"), 
         extracted_state = sub(".*,\\s*", "", restaurant),
         city = if_else(is.na(city), extracted_city, city),
         state = if_else(is.na(state), extracted_state, state)) |> 
  select(-c("extracted_city", "extracted_state")) |> 
  drop_na() |> 
  rename(region = state)
data2 <- data |> 
  filter(subcategory != "Who's Who of Food & Beverage in America") |> 
  filter(subcategory == "Best Chefs") |> 
  group_by(region, subcategory) |> 
  summarise(n = n()) |> 
  filter(n>= 49)

p <- ggplot(data2, aes(area = n, fill = region, label = region)) +
  geom_treemap(layout = "scol") +
  geom_treemap_text(fontface = "bold", 
                    colour = "black", 
                    place = "centre", 
                    grow = TRUE
                    )  +
  scale_fill_viridis_d(option = "D") +
  labs(
    title = "Best Chefs of USA", 
    subtitle = "New York, California and Illinois\n are three leaders of producing the best chefs in the USA",
    caption = "Data: James Beard Awards. Visualization: MhKirmizi"
    
  ) +
  theme(
   # plot.background = element_rect(fill = "grey66"),
    legend.position = "none", 
    plot.title = element_text(size = 36, hjust = .5, face = "bold"), 
    plot.subtitle = element_text(size = 18, hjust = .5, face = "italic"), 
    plot.caption = element_text(size = 12, hjust = 1)
  )
ggsave("best_chefs.pdf", p, width = 12, height = 8)
