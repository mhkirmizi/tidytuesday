library(tidyverse)
library(dlookr)
library(ggridges)
library(sysfonts)
## Fonts
font_add_google("Roboto", "roboto")
showtext_auto()

palmtrees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-18/palmtrees.csv')
diagnose_outlier(palmtrees)
  
palmtrees |> 
  filter(palm_subfamily != "Nypoideae") |> 
  drop_na(fruit_size_categorical, average_fruit_length_cm) |>
  ggplot(aes(average_fruit_length_cm, palm_subfamily)) +
  geom_density_ridges(aes(rel_min_height = .01, fill = palm_subfamily)) +
  scale_x_continuous(limits = c(0, 12.5)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  labs(x = "Average Fruit Length (cm)", 
       y = "Palm Subfamily", 
       fill = "Palm Subfamily", 
       title = "Palm Subfamily: Distribution of Average Fruit Length", 
       caption = "Data: {palmtrees} | Vis: MhKirmizi") +
  theme_ridges() +
  theme(
    legend.position = "top", 
    legend.justification = c(0.5, 1), 
    plot.title = element_text(size = 36, hjust = .5, family = "Roboto")
  )
ggsave("palmtrees2.pdf", dpi = 360, width = 12, height = 9)


