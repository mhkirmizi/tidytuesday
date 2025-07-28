library(tidyverse)
library(showtext)
library(ggrain)
## Fonts
font_add_google('Outfit', 'Outfit')
showtext_auto()

## Text
title <- "Education Score in Rural England"
subtitle <- "The presence of a university in a town does not influence the average education score. \nHowever, it significantly shapes the distribution of educational quality."
caption <- "Data:  The UK Office for National Statistics | Vis: MhKirmizi"
### Data 
english_education <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-01-23/english_education.csv')

english_education |>
  filter(!is.na(university_flag)) |> 
  ggplot(aes(x = university_flag, y = education_score, fill = university_flag)) +
  geom_rain(alpha = .8,
            boxplot.args = list(
              color = "black",
              outlier.shape = NA), 
            point.args = list(alpha = .4)) +
  labs(
    x = "",
    y = "Education score", 
    title = title,
    subtitle = subtitle,
    caption = caption,
    fill = "University"
    ) +
  theme_classic(base_family = 'Outfit') +
  theme(
    plot.title = element_text(size = 42, hjust = .5, face= "bold"), 
    plot.subtitle = element_text(size = 24, hjust = .5),
    plot.caption = element_text(size = 10),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "top"
  )
ggsave("education_england.png", width = 1920, height = 1080, units = "px", dpi = 132)




