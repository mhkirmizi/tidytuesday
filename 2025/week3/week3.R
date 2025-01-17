library(tidyverse)
library(hrbrthemes)
library(ggforce)
library(ggtext)
library(showtext)

font_add_google("Roboto", "roboto")
showtext_auto()
## Data
exped_tidy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-21/exped_tidy.csv')

## Text
title = "EXPEDITIONS to the Mt. EVEREST"
subtitle = "The data reveals an interesting pattern in Everest expeditions. Spring stands out as the most successful season for reaching the summit. \nOut of 462 expeditions, 355 were successful, resulting in an impressive success rate of nearly 77%.
Fall is another popular season for attempting the climb. \nAmong the 394 expeditions during this period, 262 reached the peak, yielding a success rate of approximately 67%."
caption = "Data: The Himalayan Database | Vis: MhKirmizi "
### Data Wrangling 
exped_tidy |> 
  mutate(reached = if_else(TERMREASON == 1, 1, 0)) |> 
  group_by(SEASON) |> 
  summarise(
    total = n(),
    successes = sum(reached),
    success_rate = successes / total
  )

exped_tidy |> 
  drop_na(c(BCDATE, TERMDATE)) |> 
  mutate(
    succes = if_else(TERMREASON == 1, TRUE, FALSE), 
    duration = if_else(succes == TRUE, TOTDAYS, -TOTDAYS), 
    SEASON = case_when(SEASON == 1 ~ 1, 
                       SEASON == 3 ~ 2, 
                       SEASON == 2  ~ 3, 
                       SEASON == 4 ~ 3), 
    link_color = if_else(succes, as.factor(SEASON), "Failure")) |> 
  arrange(SEASON) |> 
  mutate(EXPID = factor(EXPID, levels = unique(EXPID))) |> 
  ggplot(aes(duration, as.numeric(EXPID))) +
  geom_link(aes(
    x = 0, xend = duration, 
    y = as.numeric(EXPID), yend = as.numeric(EXPID), 
    color = link_color,
    alpha = abs(duration) 
  ), n = 300, size = 0.5) +
  geom_point(aes(x = duration, fill = as.factor(SEASON), color = link_color))+ 
  scale_x_continuous(limits =  c(-80, 80)) +
  scale_color_manual(
    values = c("1" = "#A73933FF", "2" = "#B5A052FF", "3" = "#F07266FF", "Failure" = "grey66"), 
    name = "Season",
    labels = c("1" = "Spring", "2" = "Fall", "3" = "Summer & Winter")) +
  guides(color = guide_legend(order = 1), 
         fill = "none",
         alpha = "none") +
   labs(y = "", 
        title = title, 
        subtitle = subtitle, 
        caption = caption) +
  theme_ipsum_gs() +
  theme(
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    plot.title = element_text(size = 26, face = "bold", hjust = .5), 
    plot.subtitle = element_text(size = 18, hjust = .5),
    legend.position = "bottom", 
    
  )
ggsave("week2.pdf", dpi = 360, width = 12, height = 9)  
