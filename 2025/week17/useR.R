library(tidyverse)
library(treemapify)
library(dlookr)
user2025 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-29/user2025.csv')
dlookr::diagnose(user2025)
df <- user2025 |> 
  group_by(session) |> 
  summarise(n = n())
ggplot(df, aes(area = n, label = session)) +
  treemapify::geom_treemap(fill = "#0A7575", 
                          color = "black", 
                          size = 1.5) +
  treemapify::geom_treemap_text(fontface = "bold",
                                color = "white", 
                                place = "centre"
                                ) +
  labs(title = "useR! 2025 ",
       caption = "Posit PBC | Vis: MhKirmizi")+
  theme(
    plot.title = element_text(size = 32, face = "bold",
                              hjust = .5, color = "white"),
    plot.caption = element_text(size = 12, color = "white", face = "bold"),
    legend.position = "none", 
    plot.background = element_rect(fill = "black"), 
    panel.background = element_rect(fill = "black"), 
    plot.margin = margin(40, 30, 10, 30)
  )
ggsave("useR.pdf", dpi = 300, width = 12, height = 9)
