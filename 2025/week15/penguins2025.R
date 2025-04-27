library(tidyverse)
library(ggpubr)
df <- palmerpenguins::penguins
## Text
title = "Palmer Penguins"
comparison <- list(c("Adelie", "Chinstrap"), c("Gentoo", "Chinstrap"), c("Adelie", "Gentoo"))
### The violoing plot
ggviolin(df, x = "species", 
         y = "flipper_length_mm", 
         fill = "species", 
         palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
         add = "boxplot", add.params = list(fill = "white")) +
  stat_compare_means(comparisons =comparison, label = "p.signif")  +
  stat_compare_means(label.y = 255) +
  labs(x = "Species", 
       y = "Flipper Length (mm)", 
       fill = "Species", 
       title = title, 
       caption = "Data: {palmerpenguins} | Vis: MhKirmizi") +
  theme_pubclean() +
  theme(
    plot.title = element_text(size = 24, hjust = .5, face = "bold")
  )
ggsave("palmerpenguins.png", dpi = 300, width = 12, height = 9)

