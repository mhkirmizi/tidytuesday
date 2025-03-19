library(tidyverse)
library(dlookr)
library(ggpubr)
library(outliers)
palmtrees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-18/palmtrees.csv')

title <- "Average Palm Fruit Length (cm) Distribution by Subfamily"
caption <- "Data:{palmtrees} package | Vis: Mhkirmizi"
## Rrmoving the outliers
palmtrees$average_fruit_length_cm <- dlookr::imputate_outlier(palmtrees, average_fruit_length_cm, method = "mean")
dlookr::plot_outlier(palmtrees)
df <- palmtrees |> 
  drop_na(average_fruit_length_cm) |> filter(palm_subfamily != "Nypoideae")

ggviolin(df, x = "palm_subfamily", y = "average_fruit_length_cm", 
         fill = "palm_subfamily",
         add = "boxplot", add.params = list(fill = "white"))+
  stat_compare_means(label.y = 50) + 
  scale_y_continuous(limits = c(0, 7.5)) +
  labs(x = "", 
       y = "Average Fruit Length", 
       title =title ,
       caption = caption, 
       fill = "Palm Subfamily") +
  theme(
    plot.title = element_text(size = 32, face = "bold", hjust = .5), 
    caption = element_text(size =12)
  )
  
  ggsave("palmtrees.pdf", dpi = 360, height = 9, width = 12)
  
  
  
  