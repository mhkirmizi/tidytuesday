library(tidyverse)
library(ggrain)
library(showtext)
## fonts
font_add_google('Outfit', 'Outfit')
font_add("Rubik Dirt", 
         "D:\\VIS\\Rubik_Dirt\\RubikDirt-Regular.ttf")
showtext_auto()
### Text
subtitle <- "There is an ongoing legal battle over lead contamination in the drinking water of Flint, Michigan. Shown below are the distributions of three \nversions of the water-quality data. Two datasets come from official government sources, while the third was collected independently by Flint residents."
### Data & Wrangling
flint_mdeq <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-04/flint_mdeq.csv')
flint_vt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-04/flint_vt.csv')
flint_mdeq <- flint_mdeq |> 
  rename(
    'Official' = lead, 
    'Official After Removal' = lead2
  )
flint_vt <- flint_vt |> 
  rename(
    'Citizen' = lead
  )
df <- flint_vt |> 
  left_join(flint_mdeq, by = 'sample' )
df_long <- df |> 
  pivot_longer(cols = -c('sample', 'notes'), names_to = 'lead', values_to = 'values') 

  
ggplot(df_long, aes(x = lead, y = values, color = lead, fill = lead)) +
  geom_rain(alpha = .7) +
  geom_hline(yintercept = 15, linetype = 'dashed', color = 'grey26') +
  coord_flip() +
  scale_fill_manual(values = c('Official After Removal' = "#55555FFF", 
                               'Official' = "#32363FFF", "Citizen" = "#141824FF")) +
  scale_color_manual(values = c('Official After Removal' = "#55555FFF", 
                                'Official' = "#32363FFF", "Citizen" = "#141824FF")) +
  labs(y = 'Lead in water per particle in billion (ppb)', 
       x = 'Data Source', 
       title = "Flint Water Crisis: Lead Contamination in Water", 
       subtitle = subtitle,
       caption = "Loux & Giblon, 2018 | Vis: MhKirmizi") +
  annotate("text", x = 2.75 , y = 36, 
           label = "Safe Level of Lead\n in Water (0.0015mg/L or 15ppb)\nAccording to EPA", vjust = 1, color= "grey26") +
  geom_curve(
    aes(x = 2.4 , y = 28, xend = 2.3, yend = 16),
    arrow = arrow(length = unit(0.2, "in")),
    curvature = 0.3,
    color = "gray66"
  ) +
  cowplot::theme_minimal_hgrid() +
  theme(
    text = element_text(size= 14, family = 'Outfit'),
    legend.position = 'none', 
    plot.title = element_text(size = 48, 
                              family = 'Rubik Dirt', 
                              face = 'bold', 
                              hjust = .5, 
                              color = "#000000FF"), 
    plot.subtitle = element_text(size = 18, 
                                 hjust = .5),
    plot.background = element_rect(fill = "white", color = "white"), 
    panel.background = element_rect(fill = "white", color = "white")
  )
ggsave("flint.png", width = 1920, height = 1080, units = "px", dpi = 132)  


