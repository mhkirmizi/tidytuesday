library(tidyverse)
library(ggdensity)
### Fonts
## Fonts
font_add_google("Montserrat", "Montserrat")
showtext_auto()

### colors
penguins_hue <- c("#121510FF", "#6D8325FF","#D6CFB7FF","#E5AD4FFF", "#BD5630FF", "#798E87")
    

## 
df <- palmerpenguins::penguins
ggplot(df, aes(flipper_length_mm,
               bill_length_mm, 
               fill = species)) +
  geom_hdr(xlim = c(160, 240), ylim = c(30, 70)) +
  geom_point(shape = 21, aes(size = body_mass_g)) +
  labs(x = 'Flipper Length', 
       y = 'Bill Length', 
       title = 'Palmer Penguins', 
       caption = "Data: Palmer Penguins | Vis: MhKirmizi")+
  coord_flip() +
  scale_fill_manual(values = penguins_hue[4:6]) +
  scale_color_manual(values = penguins_hue[4:6]) +
  theme_classic(base_family = "Montserrat") +
  theme(
    legend.position = 'none', 
    plot.background = element_rect(fill = penguins_hue[1], color = penguins_hue[1]), 
    panel.background = element_rect(fill = penguins_hue[1], color = penguins_hue[1]), 
    plot.title = element_text(size = 36, 
                              face = "bold", 
                              family = "Montserrat", 
                              colour = penguins_hue[3],
                              hjust = 0.5), 
    panel.grid = element_line(color = penguins_hue[3]),
    axis.ticks = element_blank(), 
    axis.text = element_text(size =12, 
                             family = "Montserrat", 
                             colour = penguins_hue[3]), 
    axis.title = element_text(size =12, 
                              family = "Montserrat", 
                              colour = penguins_hue[3]), 
    axis.line = element_line(colour = penguins_hue[3]), 
    plot.caption = element_text(size = 12, color = penguins_hue[3])
  )
ggsave("penguins_dist.png", dpi = 360, width = 12, height = 9)
ggsave("penguins_dist.pdf", dpi = 360, width = 16.5, height =11.7)  
