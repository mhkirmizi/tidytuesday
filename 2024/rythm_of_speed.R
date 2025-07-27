library(tidyverse)
library(dlookr)
library(geomtextpath)
library(showtext)
library(ggtext)
### Fonts
font_add_google("Cabin")
showtext_auto()
### Data Wrangling
df <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-03/A64_traffic.csv')
diagnose(df)
dlookr::plot_correlate(df)
unique(df$SiteId)
### Plotting
df |> 
  mutate(time_interval = `Time Interval` + 1) |>
  rename(avg_mpg = `Avg mph`) |> 
  ggplot(aes(time_interval, avg_mpg, color = as.factor(SiteId))) +
  geom_point(alpha = .2) +
  geom_labelsmooth(aes(label = SiteId), text_smoothing = 30, fill = "#F6F6FF",
                   size = 4, linewidth = 1.5, boxlinewidth = 0.3) +
  scale_x_continuous(breaks = c(25, 50, 75, 100),
                     labels = c('Morning', 'Noon', 'After Noon', 'Midnight')) +
  scale_y_continuous(labels = function(x) paste0(x, 'MPG')) +
  scale_color_manual(values = c('#226060FF','#639CA4FF', '#D2AD7CFF', '#BE7245FF')) +
  facet_wrap(~SiteId) +
  labs(x = '', 
       y = '', 
       title = "The Daily Rhythm of Speed: A Four-Site Perspective", 
       caption = "Data: WebTRIS Traffic Flow API | Created by MhKirmizi" ) +
  cowplot::theme_minimal_hgrid() +
  theme(
    text = element_text(size = 14, family = 'Cabin'),
    panel.grid.major.y = element_line(linetype = 'dashed'), 
    legend.position = 'none', 
    plot.title = element_markdown(hjust = .5,
                                  halign = .5,
                                  size = 36, 
                                  color = '#1C3333FF'),
    strip.text = element_blank(), 
    plot.margin = margin(t = 20, r = 30, b = 20, l = 30), 
    plot.background = element_rect(color = 'white', fill = 'white'), 
    panel.background = element_rect(color = 'white', fill = 'white')
  )
ggsave("rythm_of_speed.png", width = 1920, height = 1080, units = "px", dpi = 132)
