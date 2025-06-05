library(tidyverse)
library(dlookr)
library(showtext)
### Fonts
font_add_google("Cinzel Decorative", "dnd")
showtext_auto()
###Colors
hue <- c("Good" = "#FD994F",  "Neutral" ="#82889B", "Evil" = "#E87EA1")
bg_color <- colorspace::darken("#101c1c", amount = .3)
text_color <- colorspace::lighten("#3d4e25", amount = .3)

monsters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-27/monsters.csv')
diagnose(monsters)
df <- monsters |> 
  select(alignment, str, dex, con, int, wis, cha) |> 
  mutate(alignment = 
           ifelse(alignment == "Neutral", "Neutral Neutral", alignment)) |> 
  filter(alignment != "Unaligned") |> 
  separate_wider_delim(alignment, names = c("Law", "Good"), delim = " ") |> 
  mutate(
    Law = factor(Law, levels = c("Lawful", "Neutral", "Chaotic")), 
    Good = factor(Good, levels = c("Good", "Neutral", "Evil"))
  ) |> 
  group_by(Law, Good) |> 
  summarise(
    n = n(), 
    str = mean(str), 
    dex = mean(dex), 
    con = mean(con),
    int = mean(int), 
    wis = mean(wis), 
    cha = mean(cha)
  ) |> 
  ungroup() |> 
  pivot_longer(-c(Law, Good, n)) |> 
  mutate(
    name = factor(name, levels = c(
      "str", "dex", "con", "int", "wis", "cha"
    ))
  )
df <- df |> 
  mutate(group_number = ceiling((row_number())/6))
  

## Plotting with area chart
ggplot(df, aes(name, value, group =group_number)) +
  geom_area(aes(fill = Good, alpha = .4)) +
  scale_fill_manual(values = hue) +
  labs(
    x = "", 
    y = "", 
    title = "Dragons & Dungeons", 
    caption = "Data: DD Sytems Reference | Vis: MhKirmizi"
  ) +
  facet_wrap(~Law + Good) +
  theme_minimal(base_family = "dnd" ) +
  theme(
        legend.position = "none",
        panel.background = element_rect(fill = bg_color, 
                                        colour = bg_color),
        plot.background = element_rect(fill = bg_color, 
                                       colour = bg_color),
        plot.title = element_text(size = 42, face = "bold", color = text_color,
                                  family = "Cinzel Decorative", hjust = .5),
        plot.caption = element_text(size = 14, colour = text_color, face = "bold"),
        strip.text = element_text(size = 18, face = "bold", color = text_color),
        axis.line.x = element_line(colour = text_color),
        axis.line.y = element_line(colour = bg_color),
        axis.text.x = element_text(colour = text_color,
                                   size = 18),
        axis.text.y = element_text(colour = text_color,
                                   size = 16),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = .1, linetype = "dashed"),
        panel.grid.minor.y = element_blank(),
        plot.margin = margin(10, 0, 5, 5, "mm"))
ggsave("DD.png", width = 1920, height = 1080, units = "px", dpi = 132)
